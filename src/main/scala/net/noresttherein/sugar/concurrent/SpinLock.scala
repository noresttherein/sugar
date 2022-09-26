package net.noresttherein.sugar.concurrent

import java.lang.invoke.MethodHandles

import net.noresttherein.sugar.concurrent.SpinLock.{ownerField, Open, SpinsBetweenChecks}
import net.noresttherein.sugar.exceptions.StackableException
import net.noresttherein.sugar.time.{Milliseconds, PosixTime}




/** A simple spin lock implementation based around a `@volatile` flag.
  * A `SpinLock` can be ''locked/closed'' or ''unlocked/open''. Entering a critical section requires
  * locking of the Lock with one of [[net.noresttherein.sugar.concurrent.SpinLock.lock lock]] methods;
  * leaving it involves unlocking with [[net.noresttherein.sugar.concurrent.SpinLock.unlock unlock]]
  * or [[net.noresttherein.sugar.concurrent.SpinLock.close close]]. Once locked, all threads attempting to lock it
  * will actively wait 'spinning' in a loop (possibly executing
  * [[java.lang.Thread Thread.]]`.`[[java.lang.Thread.sleep sleep]] between checks, if so instructed) until
  * the lock becomes open. The lock should be unlocked only by the thread which locked it, immediately upon completion
  * of the critical section. For advanced synchronization techniques, it is still possible to open the lock
  * by other threads using [[net.noresttherein.sugar.concurrent.SpinLock.forceOpen forceOpen]] instead of `unlock`.
  *
  * Additionally, any thread at any time can [[net.noresttherein.sugar.concurrent.SpinLock.break break]] the lock:
  * this will cause all future attempts to lock or unlock it to fail
  * with a [[net.noresttherein.sugar.concurrent.BrokenLockException BrokenLockException]], and any threads
  * currently executing one of the `lock` methods likewise exit it abruptly with the said exception.
  *
  * The class is obviously type safe, but optimised for minimal overhead in low contention scenarios and, especially,
  * for very brief critical sections, such as setting two variables in tandem. It should not be used for a heavily
  * demanded resource, or when resource usage time is on a level of magnitude approaching the execution time
  * of the entirety of the synchronised code.
  * @author Marcin Mościcki
  */
class SpinLock private (initiallyOpen :Boolean) extends AutoCloseable {
	def this() = this(true)
	@volatile private[this] var owner = if (initiallyOpen) 0L else Thread.currentThread.threadId

//	/** An open lock can be locked by any thread without waiting. This value however can become stale the moment
//	  * it is returned.
//	  */ //can't see any use for this
//	def isOpen :Boolean = owner == 0L

	/** Checks if this lock is owned by the currently executed thread. In general, once the thread acquires
	  * the lock it belongs to that thread until it releases it, and no other thread can acquire it.
	  * However, a lock can be ''broken'' by any thread in order to signal all collaborating threads that the guarded
	  * resource is unavailable or that they should terminate their work. This affects all threads which currently
	  * attempt to acquire the lock as well as any such future attempts, but it remains the responsibility
	  * of the application thread owning the lock to poll this value.
	  * There is also a loophole for advanced use in method
	  * [[net.noresttherein.sugar.concurrent.SpinLock.forceOpen forceOpen]] which evicts any thread from the lock
	  * and allows its future locking by any thread in the result.
	  */
	def isAcquired :Boolean = owner == Thread.currentThread.threadId

	/** A broken lock is held by no thread and cannot be locked (or opened) again.
	  * This feature allows to signal all awaiting or using threads that they should terminate
	  * whatever they are doing. It is however the matter of application code to poll this value
	  * in reasonable intervals to verify if it should terminate. This should not however be needed,
	  * as the lock should be held only for ''very'' brief amounts of time.
	  */
	def isBroken :Boolean = owner < 0L

	/** Attempts to lock this lock, burning CPU cycles until some other thread releases it (unless currently open).
	  * If the lock is, or becomes [[net.noresttherein.sugar.concurrent.SpinLock.break broken]], the method
	  * terminates with a [[BrokenLockException]]. When (or if) the method successfully returns, the current thread
	  * holds the lock, preventing any other thread from acquiring it, and must call
	  * [[net.noresttherein.sugar.concurrent.SpinLock.unlock unlock]]`()` as soon as possible.
	  */
	@throws[BrokenLockException]("if this lock is, or becomes broken when this thread attempts to lock it.")
	def lock() :Unit = {
		val me = Thread.currentThread.threadId
		while (!ownerField.weakCompareAndSet(this, 0L, me))
			checkNotBroken()
	}

	/** Attempts to lock this lock, burning CPU cycles until some other thread releases it (unless currently open).
	  * It is similar to parameterless [[net.noresttherein.sugar.concurrent.SpinLock.lock lock()]]`()`,
	  * but the method will not wait indefinitely and will return once the defined time has elapsed.
	  * @return `true` if the thread is now the owner of the lock and `false` if the locking attempt
	  *         has failed due to a timeout.
	  */
	@throws[BrokenLockException]("if this lock is, or becomes broken when this thread attempts to lock it.")
	def lock(timeout :Milliseconds) :Boolean = {
		val me = Thread.currentThread.threadId
		val deadline = PosixTime after timeout
		var locked = false
		while (PosixTime.now < deadline && owner >= 0L) {
			var spinsLeft = SpinsBetweenChecks
			while (spinsLeft > 0) {
				while ({ locked = ownerField.weakCompareAndSet(this, 0L, me); !locked }) {
					checkNotBroken()
					spinsLeft -= 1
				}
			}
		}
		if (!locked)
			checkNotBroken()
		locked
	}

	/** Attempts to lock this lock, awaiting until some other thread releases it (unless currently open).
	  * It is similar to parameterless [[net.noresttherein.sugar.concurrent.SpinLock.lock lock()]]`()`,
	  * but the method will not wait indefinitely and will return once the defined time has elapsed.
	  * Additionally, between each locking attempt, the thread will sleep the amount of time specified by
	  * `sleep` argument, instead of waiting in a loop burning CPU cycles.
	  *
	  * This class was not however designed with a thought of long lasting critical sections or highly contended
	  * resources. In such scenarios this class may perform very poorly and heavier synchronisation techniques
	  * will be a much better solution. The method exists to alleviate cases where encountered load (temporarily)
	  * exceeds the one expected by the programmer and when there is no particular urgency for acquiring the lock.
	  *
	  * Note that [[Thread]]`.`[[Thread.sleep sleep]] does not release any held JVM object monitors.
	  * @param timeout maximum amount of time the thread should spend (actively or sleeping)
	  *                attempting to lock this instance.
	  * @param sleep   the amount of time that the current thread will sleep after each failed locking attempt.
	  * @return `true` if the thread is now the owner of the lock and `false` if the locking attempt
	  *         has failed due to a timeout.
	  */
	@throws[BrokenLockException]("if this lock is, or becomes broken when this thread attempts to lock it.")
	def lock(timeout :Milliseconds, sleep :Milliseconds) :Boolean = {
		val deadline = PosixTime after timeout
		var locked = false
		while (PosixTime.now < deadline && { locked = ownerField.weakCompareAndSet(this, true, false); !locked }) {
			checkNotBroken()
			Thread.sleep(sleep.toMillis)
		}
		locked
	}

	/** Opens the lock. Only the thread which currently holds the lock can unlock it.
	  * @see [[net.noresttherein.sugar.concurrent.SpinLock.close]]
	  */
	@throws[BrokenLockException]("if this lock is broken.")
	@throws[IllegalStateException]("if the current thread doesn't hold this lock.")
	def unlock() :Unit = {
		val me = Thread.currentThread.threadId
		if (!ownerField.compareAndSet(this, me, 0L)) {
			checkNotBroken()
			throw new IllegalStateException("Thread #" + me + "doesn't hold " + this + ".")
		}
		checkNotBroken()
	}

	/** An [[AutoCloseable]] hookup which releases the lock. Rather unfortunately named, as it actually
	  * ''opens'' (unlocks) the lock. Similar to [[net.noresttherein.sugar.concurrent.SpinLock.unlock unlock]]`()`,
	  * but is an idempotent operation and any thread can call successfully call this method if the lock is currently
	  * open (not locked).
	  */
	override def close() :Unit = {
		val me = Thread.currentThread.threadId
		if (!ownerField.compareAndSet(this, me, Open) && owner != Open)
			checkNotBroken()
	}

	/** Unlocks this lock, regardless of what thread was holding it. This doesn't 'fix' a broken lock. */
	@throws[BrokenLockException]("if this lock is, or becomes broken when this thread attempts to lock it.")
	def forceOpen() :Unit = {
		var current = owner
		if (current < 0L)
			throw new BrokenLockException(this)
		while (current > 0L && !ownerField.weakCompareAndSet(this, current, 0L)) {
			current = owner
			if (current < 0L)
				throw new BrokenLockException(this)
		}
	}

	/** Breaks the lock, entering a state in which can no longer be locked, but neither is open.
	  * All threads currently, or in the future, trying to lock this instance will exit the `lock` method
	  * by throwing a [[net.noresttherein.sugar.concurrent.BrokenLockException BrokenLockException]].
	  * All calls to [[net.noresttherein.sugar.concurrent.SpinLock.unlock unlock]]
	  * or [[net.noresttherein.sugar.concurrent.SpinLock.forceOpen forceOpen]] will also end with the same exception
	  * being thrown.
	  */
	def break() :Unit = owner = -1L


	private def checkNotBroken() :Unit =
		if (owner < 0L)
			throw new BrokenLockException(this)

	override def toString :String = {
		val current = owner
		if (current < 0L) "SpinLock(broken)@" + hashCode.toHexString
		else if (current == 0L) "SpinLock(open)@" + hashCode.toHexString
		else "SpinLock(#" + current + ")@" + hashCode.toHexString
	}
}




object SpinLock {
	/** Creates a new, open lock. */
	def apply() = new SpinLock(true)

	/** Creates a new lock and immediately locks it for the current thread. */
	def acquireNew = new SpinLock(false)

	private final val Open = 0L
	private final val Broken = -1L

	private val ownerField =
		MethodHandles.lookup.findVarHandle(classOf[SpinLock], "owner", java.lang.Long.TYPE)
	private final val SpinsBetweenChecks = 1000
}



/** An exception thrown when a [[net.noresttherein.sugar.concurrent.SpinLock SpinLock]]
  * becomes [[net.noresttherein.sugar.concurrent.SpinLock.break broken]].
  */
class BrokenLockException(msg :String, cause :Throwable = null) extends StackableException(msg, cause) {
	def this(lock :SpinLock) = this(lock.toString + " is broken.")
	override def addInfo(msg :String) :StackableException = new BrokenLockException(msg, this)
}
