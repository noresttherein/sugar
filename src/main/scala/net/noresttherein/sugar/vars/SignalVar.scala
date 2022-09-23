package net.noresttherein.sugar.vars

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.time.{Eternity, Immediate, Milliseconds, PosixTime, TimeInterval}
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.witness.DefaultValue




/** A thread safe [[net.noresttherein.sugar.vars.InOut InOut]] variable performing all access to its value
  * while holding its monitor. Very similar to [[net.noresttherein.sugar.vars.SyncVar SyncVar]],
  * but all updates are followed with a [[java.lang.Object.notifyAll notifyAll]]`()` call.
  * In addition to the standard API, this class defines a family of [[net.noresttherein.sugar.vars.SignalVar.await await]]
  * methods, which block until the next update. This allows any thread to synchronously 'watch' this variable
  * and react to every single update performed on it.
  *
  * As in `SyncVar`, all assignments happen sequentially and can block attempting to synchronize on this instance.
  * However, in order to guarantee that all threads waiting or an update will not miss it due to another update
  * happening before they have a chance to read the previous value, assignments can also block waiting on this instance
  * until all threads waiting in `await` and friends to see the ''current'' value return.
  *  Assigns `newValue` to this variable and notifies all threads waiting to see the next value of this variable.
  * The call is synchronized on this instance's monitor, and thus all assignments happen in a sequential/serialized
  * manner. It can block, both when acquiring the monitor (entering the `synchronized` block), as well as when
  * some threads waiting to see the ''current'' value have not yet had a chance to return it to their caller.
  * This guarantees that no assignment is lost (barring abnormal termination of the virtual machine)
  * Waiting threads are notified (using [[java.lang.Object.notifyAll notifyAll]]`()`) even if `this.value == newValue`.
  * While the application code can synchronize on this instance's monitor, waiting on this object is discouraged,
  * as these are not the only cases when `notifyAll` is called - the caller will have no means of determining
  * if an update has indeed happened and if the value they see is the one which replaced the last one they read.
  *
  * Calling code can synchronize manually with this monitor in order to expand the scope of the critical section
  * as needed.  Waiting on this object is however discouraged, as [[java.lang.Object.notifyAll notifyAll]]
  * is not used exclusively to wake threads awaiting the next assignment to this variable and,
  * without access to the internal state of this class, the caller has no means of determining if an update
  * has indeed happen and if the value they see is the one which replaced the last one they read.
  *
  * One significant difference from a similar [[net.noresttherein.sugar.vars.SignalVal SignalVal]] -
  * apart from the latter being possible to set only once - is that `SignalVar`'s value is specified at creation
  * (possibly as the default value for value types).
  * A `SignalVar` thus will never throw a [[NoSuchElementException]], but may also contain a value which
  * is not initialized from the business logic point of view.
  * @see [[net.noresttherein.sugar.vars.SignalVal]]
  * @see [[net.noresttherein.sugar.vars.Watched]]
  * @see [[net.noresttherein.sugar.vars.Relay]]
	* @define Ref `SignalVar`
  */
sealed class SignalVar[@specialized(SpecializedVars) T] private[vars] (private[this] var x :T)
	extends SyncVar[T]
{
	//invariant: writers >= 0 && (mutations >= 0 || waiters > 0).
	// If mutations < 0 then writers can only be decreased; if mutations >= 0 writers can only be increased
	/** The number of threads currently waiting in one of the `await` or `testAndAwait` methods to see the result of
	  * the `(mutations + 1)`-th assignment (if `mutations >= 0`), or `-mutations`-th one (if `mutations < 0`).
	  * See [[net.noresttherein.sugar.vars.SignalVar.mutations mutations]] for a detailed description
	  * of how the control passes between updating and watching threads.
	  */
	private[this] var waiters = 0   //number of threads waiting for the increase in mutations in one of await methods

	/** The total number of assignments to this variable in its whole life.
	  * At any time this instance is either in ''default mode'' (mutations >= 0) or ''notification mode''
	  * (`mutations < 0`). In both modes, `mutations.abs` is the total number of assignments performed so far.
	  *
	  * In ''default'' mode, a call of `await` results in the thread increasing field `waiters` by one and waiting
	  * on this instance until the next assignment changes the value of both `x` and `mutations`.
	  * Assignment attempts happening in ''default'' mode proceed immediately, increasing this field together
	  * with changing the value of this instance. Afterwards, if `waiters > 0` - waiting threads mentioned previously
	  * need to see the assigned value - the assigning thread negates `mutations`, making it `< 0`
	  * and notifies all threads waiting on this object (those mentioned previously).
	  * This object transitions to ''notification'' mode.
	  *
	  * In ''notification mode'', any call to both `value_=` and any of `await` methods immediately starts
	  * waiting on this instance's monitor until it transitions back to ''default'' mode - only then they can proceed
	  * in the manner described in the previous paragraph. The only threads allowed to change the state
	  * of this object are those which wait in `await` after having increased `waiters` as described.
	  * After acquiring the monitor, they decrease `waiters` by one before returning the `-mutations`-th value
	  * of this instance to the calling code. The last of these threads - the one which sets `waiters` to zero - negates
	  * `mutations` and notifies all waiting threads, both those wanting to update this instance as well as those
	  * waiting to start proper waiting for the next update in the manner described in the previous paragraph.
	  * This object subsequently transitions back to ''default'' mode.
	  */
	private[this] var mutations = 0

	private final def doneWaiting() :Unit = {
		waiters -= 1
		if (waiters == 0) {
			mutations = -mutations
			notifyAll()
		}
	}

	private[vars] override def unsync = x

	private[vars] override def unsync_=(newValue :T) :Unit = {
		while (mutations < 0)           //there ar threads still waiting to see the previous update
			wait()
		if (waiters > 0)                //prevent future updates until all waiters see the new value
			mutations = -mutations - 1
		else                            //no one is waiting for this update, proceed
			mutations += 1
		x = newValue
		notifyAll()
	}

	//how can I add scaladocs without overriding the methods?
//	/** The current value of this variable. The call is synchronized and thus can block,
//	  * but it does not otherwise wait for any condition.
//	  */ //repeated declaration for updated docs
//	def value :T
//
//	/** Assigns `newValue` to this variable and notifies all threads waiting to see the next value of this variable.
//	  * The call is synchronized on this instance's monitor, and thus all assignments happen in a sequential/serialized
//	  * manner. It can block, both when acquiring the monitor (entering the `synchronized` block), as well as when
//	  * some threads waiting to see the ''current'' value have not yet had a chance to return it to their caller.
//	  * This guarantees that no assignment is lost (barring abnormal termination of the virtual machine)
//	  * Waiting threads are notified (using [[java.lang.Object.notifyAll notifyAll]]`()`) even if `this.value == newValue`.
//	  * While the application code can synchronize on this instance's monitor, waiting on this object is discouraged,
//	  * as these are not the only cases when `notifyAll` is called - the caller will have no means of determining
//	  * if an update has indeed happened and if the value they see is the one which replaced the last one they read.
//	  */ //repeated declaration for updated docs
//	def value_=(newValue :T) :Unit

//	/** The current value of this variable. The call is synchronized and thus can block,
//	  * but it does not otherwise wait for any condition.
//	  */
//	@inline override def value :T = synchronized { x }
//
//	/** Assigns `newValue` to this variable and notifies all waiting threads. The calls is synchronized,
//	  * and thus all assignments happen in a sequential/serialized manner. The notification happens by
//	  * a call to [[java.lang.Object.notifyAll notifyAll]].
//	  */
//	@inline final override def value_=(newValue :T) :Unit = synchronized {
//		x = newValue; mutations += 1; notifyAll()
//	}
//
//	/** Atomically swaps the value of this variable, assigning to it `newValue` and returning the value immediately
//	  * preceding the assignment. All threads waiting for a change to happen on this object's monitor are notified.
//	  */
//	@inline final override def ?=(newValue :T) :T = synchronized {
//		val res = x; unsync = newValue; mutations += 1; notifyAll(); res
//	}
//
//	/** Assigns a new value to this variable providing the current value is equal to the expected value.
//	  * All threads waiting for a change to happen on this object's monitor are notified ''iff'' an assignment
//	  * was performed by this method.
//	  * @param expect   a value to compare with current value.
//	  * @param newValue a new value for this variable.
//	  * @return `true` if previous value equaled `expect` and the variable has been set to `newValue`.
//	  */
//	final override def testAndSet(expect :T, newValue :T) :Boolean = synchronized {
//		(x == expect) && { x = newValue; mutations += 1; notifyAll(); true }
//	}
//
//	/** Atomically updates the value of this variable with the given function. This is equivalent to
//	  * `this := f(this); this.get` with the guarantee that no other thread will modify the value of this variable
//	  * between the individual operations.
//	  * @param f a function to apply to the value of this variable.
//	  * @return the result of applying `f` to the current value.
//	  */
//	final override def apply(f :T => T) :T = synchronized {
//		val res = f(x); x = res; mutations += 1; notifyAll(); res
//	}
//
//	/** Combines the value of this variable with a value of some other type, atomicALLY assigning the result
//	  * of the application
//	  * back to this variable before returning it. It uses this variable as an accumulator, updated iteratively with
//	  * new values in a way similar to an in place ''foldLeft'' operation on a singleton collection; the difference
//	  * from `foldLeft` is that the function's result is the type of this variable, rather than the argument.
//	  * Default implementation naively performs this directly without any guarantees about multi-threaded semantics
//	  * and is equivalent to `val res = f(z, value); value = res; res`. This method comes
//	  * to use with concurrent `InOut` implementations such as [[net.noresttherein.sugar.vars.SyncVar SyncVar]]
//	  * or [[net.noresttherein.sugar.vars.Atomic Atomic]].
//	  * @param z accumulator value to pass as the first argument to the `f` function, together with the current
//	  *          value of this variable.
//	  * @param f a function applied to the argument and this variable, whose result should be set to this variable.
//	  * @return the result of applying `f` to this variable and the argument.
//	  */
//
//	final override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = synchronized {
//		val res = f(z, unsync); x = res; mutations += 1; notifyAll(); res
//	}
//
//	final override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = synchronized {
//		val res = f(unsync, z); x = res; mutations += 1; notifyAll(); res
//	}

	/** Waits for the value of this variable to change.
	  * The change here means any call to setters [[net.noresttherein.sugar.vars.SignalVar.value_= value]]` = _`,
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` _` as well as
	  * [[net.noresttherein.sugar.vars.SignalVar.testAndSet testAndSet]] or other atomic mutator methods.
	  * The values of the variable themselves are not compared for equality: it is possible to again assign
	  * an object being the current value, and this will also notify the caller. The waiting is atomic -
	  * in the following code, the sequence of `last` values is guaranteed to be consecutive natural numbers:
	  * {{{
	  *     val v = SignalVar[Int]
	  *     threadA =
	  *         while (v < Int.MaxValue)
	  *             v += 1
	  *     threadB = v.synchronized {
	  *         var last = v(); var next = 0
	  *         while (last < Int.MaxValue) {
	  *             next = v.await //releases the hold of the monitor of v
	  *             assert(next - last == 1)
	  *             last = next
	  *         }
	  *     }
	  * }}}
	  *
	  * @return the value directly replacing the current value of this variable (as of acquiring of this instance's
	  *         monitor at the beginning of the method).
	  */
	def await() :T = synchronized {
		while (mutations < 0) //wait until we can wait properly
			wait()
		waiters += 1
		val stamp = mutations
		try {
			while (stamp == mutations)
				wait()
		} finally { doneWaiting() }
		x
	}

	/** Waits the specified amount of time for the value of this variable to change.
	  * The change here means any call to setters [[net.noresttherein.sugar.vars.SignalVar.value_= value]]` = _`,
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` _` as well as
	  * [[net.noresttherein.sugar.vars.SignalVar.testAndSet testAndSet]] or other atomic mutator methods.
	  * The values of the variable themselves are not compared for equality: it is possible to again assign
	  * an object being the current value, and this will also notify the caller.
	  *
	  * @return the value directly replacing the current value of this variable (as of acquiring of this instance's
	  *         monitor at the beginning of the method).
	  */
	def await(timeout :Milliseconds) :T = synchronized {
		if (timeout <= Milliseconds.Zero)
			x
		else {
			waiters += 1
			val stamp = mutations
			try {
				var now = PosixTime.now
				val deadline = now + timeout
				while (mutations == stamp && now < deadline) {
					wait((now - deadline).toMillis, 0)
					now = PosixTime.now
				}
			} finally { doneWaiting() }
			x
		}
	}

	/** Waits the specified amount of time for the value of this variable to change.
	  * The change here means any call to setters [[net.noresttherein.sugar.vars.SignalVar.value_= value]]` = _`,
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` _` as well as
	  * [[net.noresttherein.sugar.vars.SignalVar.testAndSet testAndSet]] or other atomic mutator methods.
	  * The values of the variable themselves are not compared for equality: it is possible to assign
	  * the object being the current value, and this will also notify the caller.
	  * The method returns the value of this variable at the point of its completion.
	  * If the timeout is not positive, this will be the current value of this variable (after initial acquiring
	  * of its monitor). Otherwise, it will be the current value once the thread acquires the monitor for this instance
	  * after waking up due to timeout or an assignment performed by another thread. It is possible for this method both to
	  * not see an update (due to a timeout) or to 'lose' a change, that is return a value of some later assignment
	  * than the first one happening since the call of this method.
	  * @return The current value of this variable after the thread reclaims the monitor for this object.
	  *         It is possible for this method to 'lose' assignments, that is return a value which is
	  *         not the first assigned object since the call.
	  */
	def await(timeout :TimeInterval) :T = synchronized {
		if (timeout <= Immediate)
			x
		else if (timeout == Eternity)
			await()
		else {
			waiters += 1
			val stamp = mutations
			try {
				var now = PosixTime.now
				val deadline = now + timeout
				while (mutations == stamp && now < deadline) {
					wait((now - deadline).toMillis, 0)
					now = PosixTime.now
				}
			} finally { doneWaiting() }
			x
		}
	}

	/** Awaits until the value of this variable is changed.
	  * If the current value does not equal `expect`, then the method returns `false` immediately and the call
	  * does not block. Otherwise the thread starts waiting on this monitor until notified by another thread modifying
	  * this variable as in [[net.noresttherein.sugar.vars.SignalVar.await await]]. Unlike in the latter method,
	  * the change is understood here as an assignment to this variable, not an assignment of an unequal value.
	  *
	  * There is no guarantee that the value of this variable after the method returns will represent the first mutation
	  * since entering the method, that is some updates may go unnoticed by this thread due to a race condition.
	  * This includes both assignments of the same value (because of the waiting condition) as well as true updates
	  * with a different value (because another thread reassigns `expect` to this variable before this thread reacquires
	  * the its monitor). In other words, updating threads do not wait until all readers queued before they published
	  * their value exit this method (or its overloaded variants).
	  *
	  * @param expect the expected value which, if matching the current value of this variable, will cause the thread
	  *               to sleep awaiting its update to a different value.
	  *               It is ''not'' 'the value the thread is looking for'.
	  * @return the current value of this variable while the thread returns. It is equal to `expect` ''iff''
	  *         `expect != this.value` at the moment of acquiring the monitor for this instance after entering
	  *         this method.
	  */
	final def testAndAwait(expect :T) :T = synchronized {
		if (x == expect)
			await()
		x
	}

	/** Awaits until the value of this variable is different then the expected value.
	  * If the current value does not equal `expect`, then it is returned immediately and the call does not block.
	  * Otherwise the thread starts waiting on this monitor until notified by another thread modifying this variable
	  * or the specified timeout elapses.
	  *
	  * There is no guarantee that the value returned by this method will represent the first mutation
	  * since entering the method, that is some updates may go unnoticed by this thread due to a race condition.
	  * In other words, updating threads do not wait until all readers queued before they published their value
	  * exit this method (or its overloaded variants).
	  *
	  * @param expect  the expected value which, if matching the current value of this variable, will cause the thread
	  *                to sleep awaiting its update to a different value.
	  *                It is ''not'' 'the value the thread is looking for'.
	  * @param timeout the total amount of time the caller is willing to wait for the value to change.
	  * @return the value of this variable at the moment of exiting the method.
	  */
	final def testAndAwait(expect :T, timeout :Milliseconds) :T = synchronized {
		if (x != expect) x
		else await(timeout)
	}

	final def testAndAwait(expect :T, timeout :TimeInterval) :T = synchronized {
		if (x != expect) x
		else await(timeout)
	}

	override def toString :String = "SignalVar(" + value + ")@" + this.identityHashCodeString
}




/** A factory of synchronized variables whose updates signal any threads currently waiting for their change. */
object SignalVar {
	/** Creates a new wrapper over a synchronized `var`, allowing threads to wait for future changes.
	  * @param init the initial value of the variable.
	  */
	def apply[@specialized(SpecializedVars) T](init :T) :SignalVar[T] = new SignalVar[T](init)

	/** Creates a new wrapper over a synchronized `var`, allowing threads to wait for future changes.
	  * The initial value of the variable is defined by the implicit type class.
	  */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :SignalVar[T] =
		new SignalVar[T](default.get)
}
