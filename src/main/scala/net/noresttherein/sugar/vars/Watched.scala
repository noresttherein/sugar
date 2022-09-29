package net.noresttherein.sugar.vars


import java.util.concurrent.{ConcurrentSkipListSet, Executor}
import scala.annotation.unspecialized
import scala.jdk.CollectionConverters.SetHasAsScala
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.VolatileLike.{BoolVolatileLike, RefVolatileLike}
import net.noresttherein.sugar.vars.Watched.SerializedExecutor
import net.noresttherein.sugar.witness.{DefaultValue, Maybe}

import scala.collection.mutable




/** A [[net.noresttherein.sugar.vars.Volatile Volatile]]-like variable which additionally executes callback actions
  * each time its value is changed. This execution happens by default synchronously, in the thread which mutated
  * the variable, before the setter method returns. Any Java [[java.util.concurrent.Executor Executor]]
  * can be however provided as an implicit parameter at the time the variable is created.
  *
  * There are two collections of watchers associated with every instance: regular, presumably asynchronous watchers,
  * and synchronous watchers, registered with methods [[net.noresttherein.sugar.vars.Watched.watch watch]]
  * and [[net.noresttherein.sugar.vars.Watched.inSync inSync]], respectively. When and how the former are executed
  * in respect to the assignment call and each other is unspecified and depends solely on the provided `Executor`
  * implementation. While the default [[net.noresttherein.sugar.vars.Watched.SerializedExecutor SerializedExecutor]]
  * executes the callbacks in the thread which set the value of this variable, most 'real' implementations will
  * have it happen asynchronously with the setter call. The latter watchers are executed synchronously: regardless
  * of the `Executor` implementation, the mutating call to this variable will return only after all synchronous
  * watchers are executed, or any one of them throws a [[Throwable]] which is not an [[Exception]],
  * in which case the throwable is rethrown. Unless the executor in use is single threaded however, these callbacks
  * will be executed concurrently with each other.
  *
  * This variable kind is somewhat similar in application to [[net.noresttherein.sugar.vars.SignalVar SignalVar]]
  * in that both offer means for the application to react to changes in multi-threaded context. The difference
  * is that this variable is a rather straightforward implementation of the ''observer'' pattern, in which
  * the updating thread is the active side, at least scheduling the execution of callbacks, and possibly executing them
  * itself; on the other hand, `SignalVar` allows interested threads to synchronously wait on the variable
  * until a change to it is made and react themselves. The communication is asynchronous in both cases.
  *
  * @see [[net.noresttherein.sugar.vars.Watched.value_=]]
	* @define Ref `Watched`
  * @author Marcin Mościcki
  */
@SerialVersionUID(ver)
sealed class Watched[@specialized(SpecializedVars) T](init :T)(implicit executor :Executor = SerializedExecutor)
	extends VolatileLike[T] with Mutable[T] with Serializable
{
	@scala.volatile private[this] var x = init
	private[this] val watchers = new ConcurrentSkipListSet[T => Unit].asScala
	private[this] val synchronousWatchers = new ConcurrentSkipListSet[T => Unit].asScala

	protected override def factory :Watched.type = Watched

	final override def value :T = x

	/** Assigns a new value to this variable and ensures all registered watchers are executed.
	  * The concurrency details of the execution are defined by the [[java.util.concurrent.Executor Executor]]
	  * implementation given as the argument to this instance's constructor: it can be any manner of asynchronous
	  * execution employing separate threads, or it can happen within the calling thread.
	  * All exceptions - but not `Throwable` - thrown by `Executor.`[[java.util.concurrent.Executor.execute execute]]
	  * is caught, and the scheduling of tasks for every remaining watcher continues. If any exceptions where thrown,
	  * then the first one is rethrown, with the rest added as [[Throwable.addSuppressed suppressed]] exceptions.
	  * The watchers will be executed with `newValue` as the argument
	  * (rather than `this.`[[net.noresttherein.sugar.vars.Watched.value value]] at the time the watcher is called.
	  * No updates will be lost by any listener, unless a `Throwable` is thrown by the scheduling method or JVM
	  * exits abruptly for any other reason. However, the order in which the callbacks are made is unspecified:
	  * it is possible that not only the value of this variable does not match the argument of the callback
	  * at the moment of its execution, but that callbacks for two separate updates in a ''happens-before'' relation
	  * with each other will happen in the reverse order.
	  */
	final override def value_=(newValue :T) :Unit = {
		x = newValue
		trigger(newValue)
	}

	private def trigger(currentValue :T) :Unit = {
		var exception: Exception = null //the first exception caught in this method, following are added as suppressed
		if (executor eq SerializedExecutor) { //ignore the executor and manually inline the callbacks
			def schedule(callback: T => Unit): Unit =
				try {
					callback(currentValue)
				} catch {
					case e: Exception if exception == null => exception = e
					case e: Exception => exception.addSuppressed(e)
				}

			watchers.foreach(schedule)
			synchronousWatchers.foreach(schedule)
		} else {
			watchers.foreach { callback =>
				try {
					executor.execute { () => callback(currentValue) }
				} catch {
					case e: Exception if exception == null => exception = e
					case e: Exception => exception.addSuppressed(e)
				}
			}

			if (synchronousWatchers.nonEmpty) { //there may be a race condition with the set becoming empty,
				val triggerThread = Thread.currentThread //but it does not affect correctness
				@volatile var goAhead = true //start flag for all threads
				@volatile var awaiting = 0 //number of not completed tasks
				try { //register all callbacks in the executor and wait until they all complete.
					synchronousWatchers.foreach { callback =>
						awaiting += 1
						try {
							executor.execute { () =>
								if (Thread.currentThread == triggerThread) {
									try { //the executor is some in-the-calling thread implementation
										if (goAhead)
											callback(currentValue)
									} catch {
										case e: Exception if exception == null => exception = e
										case e: Exception => exception.addSuppressed(e)
										case e: Throwable if exception != null =>
											e.addSuppressed(exception)
											throw e
									} finally {
										awaiting -= 1
									}
								} else try { //asynchronous executions
									if (goAhead)
										callback(currentValue)
								} finally { //guarantee that either awaiting == 0 or there will be a notifyAll() call
									synchronized {
										awaiting -= 1;
										if (awaiting == 0) //no sense in waking up the main thread
											notifyAll()
									}
								}
							}
						} catch { //executor.execute error
							case e: Exception if exception == null => exception = e
							case e: Exception => exception.addSuppressed(e)
						}
					}
				} catch { //unexpected errors in foreach itself as well as errors (not exceptions) in executor.execute
					case e: Throwable => //stop scheduling and throw the error
						if (exception != null)
							e.addSuppressed(exception)
						goAhead = false
						throw e
				}
				//				goAhead := true
				if (awaiting > 0) //all increases of awaiting *happen-before* this check
					synchronized {
						while (awaiting > 0) wait()
					}
			}
		}
		if (exception != null)
			throw exception
	}

	/** Registers a callback function executed every time this variable is modified. The function will be given
	  * as the argument the value of the variable which triggered the notification, rather than its value at the point
	  * of execution (or even scheduling of the execution) of the function. This method synchronizes
	  * on this object's monitor.
	  */
	@unspecialized def watch(callback :T => Unit) :this.type = {
		watchers += callback; this
	}

	@unspecialized def inSync(callback :T => Unit) :this.type = {
//		if (synchronousWatchers == null)
//			synchronousWatchers = new ConcurrentSkipListSet[T => Unit].asScala
		synchronousWatchers += callback; this
	}

	//consider: a better unregistering schema
	/** Removes the given callback from the list of watchers of this variable.
	  * The callback is compared using its `equals` method, which, for compiler created function implementations,
	  * is the default referential equality (`eq`), requiring the exact same instance as was given
	  * to [[net.noresttherein.sugar.vars.Watched.watch watch]].
	  */
	@unspecialized def resign(callback :T => Unit) :this.type = synchronized {
		watchers -= callback
		synchronousWatchers -= callback
		this
	}

	override def mkString :String = mkString("Watched")

	override def toString :String = "Watched(" + value + ")@" + this.identityHashCodeString
}




/** A factory of boxed `@volatile` variables which evaluate passed callbacks every time a value is changed.
  * @define variable watched variable
  */
@SerialVersionUID(ver)
object Watched extends VolatileLikeCompanion[Watched] {

	/** Create a new $variable which can be shared and watched by multiple threads. The implicit
	  * [[java.util.concurrent.Executor Executor]] will be used to execute the registered callbacks.
	  * If no implicit value exists, [[net.noresttherein.sugar.vars.Watched.SerializedExecutor SynchronousExecutor]]
	  * will be used instead.
	  */
	def apply[@specialized(SpecializedVars) T](init :T)(implicit executor :Maybe[Executor]) :Watched[T] =
		new Watched[T](init) match {
			case any if any.getClass == CaseUnspec =>
				new WatchedRef[T](init)(executor.opt getOrElse SerializedExecutor)
			case bool if bool.getClass == CaseBool =>
				new WatchedBool(init.asInstanceOf[Boolean])(executor.opt getOrElse SerializedExecutor).asInstanceOf[Watched[T]]
			case res => res
		}

	/** Create a new $variable which can be shared and watched by multiple threads, initialized with the default
	  * value for this type (zero, `false` or `null`). The implicit [[java.util.concurrent.Executor Executor]]
	  * will be used to execute the registered callbacks. If no implicit value exists,
	  * [[net.noresttherein.sugar.vars.Watched.SerializedExecutor SerializedExecutor]] will be used instead.
	  */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T], executor :Maybe[Executor]) :Watched[T] =
		apply(default.get)(executor)




	/** The simplest executor which executes the given [[Runnable]] immediately in the body of its `execute` method. */
	@SerialVersionUID(ver)
	final object SerializedExecutor extends Executor with Serializable { //todo: exception handling and logging.
		override def execute(command :Runnable) :Unit = command.run()
	}

	//these are only used to access classes of each specialized implementation, we care not about executors
	protected override def newInstance[@specialized(SpecializedVars) T](init :T) :Watched[T] = new Watched(init)
	protected override def newRefInstance[T](init :T) :Watched[T] = new WatchedRef[T](init)
	protected override def newBoolInstance(init :Boolean) :Watched[Boolean] = new WatchedBool(init)

	private[vars] override def getAndSet[@specialized(SpecializedVars) T](v: InOut[T], newValue: T) :T = {
		val res = super.getAndSet(v, newValue)
		v.asInstanceOf[Watched[T]].trigger(newValue)
		res
	}
	private[vars] override def testAndSet[@specialized(SpecializedVars) T](v: InOut[T], expect: T, newValue: T) :Boolean =
		super.testAndSet(v, expect, newValue) && { v.asInstanceOf[Watched[T]].trigger(newValue); true }

	private[vars] override def weakTestAndSet[@specialized(SpecializedVars) T](v: InOut[T], expect: T, newValue: T) :Boolean =
		super.weakTestAndSet(v, expect, newValue) && { v.asInstanceOf[Watched[T]].trigger(newValue); true }

	private[vars] override def repeatBoolTestAndSet(bool: InOut[Boolean], expect: Boolean, ifExpected: Boolean, ifNotExpected: Boolean) = {
		val res = super.repeatBoolTestAndSet(bool, expect, ifExpected, ifNotExpected)
		val old = res == ifNotExpected ^ expect
		bool.asInstanceOf[Watched[Boolean]].trigger(old)
		res
	}


	/** An unspecialized `Watched` implementation overriding atomic mutator methods to compare the value
	  * using `eq`/`ne`, rather than `==`/`!=` as in `Volatile` (which would call `equals` on reference types,
	  * which we do not want).
	  */
	@SerialVersionUID(ver)
	private class WatchedRef[T](init :T)(implicit executor :Executor = SerializedExecutor)
		extends Watched[T](init) with RefVolatileLike[T]

	/** Optimised implementation of `Watched[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	@SerialVersionUID(ver)
	private class WatchedBool(init :Boolean)(implicit executor :Executor = SerializedExecutor)
		extends Watched[Boolean](init) with BoolVolatileLike

}
