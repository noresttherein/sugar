package net.noresttherein.sugar.vars


import java.util.concurrent.{ConcurrentSkipListMap, Executor}

import scala.annotation.unspecialized
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.VolatileLike.{BoolVolatileLike, RefVolatileLike}
import net.noresttherein.sugar.vars.Watched.SerialExecutor
import net.noresttherein.sugar.witness.{DefaultValue, Optionally}




/** A [[net.noresttherein.sugar.vars.Volatile Volatile]]-like variable which additionally executes callback actions
  * each time its value is changed. This execution happens by default synchronously, in the thread which mutated
  * the variable, before the setter method returns. Any Java [[java.util.concurrent.Executor Executor]]
  * can be however provided as an implicit parameter at the time the variable is created.
  *
  * There are two collections of watchers associated with every instance: regular, presumably asynchronous watchers,
  * and synchronous watchers, registered with methods [[net.noresttherein.sugar.vars.Watched.watch watch]]
  * and [[net.noresttherein.sugar.vars.Watched.inSync inSync]], respectively. When and how the former are executed
  * in respect to the assignment call and each other is unspecified and depends solely on the provided `Executor`
  * implementation. While the default [[net.noresttherein.sugar.vars.Watched.SerialExecutor SerialExecutor]]
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
  * @define ref watched variable
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
sealed class Watched[@specialized(SpecializedVars) T] private[vars] (implicit executor :Executor = SerialExecutor)
	extends VolatileLike[T] with Mutable[T] with Serializable
{
	@scala.volatile private[this] var x :T = _
	private[this] val watchers             = new ConcurrentSkipListMap[Any, T => Unit].asScala
	private[this] val synchronousWatchers  = new ConcurrentSkipListMap[Any, T => Unit].asScala

	private def set(value :T) :Unit = x = value

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
		if (executor eq SerialExecutor) { //ignore the executor and manually inline the callbacks
			def schedule(key :Any, callback: T => Unit): Unit =
				try {
					callback(currentValue)
				} catch {
					case e: Exception if exception == null => exception = e
					case e: Exception => exception.addSuppressed(e)
				}

			watchers.foreachEntry(schedule)
			synchronousWatchers.foreachEntry(schedule)
		} else {
			watchers.foreachEntry { (_, callback) =>
				try {
					executor.execute { () => callback(currentValue) }
				} catch {
					case e: Exception if exception == null => exception = e
					case e: Exception => exception.addSuppressed(e)
				}
			}

			if (synchronousWatchers.nonEmpty) {          //there may be a race condition with the set becoming empty,
				val triggerThread = Thread.currentThread //but it does not affect correctness
				@volatile var goAhead = true //start flag for all threads
				@volatile var awaiting = 0 //number of not completed tasks
				try { //register all callbacks in the executor and wait until they all complete.
					synchronousWatchers.foreachEntry { (_, callback) =>
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
										awaiting -= 1
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
	  * of execution (or even scheduling of the execution) of the function.
	  * @param key      a value identifying the callback, which can be used to unregister it
	  *                 using [[net.noresttherein.sugar.vars.Watched.resign resign]].
	  * @param callback a function run by this $ref's executor each time the value of this variable changes.
	  */
	@unspecialized def watch(key :Any, callback :T => Unit) :this.type = {
		watchers(key) = callback ; this
	}

	/** Registers a callback function executed every time this variable is modified. The function will be given
	  * as the argument the value of the variable which triggered the notification, rather than its value at the point
	  * of execution (or even scheduling of the execution) of the function.
	  * The function object is used as its own identifier, i.e. this call is equivalent to
	  * [[net.noresttherein.sugar.vars.Watched.watch(key:Any* watch]]`(callback, callback)`,
	  * and passing it to [[net.noresttherein.sugar.vars.Watched.resign resign]] will unregister it.
	  * @param callback a function run by this $ref's executor each time the value of this variable changes.
	  */
	@unspecialized def watch(callback :T => Unit) :this.type = watch(callback, callback)

	/** Registers a callback function executed serially every time this variable is modified.
	  * The function will be given as the argument the value of the variable which triggered the notification,
	  * rather than its value at the point of execution (or even scheduling of the execution) of the function.
	  * It will be run by the thread which performed the mutation, and the assignment call will not return
	  * until all serial watchers have been executed.
	  * @param key      a value identifying the callback, which can be used to unregister it
	  *                 using [[net.noresttherein.sugar.vars.Watched.resign resign]].
	  * @param callback a function run by this $ref's executor each time the value of this variable changes.
	  */
	@unspecialized def inSync(key :Any, callback :T => Unit) :this.type = {
		synchronousWatchers(key) = callback; this
	}

	/** Registers a callback function executed serially every time this variable is modified.
	  * until all serial watchers have been executed. The function will ''not'' synchronize on the variable's monitor,
	  * and concurrent modifications are possible. It will be given as the argument the value of the variable
	  * which triggered the notification, rather than its value at the point of execution
	  * (or even scheduling of the execution) of the function.
	  * It will be run by the thread which performed the mutation, and the assignment call will not return
	  * The function object is used as its own identifier, i.e. this call is equivalent to
	  * [[net.noresttherein.sugar.vars.Watched.inSync(key:Any* inSync]]`(callback, callback)`,
	  * and passing it to [[net.noresttherein.sugar.vars.Watched.resign resign]] will unregister it.
	  * @param callback a function run by this $ref's executor each time the value of this variable changes.
	  */
	@unspecialized def inSync(callback :T => Unit) :this.type = inSync(callback, callback)

	/** Removes the callback identified by the given key from the list of watchers of this variable.
	  * The key is compared using its `equals` method with keys given as first arguments
	  * to [[net.noresttherein.sugar.vars.Watched.watch watch]] and [[net.noresttherein.sugar.vars.Watched.inSync inSync]].
	  */
	@unspecialized def resign(key :Any) :this.type = synchronized {
		watchers -= key
		synchronousWatchers -= key
		this
	}
	/** Removes the given callback from the list of watchers of this variable.
	  * The callback is compared using its `equals` method, which, for compiler created function implementations,
	  * is the default referential equality (`eq`), requiring the exact same instance as was given
	  * to [[net.noresttherein.sugar.vars.Watched.watch watch]].
	  */
	@unspecialized def resign(callback :T => Unit) :this.type = resign(callback :Any)

	override def mkString :String = mkString("Watched")

	override def toString :String = "Watched(" + value + ")@" + this.identityHashCodeString
}




/** A factory of boxed `@volatile` variables which evaluate passed callbacks every time a value is changed.
  * @define variable watched variable
  */
@SerialVersionUID(Ver)
object Watched extends VolatileLikeCompanion[Watched] {

	/** Create a new $variable which can be shared and watched by multiple threads. The implicit
	  * [[java.util.concurrent.Executor Executor]] will be used to execute the registered callbacks.
	  * If no implicit value exists, [[net.noresttherein.sugar.vars.Watched.SerialExecutor SynchronousExecutor]]
	  * will be used instead.
	  */
	def apply[@specialized(SpecializedVars) T](init :T)(implicit executor :Optionally[Executor]) :Watched[T] =
		new Watched[T] match {
			case any if any.getClass == CaseUnspec =>
				val res :Watched[T] = new Ref[T]()(executor.opt getOrElse SerialExecutor)
				res.set(init)
				res
			case bool if bool.getClass == CaseBool =>
				val res :Watched[Boolean] = new Bool()(executor.opt getOrElse SerialExecutor)
				res.set(init.asInstanceOf[Boolean])
				res.asInstanceOf[Watched[T]]
			case res =>
				res.set(init)
				res
		}

	/** Create a new $variable which can be shared and watched by multiple threads, initialized with the default
	  * value for this type (zero, `false` or `null`). The implicit [[java.util.concurrent.Executor Executor]]
	  * will be used to execute the registered callbacks. If no implicit value exists,
	  * [[net.noresttherein.sugar.vars.Watched.SerialExecutor SerialExecutor]] will be used instead.
	  */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T], executor :Optionally[Executor]) :Watched[T] =
		apply(default.get)(executor)




	/** The simplest executor which executes the given [[Runnable]] immediately in the body of its `execute` method. */
	@SerialVersionUID(Ver)
	final object SerialExecutor extends Executor with Serializable { //todo: exception handling and logging.
		override def execute(command :Runnable) :Unit = command.run()
	}

	//these are only used to access classes of each specialized implementation, we care not about executors
	protected override def newInstance[@specialized(SpecializedVars) T](init :T) :Watched[T] = {
		val res = new Watched[T]
		res.set(init)
		res
	}
	protected override def newRefInstance[T](init :T) :Watched[T] = {
		val res :Watched[T] = new Ref[T]
		res.set(init)
		res
	}
	protected override def newBoolInstance(init :Boolean) :Watched[Boolean] = {
		val res :Watched[Boolean] = new Bool
		res.set(init)
		res
	}

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
	@SerialVersionUID(Ver)
	private class Ref[T](implicit executor :Executor = SerialExecutor) extends Watched[T] with RefVolatileLike[T]

	/** Optimised implementation of `Watched[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	@SerialVersionUID(Ver)
	private class Bool(implicit executor :Executor = SerialExecutor) extends Watched[Boolean] with BoolVolatileLike

}
