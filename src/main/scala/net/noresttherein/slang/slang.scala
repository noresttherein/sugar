package net.noresttherein

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable


/**
  * @see [[net.noresttherein.slang.slangImports]]
  * @author Marcin Mościcki
  */
package object slang extends slangImports {

	trait ScalaThrowable extends Throwable with Cloneable {
		/** Standard [[Throwable.getSuppressed getSuppressed]] array as a scala [[Seq]]. */
		def suppressed :Seq[Throwable] = ArraySeq.unsafeWrapArray(getSuppressed)

		/** Reverses the exception stack, where `_.getCause` is treated as the next element on the list.
		  * The first exception of the returned list is the original cause (one without a cause), while this exception
		  * closes the list.
		  */
		lazy val causeQueue :Seq[Throwable] = {
			val dejaVu = mutable.Set[Throwable]()
			@tailrec def push(e :Throwable, result :List[Throwable]) :Seq[Throwable] = e.getCause match {
				case null => e::result
				case cause if dejaVu(cause) =>
					val cycle = new BaseException(s"Cycle detected in the exception stack: ${e.getCause} already present.")
					cycle::e::result
				case cause => dejaVu += e; push(cause, e::result)
			}
			push(this, Nil)
		}

		/** Standard [[Throwable.getCause getCause]] wrapped in an [[Option]]. */
		def cause :Option[Throwable] = Option(getCause)

		/** Denullified [[Throwable.getMessage getMessage]] returning an empty string instead of `null` if no message
		  * was provided. */
		val message :String = if (getMessage == null) "" else getMessage
	}

	trait ScalaException extends Exception with ScalaThrowable

	trait StackableThrowable extends ScalaThrowable {
		/** A 'virtual constructor' returning an exception of the same type as this one, with the given message,
		  * and this exception as its cause. This is useful when we want to preserve both the original exception type
		  * and the initial exception as the reason, in case someone higher on the call stack would want to handle it,
		  * but also to add additional contextual information about the failed operation, such as original parameters.
		  * This is different than [[java.lang.Throwable.addSuppressed addSuppressed]] in that the latter implies
		  * another error (though possibly related) which happened when processing this exception, while this method
		  * is used in catch-rethrow scenarios. Also, the latter accepts a new exception instance from the caller,
		  * who may pass any type of the exception but typically does not know the precise type of this
		  * exception.
		  */
		def pushMessage(msg :String) :ScalaThrowable
	}


	class StackableException(msg :String, cause :Throwable = null)
		extends Exception(msg, cause) with StackableThrowable
	{
		override def pushMessage(msg :String) :ScalaThrowable = raise
	}
	class BaseException(override val message :String, e :Throwable = null)
		extends Exception(message, e) with ScalaException


	/** An exception thrown by code which should be impossible to execute.
	  * It is different from [[AssertionError]] in that the latter is always placed in reachable code
	  * and checks for situations which, while should not occur, are half-expected as possible due to programming errors.
	  * This error is always thrown unconditionally, especially in places where a compiler expects an expression which,
	  * to the best of the programmer's knowledge, will never be executed. Examples include:
	  *   - Code following infinite loops;
	  *   - Guard match patterns following a pattern list which is presumed to already cover all matched values;
	  *   - Body of sealed class's methods which must have a concrete implementation (because they override
	  *     an existing method), but which also must be overriden by all subclasses - for example
	  *     in order to narrow down the return type;
	  *   - Never called package-protected obsolete methods remaining for binary compatibility;
	  *
	  * and similar.
	  * @see [[net.noresttherein.slang.slangImports.??!]]
	  * @see [[net.noresttherein.slang.slangImports.impossible_!]]
	  */
	class ImpossibleError(msg :String, reason :Throwable) extends Error(msg, reason) with ScalaThrowable {
		def this(msg :String) = this(msg, null)

		def this() = this("Implementation error", null)
	}


	private[slang] def publishMutable() :Unit = java.lang.invoke.VarHandle.releaseFence()

}
