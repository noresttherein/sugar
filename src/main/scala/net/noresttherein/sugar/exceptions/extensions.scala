package net.noresttherein.sugar.exceptions

import java.io.{PrintWriter, StringWriter}

import scala.collection.immutable.ArraySeq

import net.noresttherein.sugar.exceptions
import net.noresttherein.sugar.exceptions.extensions.{StackTraceElementExtension, ThrowableExtension}




/** A scope defining an implicit conversion enriching any `Throwable` with some extension methods. */
trait extensions extends Any {
	/** Extension methods for [[Throwable]], mainly Scala-style accessors to standard properties. */
	@inline implicit final def ThrowableExtension(self :Throwable) :ThrowableExtension = new ThrowableExtension(self)

	@inline implicit final def StackTraceElementExtension(self :StackTraceElement) :StackTraceElementExtension =
		new StackTraceElementExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	/** Scala-like accessors to properties of [[Throwable]]. */
	class ThrowableExtension(private val self :Throwable) extends AnyVal {
		/** A reusable immutable sequence wrapping the throwable's stack trace */
		def stackTrace :StackTrace = self match {
			case e :SugaredThrowable => e.stackTrace
			case _ => StackTrace(self)
		}

		/** Stack traces of this exception and all its causes, with the stack for each exception omitting the frames
		  * already listed by previous (wrapping exception). If an exception doesn't have a stack trace,
		  * it is represented by an empty sequence. The first element is the stack trace of this very exception,
		  * from the point of its creation to the initial execution. Each subsequent exception omits the longest
		  * suffix (initial call sequence) common with the previous exception on the list
		  * ''which has a non empty stack trace''. Note that, because stack trace is filled when the exception
		  * is created, the stack trace of an exception does not need to be an actual prefix of the exception's cause,
		  * and, in extreme cases, may not share with it any common frames at all.
		  */
		def joinedStackTrace :Seq[StackTrace] = self match {
			case e :SugaredThrowable => e.joinedStackTrace
			case _ => utils.joinedStackTrace(self)
		}

		/** Standard [[Throwable.getSuppressed getSuppressed]] array as a scala [[Seq]]. */
		def suppressed :Seq[Throwable] = self match {
			case e :SugaredThrowable => e.suppressed
			case _ => ArraySeq.unsafeWrapArray(self.getSuppressed)
		}

		/** Reverses the exception stack, where `_.getCause` is treated as the next element on the list.
		  * The first exception of the returned list is the original cause (one without a cause), while this exception
		  * closes the list.
		  */
		def causeQueue :Seq[Throwable] = self match {
			case e :SugaredThrowable => e.causeQueue
			case _ => utils.causeQueue(self)
		}

		/** Standard [[Throwable.getCause getCause]] wrapped in an [[net.noresttherein.sugar.vars.Option]]. */
		def cause :Option[Throwable] = self match {
			case e :SugaredThrowable => e.cause
			case _ => Option(self.getCause)
		}

		/** Sets the [[ThrowableExtension.cause cause]] of this [[Throwable]] using
		  * [[Throwable.initCause initCause]] method. This method can be called at most once, and only
		  * if a `Throwable` cause was not given as a constructor parameter of this `Throwable`.
		  */
		def cause_=(cause :Throwable) :Unit = self match {
			case e :SugaredThrowable => e.cause = cause
			case _ => self.initCause(cause)
		}


		/** Standard[[Throwable.getMessage getMessage]] wrapped in an [[Option]]. */
		def message :Option[String] = self match {
			case e :SugaredThrowable => e.message
			case _ => Option(self.getMessage)
		}

		/** Denullified [[Throwable.getMessage getMessage]] returning an empty string instead of `null` if no message
		  * was provided.
		  */
		def msg :String = self match {
			case e :SugaredThrowable => e.msg
			case _ if self.getMessage == null => ""
			case _ => self.getMessage
		}


		/**`Option(getLocalizedMessage)`. */
		def localizedMessage :Option[String] = self match {
			case e :SugaredThrowable => e.localizedMessage
			case _ => Option(self.getLocalizedMessage)
		}

		/**`Option(getLocalizedMessage) getOrElse ""`. */
		def localizedMsg :String = self match {
			case e :SugaredThrowable => e.localizedMsg
			case _ if self.getLocalizedMessage == null => ""
			case _ => self.getLocalizedMessage
		}

		/** Traverses the exception stack defined by the standard [[Throwable.getCause Throwable.getCause]] method
		  * and returns the bottom exception. The result will never be `null`: if `this.getCause == null`
		  * then `this` is returned.
		  */
		def rootCause :Throwable = utils.rootCause(self)

		/** The message included in the bottom exception of the cause stack, that is the last exception,
		  * in the list with [[Throwable.getCause getCause]] as the next message.
		  * @return `Option(originalCause.getMessage)`
		  */
		def rootMessage :Option[String] = self match {
			case e :SugaredThrowable => e.rootMessage
			case _ => Option(rootCause.getMessage)
		}

		/** Denullified [[Throwable.getMessage getMessage]] of the original `Throwable` cause of this exception,
		  * returning an empty string instead of `null` if no message was provided.
		  * @return [[net.noresttherein.sugar.exceptions.SugaredThrowable.rootMessage originalMessage]]` getOrElse ""`.
		  */
		def rootMsg :String = self match {
			case e :SugaredThrowable => e.rootMsg
			case _ =>
				val msg = utils.rootCause(self).getMessage
				if (msg == null) "" else msg
		}


		/** Attempts to create a `Throwable` of the same type as this instance, with `msg` as its message
		  *  and `this` as its cause.
		  *   1. If this is a [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]] instance, it delegates
		  *      to its [[net.noresttherein.sugar.exceptions.SugaredThrowable.addInfo addInfo]] method.
		  *   1. Otherwise, if this is one of the standard, common Scala/Java exceptions, a new instance of its class
		  *      is created explicitly.
		  *   1. Otherwise, reflection is used to find a `(String, Throwable)` or a `(String)` constructor
		  *      in the class of this `Throwable`, which is then invoked through reflection.
		  *   1. If the above fails, the cause of the failure, containing `msg` as a part of its message,
		  *      is added as a [[Throwable.addSuppressed suppressed]] exception to this instance,
		  *      and the method returns `this`.
		  */ //todo: always use addSuppressed(RethrowContext())
		def addInfo(msg :String) :Throwable = pushErrorMessage(msg)(self)

		/** Formats the whole stack trace of this exception as a `String`
		  * in the same way as [[Throwable.printStackTrace printStackTrace]].
		  */
		def stackTraceString :String = self match {
			case sugared :SugaredThrowable => sugared.stackTraceString
			case _ => utils.stackTraceString(self)
		}

		/** Formats the stack trace of this exception and all its causes, listed in the reverse order.
		  * If this exception does not have a cause, this is equal to
		  * [[net.noresttherein.sugar.exceptions.extensions.ThrowableExtension.stackTraceString stackTraceString]]
		  * (and [[Throwable.printStackTrace printStackTrace]]). Otherwise, the root cause of this exception
		  * has its full stack trace formatted first, followed by stack traces of wrapping exceptions, omitting
		  * shared frames, with this exception being formatted as the last one.
		  */
		def reverseStackTraceString :String = self match {
			case sugared :SugaredThrowable => sugared.reverseStackTraceString
			case _ => utils.reverseStackTraceString(self)
		}
//
//		/** Formats this exception together with its stack trace in the standard format.
//		  * The first line of the returned `String` is equal to `this.toString`; the following lines
//		  * are the same as would be printed with [[Throwable.printStackTrace Throwable.printStackTrace]].
//		  */
//		def toStringWithStackTrace :String = utils.formatWithStackTrace(self)
	}




	class StackTraceElementExtension private[extensions] (private val frame :StackTraceElement) extends AnyVal {
		@inline def matches(pattern :StackTraceElement) :Boolean =
			frame.getClassName == pattern.getClassName && frame.getMethodName == pattern.getMethodName &&
				(frame.getLineNumber < 0 || pattern.getLineNumber < 0 || frame.getLineNumber == pattern.getLineNumber) &&
				(frame.getFileName == null || pattern.getFileName == null || frame.getFileName == pattern.getFileName)
	}
}
