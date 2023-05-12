package net.noresttherein.sugar.exceptions

import scala.collection.immutable.ArraySeq

import net.noresttherein.sugar.exceptions
import net.noresttherein.sugar.exceptions.extensions.ThrowableExtension




/** A scope defining an implicit conversion enriching any `Throwable` with some extension methods. */
trait extensions extends Any {
	/** Extension methods for [[Throwable]], mainly Scala-style accessors to standard properties. */
	@inline implicit final def ThrowableExtension(self :Throwable) :ThrowableExtension = new ThrowableExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	/** Scala-like accessors to properties of [[Throwable]]. */
	class ThrowableExtension(private val self :Throwable) extends AnyVal {
		def stackTrace :Seq[StackTraceElement] = self match {
			case e :SugaredThrowable => e.stackTrace
			case _ => ArraySeq.unsafeWrapArray(self.getStackTrace)
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
			case _ => exceptions.causeQueue(self)
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


		/**`Option(getLocaliazedMessage)`. */
		def localizedMessage :Option[String] = self match {
			case e :SugaredThrowable => e.localizedMessage
			case _ => Option(self.getLocalizedMessage)
		}

		/**`Option(getLocaliazedMessage) getOrElse ""`. */
		def localizedMsg :String = self match {
			case e :SugaredThrowable => e.localizedMsg
			case _ if self.getLocalizedMessage == null => ""
			case _ => self.getLocalizedMessage
		}

		/** Traverses the exception stack defined by the standard [[Throwable.getCause Throwable.getCause]] method
		  * and returns the bottom exception. The result will never be `null`: if `this.getCause == null`
		  * then `this` is returned.
		  */
		def originalCause :Throwable = exceptions.originalCause(self)

		/** The message included in the bottom exception of the cause stack, that is the last exception,
		  * in the list with [[Throwable.getCause getCause]] as the next message.
		  * @return `Option(originalCause.getMessage)`
		  */
		def originalMessage :Option[String] = self match {
			case e :SugaredThrowable => e.originalMessage
			case _ => Option(originalCause.getMessage)
		}

		/** Denullified [[Throwable.getMessage getMessage]] of the original `Throwable` cause of this exception,
		  * returning an empty string instead of `null` if no message was provided.
		  * @return [[net.noresttherein.sugar.exceptions.SugaredThrowable.originalMessage originalMessage]]` getOrElse ""`.
		  */
		def originalMsg :String = self match {
			case e :SugaredThrowable => e.originalMsg
			case _ =>
				val msg = exceptions.originalCause(self).getMessage
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
		  */
		def addInfo(msg :String) :Throwable = pushErrorMessage(msg)(self)
	}

}
