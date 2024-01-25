package net.noresttherein.sugar

import scala.annotation.nowarn




package exceptions {

	import java.io.IOException

	private[exceptions] trait markerStackTraceElements {
		/* WHENEVER YOU EDIT THIS FILE UPDATE evalStackTraceElement WITH A CORRECT LINE NUMBER! */

		private[exceptions] def eval[T](action: => T) :T = action

		/** This method is never called. It is used as an artificial top stack trace element
		  * of cheaper [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] exceptions created and thrown
		  * by method [[net.noresttherein.sugar.imports.rethrow rethrow]], solely to provide this information
		  * here. These exceptions do not have their stack trace filled by the virtual machine, but initialize it instead
		  * with frames of the [[Throwable]] caught by `rethrow`, leading up to the call of `rethrow` itself.
		  * In order to minimize confusion coming from a stack trace leading to code which throws no exception,
		  * a final frame for this method is added to point programmers to this documentation.
		  */ //remember to update the line number if you edit these docs!
		@nowarn private def conjureThrowable :Nothing = ??!

		private[exceptions] final val evalStackTraceElement =
			new StackTraceElement(classOf[markerStackTraceElements].getName, "eval", "exceptions.scala", 12)

		private[exceptions] final val conjureThrowableStackTraceElement =
			new StackTraceElement(classOf[markerStackTraceElements].getName, "conjureThrowable", "exceptions.scala", 22)

//		private[exceptions] final val fillInStackTraceStackTraceElement =
//			new StackTraceElement(classOf[Rethrowable].getName, "fillInStackTrace", "SugaredThrowable.scala", 326)

	}

	object aliases {
		type CCE   = ClassCastException
		type IAE   = IllegalArgumentException
		type IOE   = IOException
		type IOOBE = IndexOutOfBoundsException
		type ISE   = IllegalStateException
		type NFE   = NumberFormatException
		type NPE   = NullPointerException
		type NSEE  = NoSuchElementException
		type UOE   = UnsupportedOperationException
	}
}




package object exceptions extends exceptions.imports with exceptions.markerStackTraceElements {
	private[exceptions] final val Ver = 1L

	type EagerExceptionFactory            = EagerThrowableFactory[Exception]
	type LazyExceptionFactory             = LazyThrowableFactory[Exception]
	type FlexibleExceptionFactory         = FlexibleThrowableFactory[Exception]
	type EagerDetailedExceptionFactory    = EagerDetailedThrowableFactory[Exception]
	type LazyDetailedExceptionFactory     = LazyDetailedThrowableFactory[Exception]
	type FlexibleDetailedExceptionFactory = FlexibleDetailedThrowableFactory[Exception]

	val Oops            :FlexibleThrowableFactory[Oops]            = ThrowableFactory("Oops", new Oops(_, _, _))
	val ImpossibleError :FlexibleThrowableFactory[ImpossibleError] = ThrowableFactory(new ImpossibleError(_, _, _))

	val IncompatibleArgumentsException :LazyExceptionFactory =
		ThrowableFactory(new IncompatibleArgumentsException(null, _, _))

	val IncompatibleArgumentTypesException :LazyExceptionFactory =
		ThrowableFactory(new IncompatibleArgumentTypesException(null, _, _))

	val SugaredClassCastException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredClassCastException(_, _, _))

	val SugaredConcurrentModificationException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredConcurrentModificationException(_, _, _))

	val SugaredIllegalArgumentException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredIllegalArgumentException(_, _, _))

	val SugaredIllegalStateException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredIllegalStateException(_, _, _))

	val SugaredIndexOutOfBoundsException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredIndexOutOfBoundsException(_, _, _))

	val SugaredNoSuchElementException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredNoSuchElementException(_, _, _))

	val SugaredNullPointerException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredNullPointerException(_, _, _))

	val SugaredUnsupportedOperationException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredUnsupportedOperationException(_, _, _))

}



