package net.noresttherein.sugar

import scala.annotation.nowarn




package exceptions {
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
}




package object exceptions extends exceptions.imports with exceptions.markerStackTraceElements {
	private[exceptions] final val Ver = 1L

	type EagerExceptionFactory    = EagerThrowableFactory[Exception]
	type LazyExceptionFactory     = LazyThrowableFactory[Exception]
	type FlexibleExceptionFactory = FlexibleThrowableFactory[Exception]
}


