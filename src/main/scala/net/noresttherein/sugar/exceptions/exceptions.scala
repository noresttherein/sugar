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
			new StackTraceElement(classOf[markerStackTraceElements].getName, "eval", "exceptions.scala", 15)

		private[exceptions] final val conjureThrowableStackTraceElement =
			new StackTraceElement(classOf[markerStackTraceElements].getName, "conjureThrowable", "exceptions.scala", 25)

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




/** A home to [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]] subtrait of `Throwable`
  * and its subclasses. Contains also [[net.noresttherein.sugar.exceptions.extensions extension]] methods
  * for regular exceptions.
  * @define factoryInfo A factory of '[[net.noresttherein.sugar.exceptions.SugaredException sugared]]' subclass of $class.
  */
package object exceptions extends exceptions.imports with exceptions.markerStackTraceElements {
	private[exceptions] final val Ver = 1L

	type EagerExceptionFactory            = EagerThrowableFactory[Exception]
	type LazyExceptionFactory             = LazyThrowableFactory[Exception]
	type FlexibleExceptionFactory         = FlexibleThrowableFactory[Exception]
	type EagerDetailedExceptionFactory    = EagerDetailedThrowableFactory[Exception]
	type LazyDetailedExceptionFactory     = LazyDetailedThrowableFactory[Exception]
	type FlexibleDetailedExceptionFactory = FlexibleDetailedThrowableFactory[Exception]

	type EagerRuntimeExceptionFactory            = EagerThrowableFactory[RuntimeException]
	type LazyRuntimeExceptionFactory             = LazyThrowableFactory[RuntimeException]
	type FlexibleRuntimeExceptionFactory         = FlexibleThrowableFactory[RuntimeException]
	type EagerDetailedRuntimeExceptionFactory    = EagerDetailedThrowableFactory[RuntimeException]
	type LazyDetailedRuntimeExceptionFactory     = LazyDetailedThrowableFactory[RuntimeException]
	type FlexibleDetailedRuntimeExceptionFactory = FlexibleDetailedThrowableFactory[RuntimeException]

	//todo: factories for other exception, together with methods in imports.
	/** A factory of [[net.noresttherein.sugar.exceptions.Oops! Oops]] exceptions.*/
	val Oops :FlexibleThrowableFactory[Oops] = ThrowableFactory("Oops", new Oops(_, _, _))

	/** A factory of [[net.noresttherein.sugar.exceptions.ImpossibleError! ImpossibleError]] errors.*/
	val ImpossibleError :FlexibleThrowableFactory[ImpossibleError] =
		ThrowableFactory("This location should have been impossible to reach.", new ImpossibleError(_, _, _))

	/** A factory of [[net.noresttherein.sugar.exceptions.MaxSizeReachedException! MaxSizedReachedException]] exceptions.*/
	val MaxSizeReachedException :FlexibleExceptionFactory = //Consider: making it a lazy factory
		ThrowableFactory("Size limit exceeded", new MaxSizeReachedException(_, _, _))

	/** A factory of [[net.noresttherein.sugar.exceptions.IncompatibleArgumentsException! IncompatibleArgumentsException]]
	  * subclass of [[IllegalArgumentException]].*/
	val IncompatibleArgumentsException :LazyExceptionFactory = ThrowableFactory(
		"Arguments do not satisfy method constraints.", new IncompatibleArgumentsException(null, _, _)
	)

	/** A factory of [[net.noresttherein.sugar.exceptions.IncompatibleArgumentTypesException! IncompatibleArgumentTypesException]]
	  * subclass of [[IllegalArgumentException]].*/
	val IncompatibleArgumentTypesException :LazyExceptionFactory = ThrowableFactory(
		"Argument types do not satisfy method constraints.", new IncompatibleArgumentTypesException(null, _, _)
	)

	/** $factoryInfo
	  * @define class `ArithmeticException` */
	val SugaredArithmeticException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredArithmeticException(_, _, _))

	/** $factoryInfo
	  * @define class `ClassCastException` */
	val SugaredClassCastException :FlexibleExceptionFactory =
		ThrowableFactory("Unexpected class.", new SugaredClassCastException(_, _, _))

	/** $factoryInfo
	  * @define class `ConcurrentModificationException` */
	val SugaredConcurrentModificationException :FlexibleExceptionFactory =
		ThrowableFactory("Concurrent modification.", new SugaredConcurrentModificationException(_, _, _))

	/** $factoryInfo
	  * @define class `IllegalArgumentException` */
	val SugaredIllegalArgumentException :FlexibleExceptionFactory =
		ThrowableFactory("Illegal argument.", new SugaredIllegalArgumentException(_, _, _))

	/** $factoryInfo
	  * @define class `IllegalStateException` */
	val SugaredIllegalStateException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredIllegalStateException(_, _, _))

	/** $factoryInfo
	  * @define class `IndexOutOfBoundsException` */
	val SugaredIndexOutOfBoundsException :FlexibleExceptionFactory =
		ThrowableFactory("Index out of bounds.", new SugaredIndexOutOfBoundsException(_, _, _))

	/** $factoryInfo
	  * @define class `IOException` */
	val SugaredIOException :FlexibleExceptionFactory =
		ThrowableFactory(new SugaredIOException(_, _, _))

	/** $factoryInfo
	  * @define class `NoSuchElementException` */
	val SugaredNoSuchElementException :FlexibleExceptionFactory =
		ThrowableFactory("No such element.", new SugaredNoSuchElementException(_, _, _))

	/** $factoryInfo
	  * @define class `NullPointerException` */
	val SugaredNullPointerException :FlexibleExceptionFactory =
		ThrowableFactory("null", new SugaredNullPointerException(_, _, _))

	/** $factoryInfo
	  * @define class `NumberFormatException` */
	val SugaredNumberFormatException :FlexibleExceptionFactory =
		ThrowableFactory("Invalid format.", new SugaredNumberFormatException(_, _, _))

	/** $factoryInfo
	  * @define class `UnsupportedOperationException` */
	val SugaredUnsupportedOperationException :FlexibleExceptionFactory =
		ThrowableFactory("Unsupported operation.", new SugaredUnsupportedOperationException(_, _, _))

}



