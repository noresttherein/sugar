package net.noresttherein.sugar.exceptions

import java.io.IOException
import java.util.ConcurrentModificationException

import net.noresttherein.sugar.reflect.prettyprint.classNameOf


/** A lightweight, 'temporary' exception with disabled suppression and stack trace.
  * It is thrown to indicate a well defined error discovered in a deeply nested private method,
  * with an intent of being caught by an entry method which resulted in the problematic call,
  * and rethrown as another type of exception, in a scope where more useful information about the input
  * which lead to the error is available. Instances can be often reused in that case, reducing the exception
  * overhead even further.
  */
@SerialVersionUID(Ver)
class InternalException(message :String, lazyMessage :() => String, cause :Throwable)
	extends RuntimeException(message, cause, false, false) with LazyException
{
	def this(message :() => String, cause :Throwable) = this(null, message, cause)
	def this(message :String, cause :Throwable) = this(message, null, cause)
	def this(message :() => String) = this(null, message, null)
	def this(message :String) = this(message, null, null)
	def this(cause :Throwable) = this(null, null, cause)
	def this() = this(null, null, null)

	protected override var lazyMsg :() => String = lazyMessage

	override def addInfo(msg :String) :SugaredThrowable = new InternalException(msg, this)
	override def addInfo(msg :() => String) :SugaredThrowable = new InternalException(msg, this)
}



/** An [[Error]] thrown to indicate a situation which clearly indicates a bug in the executed code,
  * rather than invalid parameters or external state.
  */
@SerialVersionUID(Ver)
class Oops(message :String, lazyMessage :() => String, cause :Throwable)
	extends SugaredError(message, lazyMessage, cause)
{
	override def addInfo(msg :String) :SugaredThrowable = new Oops(msg, null, this)
	override def addInfo(msg :() => String) :SugaredThrowable = new Oops(null, msg, this)
}


/** An exception thrown by code which should be impossible to execute.
  * It is different from [[AssertionError]] in that the latter is always placed in reachable code
  * and checks for situations which, while should not occur, are half-expected as possible due to programming errors.
  * This error is always thrown unconditionally, especially in places where a compiler expects an expression which,
  * to the best of the programmer's knowledge, will never be executed. Examples include:
  *   - Code following infinite loops;
  *   - Guard match patterns following a pattern list which is presumed to already cover all matched values;
  *   - Body of sealed class's methods which must have a concrete implementation (because they override
  *     an existing method), but which also must be overridden by all subclasses - for example
  *     in order to narrow down the return type;
  *   - Never called package-protected obsolete methods remaining for binary compatibility;
  *
  * and similar.
  * @see [[net.noresttherein.sugar.imports.??!]]
  * @see [[net.noresttherein.sugar.imports.impossible_!]]
  */
@SerialVersionUID(Ver)
class ImpossibleError(message :String, lazyMessage :() => String, cause :Throwable)
	extends SugaredError(message, lazyMessage, cause)
{
	override def addInfo(msg :String) :SugaredThrowable = new ImpossibleError(msg, null, this)
	override def addInfo(msg :() => String) :SugaredThrowable = new ImpossibleError(null, msg, this)
}


/** An [[IllegalArgumentException]] subclass thrown to indicate that while the method arguments are legal independently,
  * as a whole, they do not satisfy the required pre condition.
  */
@SerialVersionUID(Ver)
class IncompatibleArgumentsException(msg :String, lazyMsg :() => String, cause :Throwable)
	extends SugaredIllegalArgumentException(msg, lazyMsg, cause)
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
	override def className :String = classNameOf(this)
}

@SerialVersionUID(Ver)
class IncompatibleArgumentTypesException(msg :String, lazyMsg :() => String = null, cause :Throwable = null)
	extends IncompatibleArgumentsException(msg, lazyMsg, cause)
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
}


/** An [[ArithmeticException]] subclass mixing in [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredArithmeticException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends ArithmeticException(message) with ImplException with LazyException

/** A [[ClassCastException]] subclass mixing in [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredClassCastException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends ClassCastException(message) with ImplException with LazyException
{
	initCause(cause)
}

/** A [[ConcurrentModificationException]] subclass mixing in
  * [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredConcurrentModificationException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends ConcurrentModificationException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "concurrent modification"
		else message
		, cause
	) with ImplException with LazyException

/** An [[IllegalArgumentException]] subclass mixing in
  * [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredIllegalArgumentException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IllegalArgumentException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "illegal argument"
		else message
		, cause
	) with ImplException with LazyException

/** An [[IllegalStateException]] subclass mixing in
  * [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredIllegalStateException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IllegalStateException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "illegal state"
		else message
		, cause
	) with ImplException with LazyException

/** An [[IndexOutOfBoundsException]] subclass mixing in
  * [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredIndexOutOfBoundsException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IndexOutOfBoundsException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "index out of bounds"
		else message
	) with ImplException with LazyException
{
	initCause(cause)
}

/** An [[IOException]] subclass mixing in [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredIOException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IOException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else null
		else message
	) with ImplException with LazyException

/** A [[NoSuchElementException]] subclass mixing in
  * [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredNoSuchElementException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NoSuchElementException(message, cause) with ImplException with LazyException

/** A [[NullPointerException]] subclass mixing in [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredNullPointerException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NullPointerException(message) with ImplException with LazyException
{
	initCause(cause)
}

/** A [[NumberFormatException]] subclass mixing in [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredNumberFormatException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NumberFormatException(message) with ImplException with LazyException
{
	initCause(cause)
}

/** An [[UnsupportedOperationException]] subclass mixing in
  * [[net.noresttherein.sugar.exceptions.SugaredException SugaredException]].
  * $lazyParamsNote
  *
  * $superClassNameNote
  *
  * $hasFlexibleFactoryNote
  */
@SerialVersionUID(Ver)
class SugaredUnsupportedOperationException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends UnsupportedOperationException(message, cause) with ImplException with LazyException






//todo: reduce the number of these exceptions to a single class per Java exception.
@SerialVersionUID(Ver)
class IllegalArgumentRethrowable(msg :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IllegalArgumentException(msg, cause) with ImplException with LazyException with Rethrowable
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
	def this(msg :String) = this(msg, null, null)
	def this(msg :() => String, cause :Throwable) = this(null, msg, cause)
	def this(msg :() => String) = this(null, msg, null)
	def this() = this(null, null, null)

	override def isRethrown :Boolean = cause == null //not getCause, as it will always return null during fillInStackTrace
}

@SerialVersionUID(Ver)
class IndexOutOfBoundsRethrowable(msg :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IndexOutOfBoundsException(msg) with ImplException with LazyException with Rethrowable
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
	def this(msg :String) = this(msg, null, null)
	def this(msg :() => String, cause :Throwable) = this(null, msg, cause)
	def this(msg :() => String) = this(null, msg, null)
	def this() = this(null, null, null)

	override def isRethrown :Boolean = cause == null //not getCause, as it will always return null during fillInStackTrace
}

@SerialVersionUID(Ver)
class NoSuchElementRethrowable(msg :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NoSuchElementException(cause) with ImplException with LazyException with Rethrowable
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
	def this(msg :String) = this(msg, null, null)
	def this(msg :() => String, cause :Throwable) = this(null, msg, cause)
	def this(msg :() => String) = this(null, msg, null)
	def this() = this(null, null, null)

	override def isRethrown :Boolean = cause == null //not getCause, as it will always return null during fillInStackTrace
}

@SerialVersionUID(Ver)
class NullPointerRethrowable(msg :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NullPointerException with ImplException with LazyException with Rethrowable
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
	def this(msg :String) = this(msg, null, null)
	def this(msg :() => String, cause :Throwable) = this(null, msg, cause)
	def this(msg :() => String) = this(null, msg, null)
	def this() = this(null, null, null)

	override def isRethrown :Boolean = cause == null //not getCause, as it will always return null during fillInStackTrace

	super.initCause(cause)
}

@SerialVersionUID(Ver)
class UnsupportedOperationRethrowable(msg :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends UnsupportedOperationException(cause) with ImplException with LazyException with Rethrowable
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
	def this(msg :String) = this(msg, null, null)
	def this(msg :() => String, cause :Throwable) = this(null, msg, cause)
	def this(msg :() => String) = this(null, msg, null)
	def this() = this(null, null, null)

	override def isRethrown :Boolean = cause == null //not getCause, as it will always return null during fillInStackTrace
}
