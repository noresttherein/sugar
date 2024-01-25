package net.noresttherein.sugar.exceptions

import java.util.ConcurrentModificationException


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


@SerialVersionUID(Ver)
class IncompatibleArgumentsException(msg :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends RuntimeException(msg, cause) with LazyException
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
}

@SerialVersionUID(Ver)
class IncompatibleArgumentTypesException(msg :String, lazyMsg :() => String = null, cause :Throwable = null)
	extends IncompatibleArgumentsException(msg, lazyMsg, cause)
{
	def this(msg :String, cause :Throwable) = this(msg, null, cause)
}




@SerialVersionUID(Ver)
class SugaredClassCastException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends ClassCastException(message) with LazyException
{
	initCause(cause)
}

@SerialVersionUID(Ver)
class SugaredConcurrentModificationException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends ConcurrentModificationException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "concurrent modification"
		else message
		, cause
	) with LazyException

@SerialVersionUID(Ver)
class SugaredIllegalArgumentException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IllegalArgumentException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "illegal argument"
		else message
		, cause
	) with LazyException

@SerialVersionUID(Ver)
class SugaredIllegalStateException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IllegalStateException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "illegal state"
		else message
		, cause
	) with LazyException

@SerialVersionUID(Ver)
class SugaredIndexOutOfBoundsException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends IndexOutOfBoundsException(
		if (lazyMsg == null && message == null)
			if (cause != null) cause.getMessage else "index out of bounds"
		else message
	) with LazyException
{
	initCause(cause)
}

@SerialVersionUID(Ver)
class SugaredNoSuchElementException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NoSuchElementException(message, cause) with LazyException

@SerialVersionUID(Ver)
class SugaredNullPointerException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends NullPointerException(message) with LazyException
{
	initCause(cause)
}

@SerialVersionUID(Ver)
class SugaredUnsupportedOperationException(message :String, protected override var lazyMsg :() => String, cause :Throwable)
	extends UnsupportedOperationException(message, cause) with LazyException






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
