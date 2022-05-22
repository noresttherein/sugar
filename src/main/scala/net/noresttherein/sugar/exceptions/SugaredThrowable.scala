package net.noresttherein.sugar.exceptions

import java.io.{PrintStream, PrintWriter}

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import net.noresttherein.sugar.exceptions




/** Base trait for [[Throwable]] errors and exceptions, providing Scala-style accessors to properties of `Throwable`. */
trait SugaredThrowable extends Throwable with Cloneable {

	/** A reusable immutable sequence wrapping the throwable's stack trace */
	lazy val stackTrace :Seq[StackTraceElement] = getStackTrace match {
		case null => Nil
		case array => ArraySeq.unsafeWrapArray(array)
	}

	/** Standard [[Throwable.getSuppressed getSuppressed]] array as a scala [[Seq]]. */
	lazy val suppressed :Seq[Throwable] = getSuppressed match {
		case null => Nil
		case array => ArraySeq.unsafeWrapArray(array)
	}

	/** Reverses the exception stack, where `_.getCause` is treated as the next element on the list.
	  * The first exception of the returned list is the original cause (one without a cause), while this exception
	  * closes the list.
	  */
	lazy val causeQueue :Seq[Throwable] = exceptions.causeQueue(this)

	/** Standard [[Throwable.getCause getCause]] wrapped in an [[Option]]. */
	def cause :Option[Throwable] = Option(getCause)

	/** Sets the [[SugaredThrowable.cause cause]] of this [[Throwable]] using
	  * [[Throwable.initCause initCause]] method. This method can be called at most once, and only
	  * if a `Throwable` cause was not given as a constructor parameter of this `Throwable`.
	  */
	def cause_=(cause :Throwable) :Unit = super.initCause(cause)

	override def initCause(e :Throwable) :SugaredThrowable = { super.initCause(e); this }

	/** Standard[[Throwable.getMessage getMessage]] wrapped in an [[Option]]. */
	def message :Option[String] = Option(getMessage)

	/** Denullified [[Throwable.getMessage getMessage]] returning an empty string instead of `null` if no message
	  * was provided.
	  */
	def msg :String = getMessage match {
		case null => ""
		case string => string
	}

	/**`Option(getLocaliazedMessage)`. */
	def localizedMessage :Option[String] = Option(getLocalizedMessage)

	/**`Option(getLocaliazedMessage) getOrElse ""`. */
	def localizedMsg :String = getLocalizedMessage match {
		case null => ""
		case msg => msg
	}

	/** Traverses the exception stack defined by the standard [[Throwable.getCause Throwable.getCause]] method
	  * and returns the bottom exception. The result will never be `null`: if `this.getCause == null`
	  * then `this` is returned.
	  */
	def originalCause :Throwable = exceptions.originalCause(this)

	/** The message included in the bottom exception of the cause stack, that is the last exception,
	  * in the list with [[Throwable.getCause getCause]] as the next message.
	  * @return `Option(originalCause.getMessage)`
	  */
	def originalMessage :Option[String] = Option(originalCause.getMessage)

	/** Denullified [[Throwable.getMessage getMessage]] of the original `Throwable` cause of this exception,
	  * returning an empty string instead of `null` if no message was provided.
	  * @return [[net.noresttherein.sugar.exceptions.SugaredThrowable.originalMessage originalMessage]]` getOrElse ""`.
	  */
	def originalMsg :String = originalCause.getMessage match {
		case null => ""
		case msg => msg
	}
}


/** Base trait for exceptions, providing Scala-style accessors to properties of `Throwable` */
trait SugaredException extends Exception with SugaredThrowable


/** A base exception class extending `SugaredException` and accepting all constructor parameters of `Throwable`.
  * @param message The detailed message of this exception returned from its standard
  *                [[Throwable.getMessage getMessage]] method as well as properties
  *                [[net.noresttherein.sugar.exceptions.SugaredThrowable.message message]]
  *                and [[net.noresttherein.sugar.exceptions.SugaredThrowable.msg msg]] properties
  *                of this class. Defaults to `null`.
  * @param cause              An optional caught [[Throwable]] which caused this exception to be thrown in turn.
  *                           Defaults to `null`, signifying no deeper cause.
  * @param enableSuppression  If true, exceptions caught when handling this exception (caching it and rethrowing)
  *                           can be added to the [[Throwable.getSuppressed suppressed]] property with method
  *                           [[Throwable.addSuppressed addSuppressed]] of [[Throwable]].
  *                           Defaults to `true`, as in `Exception`. The only real reason to change this value
  *                           is sharing exception instances to reduce the cost of their creation
  *                           (in particular filling their stack trace - see the `writeableStackTrace` parameter).
  * @param writableStackTrace If true, the exception during construction is initialized with stack trace information.
  *                           Moreover, [[Throwable.setStackTrace setStackTrace]] allows to change/initialize the actual
  *                           `stackTraceProperty` with any array of [[StackTraceElement]]s.
  *                           Defaults to `true`, as in `Exception`. The only real reason to change this value
  *                           is sharing exception instances to reduce the cost of their creation.
  */
@SerialVersionUID(1L)
abstract class AbstractException(message :String = null, cause :Throwable = null,
                                 enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends Exception(message, cause, enableSuppression, writableStackTrace) with SugaredException
{
	def this(cause :Throwable) = this(null, cause)
}


/** A base error class extending `SugaredThrowable` and accepting all constructor parameters of `Throwable`.
  * @param message The detailed message of this exception returned from its standard
  *                [[Throwable.getMessage getMessage]] method as well as properties
  *                [[net.noresttherein.sugar.exceptions.SugaredThrowable.message message]]
  *                and [[net.noresttherein.sugar.exceptions.SugaredThrowable.msg msg]] properties
  *                of this class. Defaults to `null`.
  * @param cause              An optional caught [[Throwable]] which caused this exception to be thrown in turn.
  *                           Defaults to `null`, signifying no deeper cause.
  * @param enableSuppression  If true, exceptions caught when handling this exception (caching it and rethrowing)
  *                           can be added to the [[Throwable.getSuppressed suppressed]] property with method
  *                           [[Throwable.addSuppressed addSuppressed]] of [[Throwable]].
  *                           Defaults to `true`, as in `Exception`. The only real reason to change this value
  *                           is sharing exception instances to reduce the cost of their creation
  *                           (in particular filling their stack trace - see the `writeableStackTrace` parameter).
  * @param writableStackTrace If true, the exception during construction is initialized with stack trace information.
  *                           Moreover, [[Throwable.setStackTrace setStackTrace]] allows to change/initialize the actual
  *                           `stackTraceProperty` with any array of [[StackTraceElement]]s.
  *                           Defaults to `true`, as in `Exception`. The only real reason to change this value
  *                           is sharing exception instances to reduce the cost of their creation.
  */@SerialVersionUID(1L)
class AbstractError(message :String = null, cause :Throwable = null,
                    enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends Error(message, cause, enableSuppression, writableStackTrace) with SugaredThrowable
{
	def this(cause :Throwable) = this(null, cause)
}


/** A [[Throwable]] aware of its type, providing a 'copy constructor' creating a new instance of the same class,
  * with a new message and this instance as its cause. This allows rethrowing of an exception with additional context
  * information pushed higher on the call stack, without wrapping the original exception in some other class,
  * so it can be seamlessly caught by the caller.
  * @see [[net.noresttherein.sugar.exceptions.imports.rethrow]]
  * @see [[net.noresttherein.sugar.exceptions.StackableException]]
  * @see [[net.noresttherein.sugar.exceptions.Rethrowable]]
  */
trait StackableThrowable extends SugaredThrowable {

	/** A 'virtual constructor' returning an exception of the same type as this one, with the given message,
	  * and this exception as its cause. This is useful when we want to preserve both the original exception type
	  * and the initial exception as the reason, in case someone higher on the call stack would want to handle it,
	  * but also to add additional contextual information about the failed operation, such as the original parameters.
	  * This is different than [[java.lang.Throwable.addSuppressed addSuppressed]] in that the latter implies
	  * another error (though possibly related) which happened when processing this exception, while this method
	  * is used in catch-rethrow scenarios. Also, the latter accepts a new exception instance from the caller,
	  * who may pass any type of the exception but typically does not know the precise type of this
	  * exception.
	  */
	def addInfo(msg :String) :StackableThrowable
}


/** A [[Throwable]] aware of its type, providing a 'copy constructor' creating a new instance of the same class,
  * with a new message and this instance as its cause. This allows rethrowing of an exception with additional context
  * information pushed higher on the call stack, without wrapping the original exception in some other class,
  * so it can be seamlessly caught by the caller.
  *
  * The implementation of [[net.noresttherein.sugar.exceptions.StackableException.addInfo addInfo]] in this class
  * assumes the extending class has one of `(String, Throwable, Boolean, Boolean)`, `(String, Throwable)`, `(String)`
  * constructors, and calls it by reflection. Subclasses are welcome to override the method in order to create
  * a new instance directly.
  * @see [[net.noresttherein.sugar.exceptions.RethrowableException]]
  */
@SerialVersionUID(1L)
class StackableException(message :String = null, cause :Throwable = null,
                         enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends AbstractException(message, cause, enableSuppression, writableStackTrace) with StackableThrowable
{
	def this(cause :Throwable) = this(null, cause)

	override def addInfo(msg :String) :StackableThrowable =
		try {
			newThrowable(msg, this)(ClassTag[StackableThrowable](getClass))
		} catch {
			case e :Exception =>
				addSuppressed(e)
				addSuppressed(new RethrowContext(msg))
				this
		}
}


/** A mixin trait for exceptions designed to be rethrown with method
  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
  * It fills [[net.noresttherein.sugar.exceptions.SugaredThrowable.stackTrace stackTrace]] property
  * based on the stack trace of its cause (if not null). It is initialized with the suffix
  * of the stack trace of the first (non null) cause starting with the frame corresponding to the most recent call
  * of `imports.rethrow`. If there is no frame on the call stack of the cause corresponding to that method,
  * or [[Throwable.getCause getCause]]` == null`, then the stack trace returned
  * by the standard [[Throwable.getStackTrace getStackTrace]] is used as normal. In order to fully benefit
  * from this approach, an exception should be created with `writableStackTrace` property set to `false`.
  * In that case, JVM will not set internal data needed to initialize the stack trace, and super implementation
  * of `getStackTrace` will return `null`. Extending classes should therefore provide an ability to create instances
  * in three modes:
  *   1. An original exception, without an underlying cause;
  *   1. An exception with a cause, constructed in the normal manner (with `writableStackTrace == true`
  *      and [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]]` == false`,
  *      wrapping exceptions of other classes, not rethrown with the provided `rethrow` method;
  *   1. An instance with a non writeable stack trace and `isRethrown` set to `true`, created by `rethrow`
  *      to wrap a caught exception.
  *
  * Method [[net.noresttherein.sugar.exceptions.Rethrowable.addInfo addInfo]] should create instances of the third kind.
  * Its default implementation attempts to create a new instance using reflection, searching for a constructor
  * `(String, Throwable, Boolean)` - with the `Boolean` parameter assumed to be the value of `isRethrown` property -
  * or `(String, Throwable, Boolean, Boolean)`, with the `Boolean` parameters
  * assumed to be the values of `enableSuppression` and `writableStackTrace` parameters passed to the `Throwable`
  * constructor with the same signature. For better performance, a override `addInfo` in order to create the rethrown
  * instance directly, rather than through reflection.
  */
trait Rethrowable extends StackableThrowable {
	/** A flag which should be set only when this instance is thrown from method
	  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]]
	  * in this sugar. If `true` and [[Throwable.getCause getCause]]` != null`, then this [[Throwable]]
	  * will not try to fill in and use the stack trace property by default methods, but instead will use the suffix
	  * of the stack trace of its `cause` starting with most recent invocation frame for method `imports.rethrow`.
	  *
	  * As this flag will typically also imply that the stack trace is not writeable, JVM creating this instance will
	  * not initialize the internal intermediate `backtrace` with low-level stack information,
	  * which can yield performance benefits.
	  */
	def isRethrown :Boolean

	override lazy val stackTrace :Seq[StackTraceElement] = ArraySeq.unsafeWrapArray(
		if (getCause == null)
			exceptions.dropFillInStackTraceFrames(super.getStackTrace)
		else //setStackTrace would work only if stack trace is writeable, defeating the purpose of this class
			exceptions.stackTraceSuffix(this) match {
				case empty if empty.length == 0 => exceptions.dropFillInStackTraceFrames(super.getStackTrace)
				case suffix => suffix
			}
	)

	override def getStackTrace :Array[StackTraceElement] = stackTrace.toArray

	override def fillInStackTrace() :Throwable =
		if (isRethrown) this
		else super.fillInStackTrace()

	override def printStackTrace(s :PrintStream) :Unit = s.synchronized {
		exceptions.printStackTrace(this, exceptions.EmptyStackTrace, s.println)
	}
	override def printStackTrace(s :PrintWriter) :Unit = s.synchronized {
		exceptions.printStackTrace(this, exceptions.EmptyStackTrace, s.println)
	}

	override def addInfo(msg :String) :StackableThrowable =
		try {
			newRethrowable[Rethrowable](msg, this)(ClassTag(getClass))
		} catch {
			case e :Exception =>
				addSuppressed(e)
				addSuppressed(new RethrowContext(msg))
				this
		}

}


/** Base class for exceptions designed to be potentially rethrown using method
  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.rethrow rethrow]].
  * If initialized with property [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]] to `true`,
  * for example by its `(String, Throwable)` constructor with a non-null cause, this exception will be created
  * without a writeable stack trace. Instead, its stack trace will be initialized
  * with the suffix of `cause.getStackTrace` starting with the most recent call to `imports.rethrow`, yielding a small
  * performance benefit. See the documentation of [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]]
  * for more information.
  */
@SerialVersionUID(1L)
class RethrowableException(message :String, cause :Throwable, override val isRethrown :Boolean)
	extends StackableException(message, cause, true, !isRethrown) with Rethrowable
{
	def this(message :String = null, cause :Throwable = null) = this(message, cause, cause ne null)
	def this(cause :Throwable) = this(null, cause)
}


/** A [[Throwable]] designed not to be thrown, but instead added to the list of [[Throwable.addSuppressed suppressed]]
  * exceptions of another exception, providing additional information added by earlier method on the call stack
  * of another thrown, caught and rethrown exception.
  * @see [[net.noresttherein.sugar.exceptions.imports.rethrow]]
  */
@SerialVersionUID(1L)
class RethrowContext(message :String, cause :Throwable = null)
	extends Throwable(message, cause, false, false) with SugaredThrowable




/** A base class for exceptions with lazily evaluated error messages. */
@SerialVersionUID(1L)
class LazyException(initMessage: => String, cause :Throwable = null,
                    enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends Exception(null, cause, enableSuppression, writableStackTrace) with SugaredException
{
	override lazy val msg :String = initMessage
	override def message :Option[String] = Some(msg)
	override def getMessage :String = msg
}


/** A lightweight, 'temporary' exception with disabled suppression and stack trace.
  * It is thrown to indicate a well defined error discovered in a deeply nested private method,
  * with an intent of being caught by an entry method which resulted in the problematic call,
  * and rethrown as another type of exception, in a scope where more useful information about the input
  * which lead to the error is available. Instances can be often reused in that case, reducing the exception
  * overhead even further.
  */
@SerialVersionUID(1L)
class InternalException(msg :String = null, cause :Throwable = null)
	extends RuntimeException(msg, cause, false, false) with StackableThrowable
{
	override def addInfo(msg :String) :StackableThrowable = new InternalException(msg, this)
}






/** An [[Exception]] thrown to indicate a situation which clearly indicates a bug in the executed code,
  * rather than invalid parameters or external state.
  */
@SerialVersionUID(1L)
class Oops(msg :String, reason :Throwable = null) extends RuntimeException(msg, reason) with StackableThrowable {
	override def addInfo(msg :String) :StackableThrowable = new Oops(msg, this)
}


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
  *   - Never called sugar-protected obsolete methods remaining for binary compatibility;
  *
  * and similar.
  * @see [[net.noresttherein.sugar.imports.??!]]
  * @see [[net.noresttherein.sugar.imports.impossible_!]]
  */
@SerialVersionUID(1L)
class ImpossibleError(msg :String = "Implementation error", reason :Throwable = null)
	extends Error(msg, reason) with StackableThrowable
{
	override def addInfo(msg :String) :StackableThrowable = new ImpossibleError(msg, this)
}
