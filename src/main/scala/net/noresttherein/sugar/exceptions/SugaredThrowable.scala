package net.noresttherein.sugar.exceptions

import java.io.{PrintStream, PrintWriter}

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import net.noresttherein.sugar.concurrent.Fences.{acquireFence, releaseFence}
import net.noresttherein.sugar.exceptions.reflect.{newRethrowable, newThrowable}
import net.noresttherein.sugar.reflect.prettyprint.classNameOf
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.vars.{Maybe, Opt}




/** Base trait for [[Throwable]] errors and exceptions, providing Scala-style accessors to properties of `Throwable`. */
trait SugaredThrowable extends Throwable with Cloneable {

	/** A reusable immutable sequence wrapping the throwable's stack trace */
	lazy val stackTrace :StackTrace = getStackTrace match {
		case null  => StackTrace.empty
		case array => StackTrace(array)
	}

	/** A list of manually pushed activation frames carrying information about call parameters.
	  * These can be pushed on a thread-local stack using the
	  * [[net.noresttherein.sugar.exceptions.StackContext$ StackContext]] object, and allow providing
	  * additional information about the call.
	  */
	val context :Seq[StackContext] = StackContext.get

	/** Stack traces of this exception and all its causes, with the stack for each exception omitting the frames
	  * already listed by previous (wrapping exception). If an exception doesn't have a stack trace,
	  * it is represented by an empty sequence. The first element is the stack trace of this very exception,
	  * from the point of its creation to the initial execution. Each subsequent exception omits the longest
	  * suffix (initial call sequence) common with the previous exception on the list
	  * ''which has a non empty stack trace''. Note that, because stack trace is filled when the exception
	  * is created, the stack trace of an exception does not need to be an actual prefix of the exception's cause,
	  * and, in extreme cases, may not share with it any common frames at all.
	  */
	def joinedStackTrace :Seq[StackTrace] = utils.joinedStackTrace(this)

	/** Standard [[Throwable.getSuppressed getSuppressed]] array as a scala [[Seq]]. */
	lazy val suppressed :Seq[Throwable] = getSuppressed match {
		case null => Nil
		case array => ArraySeq.unsafeWrapArray(array)
	}

	/** Calls [[Throwable.addSuppressed addSuppressed]]`(e)` and returns `this`. */
	@inline def suppress(e :Throwable) :this.type = { addSuppressed(e); this }

	/** Reverses the exception stack, where `_.getCause` is treated as the next element on the list.
	  * The first exception of the returned list is the original cause (one without a cause), while this exception
	  * closes the list.
	  */
	lazy val causeQueue :Seq[Throwable] = utils.causeQueue(this)

	/** Standard [[Throwable.getCause getCause]] wrapped in a [[net.noresttherein.sugar.vars.Opt Opt]]. */
	def cause :Opt[Throwable] = Opt(getCause)

//	/** Sets the [[SugaredThrowable.cause cause]] of this [[Throwable]] using
//	  * [[Throwable.initCause initCause]] method. This method can be called at most once, and only
//	  * if a `Throwable` cause was not given as a constructor parameter of this `Throwable`.
//	  */
//	def cause_=(cause :Throwable) :Unit = super.initCause(cause)
//
//	override def initCause(e :Throwable) :SugaredThrowable = { super.initCause(e); this }

	/** Standard[[Throwable.getMessage getMessage]] wrapped in a [[net.noresttherein.sugar.vars.Opt Opt]]. */
	def message :Opt[String] = Opt(getMessage)

	/** Denullified [[Throwable.getMessage getMessage]] returning an empty string instead of `null` if no message
	  * was provided.
	  */
	def msg :String = getMessage match {
		case null => ""
		case string => string
	}

	/**`Opt(getLocalizedMessage)`. */
	def localizedMessage :Opt[String] = Opt(getLocalizedMessage)

	/**`Option(getLocalizedMessage) getOrElse ""`. */
	def localizedMsg :String = getLocalizedMessage match {
		case null => ""
		case msg => msg
	}

	/** Traverses the exception stack defined by the standard [[Throwable.getCause Throwable.getCause]] method
	  * and returns the bottom exception. The result will never be `null`: if `this.getCause == null`
	  * then `this` is returned.
	  */
	def rootCause :Throwable = utils.rootCause(this)

	/** The message included in the bottom exception of the cause stack, that is the last exception,
	  * in the list with [[Throwable.getCause getCause]] as the next message.
	  * @return `Opt(originalCause.getMessage)`
	  */
	def rootMessage :Opt[String] = Opt(rootCause.getMessage)

	/** Denullified [[Throwable.getMessage getMessage]] of the original `Throwable` cause of this exception,
	  * returning an empty string instead of `null` if no message was provided.
	  * @return [[net.noresttherein.sugar.exceptions.SugaredThrowable.rootMessage originalMessage]]` getOrElse ""`.
	  */
	def rootMsg :String = rootCause.getMessage match {
		case null => ""
		case msg => msg
	}

	/** The name of the class used in `toString` implementation. It defaults simply to `getClass.getName`,
	  * but can overridden in 'interface' exception to hide an actual, private and often anonymous, implementation.
	  * {{{
	  *     trait MyAppException extends Exception with SugaredException {
	  *         override def className = classOf[MyAppException].getName
	  *     }
	  *     object MyAppException {
	  *         def apply(msg :String, cause :Throwable = null) :MyAppException =
	  *             new Exception(msg, cause) with MyAppException
	  *     }
	  *
	  *     try { throw MyAppException("oops") } catch {
	  *         case e :MyAppException => println(e.toString) //prints "MyAppException: oops"
	  *     }
	  * }}}
	  * Declaring an exception as a trait rather than a class not only allows multiple inheritance,
	  * but can be mixed freely to existing exceptions. This for example can be used to implement
	  * a transparent decorator class which catches all exceptions thrown by the adapted object
	  * and for every exception `E` caught, throws an `E with LocalizedException`, where `LocalizedException`
	  * is a trait using some application specific solution to override `getLocalizedMessage`.
	  * This way any existing code relying on particular exceptions being thrown will continue to work,
	  * with a benefit of extra functionality.
	  *
	  * In general, it is a good policy to return only names of public classes here - or at least those
	  * that client code can catch. Private exception classes extending some known 'interface' exception
	  * may return its name instead of their class name.
	  */
	def className :String = classNameOf(this)

	/** A 'virtual constructor' returning an exception of the same type as this one, with the given message,
	  * and this exception as its cause. This is useful when we want to preserve both the original exception type
	  * and the initial exception as the reason, in case someone higher on the call stack would want to handle it,
	  * but also to add additional contextual information about the failed operation, such as the original parameters.
	  * This is different than [[java.lang.Throwable.addSuppressed addSuppressed]] in that the latter implies
	  * another error (though possibly related) which happened when processing this exception, while this method
	  * is used in catch-rethrow scenarios. Also, the latter accepts a new exception instance from the caller,
	  * who may pass any type of the exception but typically does not know the precise type of this
	  * exception.
	  *
	  * The default implementation forwards to the overloaded variant with a lazy `() => String` parameter.
	  * It is recommended that subclasses override that method.
	  */
	def addInfo(msg :String) :SugaredThrowable = addInfo(() => msg)


	/** A 'virtual constructor' returning an exception of the same type as this one, with the given message,
	  * and this exception as its cause. This is useful when we want to preserve both the original exception type
	  * and the initial exception as the reason, in case someone higher on the call stack would want to handle it,
	  * but also to add additional contextual information about the failed operation, such as the original parameters.
	  * This is different than [[java.lang.Throwable.addSuppressed addSuppressed]] in that the latter implies
	  * another error (though possibly related) which happened when processing this exception, while this method
	  * is used in catch-rethrow scenarios. Also, the latter accepts a new exception instance from the caller,
	  * who may pass any type of the exception but typically does not know the precise type of this
	  * exception.
	  *
	  * The default implementation relies on reflection, and the search for an appropriate constructor is a heuristic
	  * looking for standard `Exception` constructor parameters. If creation of new `SugaredThrowable` fails
	  * for whatever reason, an [[net.noresttherein.sugar.exceptions.RethrowContext RethrowContext]] is instead added
	  * to the suppressed messages. Note that the latter may also fail if `enableSuppression` flag on this
	  * exception is set to `false`. It is recommended that subclasses override this method.
	  */
	def addInfo(msg :() => String) :SugaredThrowable =
		try {
			newThrowable(msg, this)(ClassTag[SugaredThrowable](getClass))
		} catch {
			case e :Exception =>
				addSuppressed(e)
				addSuppressed(new RethrowContext(msg))
				this
		}


	/** Formats the whole stack trace of this exception as a `String`
	  * in the same way as [[Throwable.printStackTrace printStackTrace]].
	  */
	def stackTraceString :String = utils.stackTraceString(this)

	/** Formats the stack trace of this exception and all its causes, listed in the reverse order.
	  * If this exception does not have a cause, this is equal to
	  * [[net.noresttherein.sugar.exceptions.extensions.ThrowableExtension.stackTraceString stackTraceString]]
	  * (and [[Throwable.printStackTrace printStackTrace]]). Otherwise, the root cause of this exception
	  * has its full stack trace formatted first, followed by stack traces of wrapping exceptions, omitting
	  * shared frames, with this exception being formatted as the last one.
	  */
	def reverseStackTraceString :String = utils.reverseStackTraceString(this)
//
//	/** Formats this exception together with its stack trace in the standard format.
//	  * The first line of the returned `String` is equal to `this.toString`; the following lines
//	  * are the same as would be printed with [[Throwable.printStackTrace Throwable.printStackTrace]].
//	  */
//	def toStringWithStackTrace :String = utils.formatWithStackTrace(this)

	override def toString :String = {
		val s :String = className
		val message :String = getLocalizedMessage
		if (message != null) s + ": " + message else s
	}
}



/** Base trait for exceptions, providing Scala-style accessors to properties of `Throwable` */
trait SugaredException extends Exception with SugaredThrowable



/** A mixin trait for `Throwable` with lazy messages. Requires the subclass to define `lazyMsg :() => String` variable.
  * Once evaluated, the message is cached, but in contested, multi threaded environment,
  * the constructor may be evaluated more than once.
  */
trait LazyThrowable extends SugaredThrowable {
	protected var lazyMsg :() => String
	private[this] var evaluatedMsg :String = super.getMessage

//	private def initMessage(msg: () => String) :Unit = lazyMsg = msg

	/** Returns the message of this exception, evaluating it, if needed. */
	override def getMessage :String = {
		val init = lazyMsg
		acquireFence()
		if (init == null)
			evaluatedMsg
		else {
			val res = init()
			evaluatedMsg = res
			releaseFence()
			lazyMsg = null
			res
		}
	}
}

/** A mixin trait for exceptions with lazy messages. Requires the subclass to define `lazyMsg :() => String` variable.
  * Once evaluated, the message is cached, but in contested, multi threaded environment,
  * the constructor may be evaluated more than once.
  */
trait LazyException extends SugaredException with LazyThrowable


/** A `SugaredException` which uses the name of its superclass, rather than its own,
  * in its [[net.noresttherein.sugar.exceptions.ImplException.className className]] property.
  * This allows throwing, for example,
  * a [[net.noresttherein.sugar.exceptions.SugaredIllegalArgumentException SugaredIllegalArgumentException]],
  * but have it (in most cases) show up in logs as a regular `IllegalArgumentException`. It diminishes the chance
  * someone will try to catch the implementation exception instead of the regular Scala/Java one, and allows
  * expressing the API in terms of standard clases, and treat the use of a sugared exception as an implementation detail.
  *
  * Another use case are exceptions of an anonymous class, which might occur either because the base class has properties
  * expressed in terms of path dependant types (for example, `ParsingException.liquid :this.format.Liquid`),
  * or because the super class is a ''single abstract method'' type, used to lift a lambda literal to an anonymous class.
  */
trait ImplException extends SugaredException {
	override def className :String = getClass.getSuperclass.name
}



/** A base exception class extending `SugaredException` and accepting all constructor parameters of `Throwable`.
  * @param initMessage        The detailed message of this exception returned from its standard
  *                           [[Throwable.getMessage getMessage]] method as well as properties
  *                           [[net.noresttherein.sugar.exceptions.SugaredThrowable.message message]]
  *                           and [[net.noresttherein.sugar.exceptions.SugaredThrowable.msg msg]] properties
  *                           of this class. Defaults to `null`.
  * @param lazyMessage        A lazy alternative to `initMessage`.
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
  */ //todo: make the message argument a union type
abstract class AbstractException(initMessage :String = null, lazyMessage :() => String = null, cause :Throwable = null,
                                 enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends Exception(initMessage, cause, enableSuppression, writableStackTrace) with LazyException
{
	def this(initMessage :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) =
		this(initMessage, null, cause, enableSuppression, writableStackTrace)

	def this(lazyMessage :() => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) =
		this(null, lazyMessage, cause, enableSuppression, writableStackTrace)

	def this(initMessage :String, cause :Throwable) = this(initMessage, null, cause, true, true)

	def this(lazyMessage :() => String, cause :Throwable) = this(null, lazyMessage, cause, true, true)

	def this(lazyMessage :() => String) = this(null, lazyMessage, null, true, true)

	def this(cause :Throwable) = this(cause.getMessage, null, cause, true, true)

	protected override var lazyMsg :() => String = lazyMessage
}


/** A convenience base class for application exceptions, which allows passing the message by name,
  * rather than as a `() => String`.
  */
abstract class AbstractLazyException(msg: => String, cause :Throwable = null,
                                     enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends AbstractException(() => msg, cause, enableSuppression, writableStackTrace)


/** A base error class extending `SugaredThrowable` and accepting all constructor parameters of `Throwable`.
  * @param message            The detailed message of this exception returned from its standard
  *                           [[Throwable.getMessage getMessage]] method as well as properties
  *                           [[net.noresttherein.sugar.exceptions.SugaredThrowable.message message]]
  *                           and [[net.noresttherein.sugar.exceptions.SugaredThrowable.msg msg]] properties
  *                           of this class. Defaults to `null`.
  * @param lazyMessage        A lazy alternative to `initMessage`.
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
@SerialVersionUID(Ver)
class SugaredError(message :String, lazyMessage :() => String, cause :Throwable,
                   enableSuppression :Boolean, writableStackTrace :Boolean)
	extends Error(message, cause, enableSuppression, writableStackTrace) with LazyThrowable
{
	def this(message :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) =
		this(message, null, cause, enableSuppression, writableStackTrace)

	def this(lazyMessage :() => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) =
		this(null, lazyMessage, cause, enableSuppression, writableStackTrace)

	def this(message :String, lazyMessage :() => String, cause :Throwable) =
		this(message, lazyMessage, cause, true, true)

	def this(message :String, cause :Throwable) = this(message, null, cause, true, true)
	def this(message :String) = this(message, null, null, true, true)

	def this(lazyMessage :() => String, cause :Throwable) = this(null, lazyMessage, cause, true, true)
	def this(lazyMessage :() => String) = this(null, lazyMessage, null, true, true)

	def this(cause :Throwable) = this(null, null, cause, true, true)
	def this() = this(null, null, null, true, true)

	protected override var lazyMsg :() => String = lazyMessage
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
  *   1. An exception with or without a cause, constructed in the normal manner (with `writableStackTrace == true`
  *      and [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]]` == false`,
  *      potentially wrapping exceptions of other classes, not rethrown with the provided `rethrow` method;
  *   1. An exception with a non writeable stack trace and `isRethrown` set to `true`, wrapping another exception
  *      and initializing the stack trace based on the stack trace of the wrapped exception - for use by `rethrow`
  *      method.
  *
  * Method [[net.noresttherein.sugar.exceptions.Rethrowable.addInfo addInfo]] should create instances of the third kind.
  * Its default implementation attempts to create a new instance using reflection, searching for a constructor
  * `(String, Throwable, Boolean)` - with the `Boolean` parameter assumed to be the value of `isRethrown` property -
  * or `(String, Throwable, Boolean, Boolean)`, with the `Boolean` parameters
  * assumed to be the values of `enableSuppression` and `writableStackTrace` parameters passed to the `Throwable`
  * constructor with the same signature. For better performance, a override `addInfo` in order to create the rethrown
  * instance directly, rather than through reflection.
  */ //consider: allowing rethrowing in any situation, not just in rethrow method
trait Rethrowable extends SugaredThrowable {
	/** A flag which should be set only when this instance is thrown from method
	  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]]
	  * in this package. If `true` and [[Throwable.getCause getCause]]` != null`, then this [[Throwable]]
	  * will not try to fill in and use the stack trace property by default methods, but instead will use the suffix
	  * of the stack trace of its `cause` starting with the most recent invocation frame for method `imports.rethrow`.
	  *
	  * As this flag will typically also imply that the stack trace is not writeable, JVM creating this instance will
	  * not initialize the internal intermediate `backtrace` with low-level stack information,
	  * which can yield performance benefits.
	  */
	def isRethrown :Boolean

	override lazy val stackTrace :StackTrace = StackTrace(
		if (getCause == null)
			utils.dropFillInStackTraceFrames(super.getStackTrace)
		else //setStackTrace would work only if stack trace is writeable, defeating the purpose of this class
			utils.stackTraceSuffix(this) match {
				case empty if empty.length == 0 => utils.dropFillInStackTraceFrames(super.getStackTrace)
				case suffix => suffix
			}
	)

	override def getStackTrace :Array[StackTraceElement] = stackTrace.toArray

	override def fillInStackTrace() :Throwable = synchronized {
		if (isRethrown) this
		else super.fillInStackTrace()
	}

	override def printStackTrace(s :PrintStream) :Unit = s.synchronized {
		utils.printStackTrace(this, s.println)
	}
	override def printStackTrace(s :PrintWriter) :Unit = s.synchronized {
		utils.printStackTrace(this, s.println)
	}

	override def addInfo(msg :String) :SugaredThrowable =
		try {
			newRethrowable[Rethrowable](msg, this)(ClassTag(getClass))
		} catch {
			case e :Exception =>
				addSuppressed(e)
				addSuppressed(new RethrowContext(msg))
				this
		}
}



/** A `Rethrowable` and `ImplException` for convenient mixing them both into exception classes. */
trait ImplRethrowable extends Exception with Rethrowable with ImplException {
	override def isRethrown :Boolean = getCause != null
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
@SerialVersionUID(Ver)
class RethrowableException(initMessage :String, lazyMessage :() => String,
                           cause :Throwable, override val isRethrown :Boolean)
	extends AbstractException(initMessage, lazyMessage, cause, true, !isRethrown) with Rethrowable
{
	def this(message :String, cause :Throwable, isRethrown :Boolean) = this(message, null, cause, isRethrown)
	def this(message :String, cause :Throwable) = this(message, null, cause, cause ne null)
	def this(message :String) = this(message, null, null, false)
	def this() = this(null, null, null, false)
	def this(message :() => String, cause :Throwable, isRethrown :Boolean) = this(null, message, cause, isRethrown)
	def this(message :() => String, cause :Throwable) = this(null, message, cause, cause ne null)
	def this(message :() => String) = this(null, message, null, false)
	def this(cause :Throwable) = this(null, null, cause, true)
}


/** A [[Throwable]] designed not to be thrown, but instead added to the list of [[Throwable.addSuppressed suppressed]]
  * exceptions of another exception, providing additional information added by earlier method on the call stack
  * of another thrown, caught and rethrown exception.
  * @see [[net.noresttherein.sugar.exceptions.imports.rethrow]]
  */
@SerialVersionUID(Ver)
class RethrowContext(initMessage :String, lazyMessage :() => String, cause :Throwable)
	extends Throwable(initMessage, cause, false, false) with LazyThrowable
{
	def this(message :String, cause :Throwable) = this(message, null, cause)
	def this(message :String) = this(message, null, null)
	def this(message :() => String, cause :Throwable) = this(null, message, cause)
	def this(message :() => String) = this(null, message, null)

	protected override var lazyMsg :() => String = lazyMessage
}
