package net.noresttherein.sugar.exceptions

import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.exceptions.Constructors.{defaultConstructor, defaultRethrowableConstructor, lazyStringConstructor, lazyStringThrowableBoolBoolConstructor, lazyStringThrowableConstructor, newLazyRethrowableConstructor, newRethrowableConstructor, rethrownLazyRethrowableConstructor, rethrownRethrowableConstructor, stringConstructor, stringThrowableBoolBoolConstructor, stringThrowableConstructor, throwableConstructor}
import net.noresttherein.sugar.extensions.{ClassExtension, classNameMethods}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Opt




/** A factory of ([[net.noresttherein.sugar.exceptions.SugaredException sugared]]) exceptions which do not evaluate
  * their message unless [[Throwable.getMessage getMessage]] is actually called.
  * The exception class is reported as the [[net.noresttherein.sugar.exceptions.ExceptionFactory.name name]]
  * given to this class; it cannot thus be caught by matching it by type, but rather need to use
  * this object and its [[net.noresttherein.sugar.exceptions.ExceptionFactory.unapply unapply]] as a match pattern.
  * Two factories with the same name are considered equal, and will match each other's exceptions.
  * Example of usage:
  * {{{
  *     object TotalPartyKill extends ExceptionFactory //will use `TotalPartyKill.Exception` as the exception name
  *
  *     try { fight(dragon) } catch {
  *         case TotalPartyKill(e) => println(e) //"TotalPartyKill.Exception: heroically slain by Kaladrax!"
  *     }
  *
  *     def fight(opponent :Monster) :Loot =
  *         if (opponent.level > 20)
  *             throw TotalPartyKill(() => "heroically slain by " + opponent + "!")
  *         else
  *             opponent.loot
  * }}}
  */
@SerialVersionUID(Ver)
class ExceptionFactory private (maybeName :Option[String]) extends Serializable {
	/** Creates a factory using the given name as a simulated class name for exceptions created/thrown by this instance. */
	def this(name :String) = this(Option(name))

	/** Creates an `ExceptionFactory` initialized with the inner name of this class. */
	def this() = this(None)

	/** The name of this factory. It is returned by
	  * method [[net.noresttherein.sugar.exceptions.SugaredException.className className]]
	  * of the exceptions created by this factory. This in turn is used in the exception's `toString` implementation
	  * in place of its actual class name. If no name has been given to the constructor, then it defaults
	  * to `this.className`.
	  */
	val name :String = maybeName getOrElse this.className

	/** The [[net.noresttherein.sugar.exceptions.SugaredThrowable.className className]] used by thrown exceptions:
	  * in their `toString` method, this name will be printed instead of an actual class name
	  * (which will normally be anonymous). Equals `this.name + ".Exception"`.
	  */
	val exceptionName :String = name + ".Exception"

	/** Creates a new exception with the given message. `Base` is a ''SAM'' type
	  * leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Exception.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `this.Exception` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  */ //inlined for a nicer stack trace
	@inline final def apply(exception :this.Exception) :SugaredException = exception

	/** Creates a new exception with the given message and cause. `Base` is a ''SAM'' type
	  *  leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Exception.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `this.Exception` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  *///inlined for a nicer stack trace
	@inline final def apply(exception :this.Exception, cause :Throwable) :SugaredException = {
		exception.initCause(cause)
		exception
	}


	/** Throws a new exception with the given message. `Base` is a ''SAM'' type
	  * leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Exception.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `this.Exception` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  */ //inlined for a nicer stack trace
	@inline def raise(exception :this.Exception) :Nothing = throw exception

	/** Throws a new exception with the given message and cause. `Base` is a ''SAM'' type
	  * leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Exception.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `this.Exception` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  *///inlined for a nicer stack trace
	@inline final def raise(exception :this.Exception, cause :Throwable) :SugaredException =
		throw exception.initCause(cause)

	/** Matches exceptions created by this factory - or an equal one. */
	def unapply(e :Throwable) :Opt[this.Exception] = e match {
		case base :Exception if base.factory == this => Got(base)
		case _ => Lack
	}

	/** A match pattern for exceptions from this factory, extracting their error messages and causes. */
	@SerialVersionUID(Ver)
	object Extract {
		/** Matches an exception created by this factory, extracting - and evaluating - its message and,
		  * optionally, cause.
		  */
		def unapply(e :Throwable) :Opt[(String, Option[Throwable])] = e match {
			case base :Exception if base.factory == ExceptionFactory.this => Got((base.msg, base.cause))
			case _ => Lack
		}
	}


	/** A base class for exceptions associated with this factory.
	  * It is an abstract type, missing implementation solely
	  * for its [[net.noresttherein.sugar.exceptions.ExceptionFactory.Exception.apply apply]]`() :String`
	  * method introduces for this purpose. Methods [[net.noresttherein.sugar.exceptions.SugaredException.msg msg]],
	  * [[net.noresttherein.sugar.exceptions.SugaredException.message message]]
	  * and [[net.noresttherein.sugar.exceptions.SugaredException.getMessage getMessage]] will return the `String`
	  * returned by that method. While the class is open for extension, the intention is that instances
	  * are created only through promotion forced by one of the owning factory's `apply` methods:
	  * {{{
	  *     try {
	  *         throw AleIsOut(() => "Gimli drank everything!")
	  *     } catch {
	  *         case AleIsOut(e) =>
	  *             throw AleIsOut(() => "I drank only a dozen mugs, must have been someone else!", e)
	  *     }
	  * }}}
	  */
	@SerialVersionUID(Ver) //todo: this makes most sense if it is at least Rethrowable
	abstract class Exception extends AbstractException(null, null, null, true, false) {
		private[ExceptionFactory] def factory = ExceptionFactory.this

		override def className :String = exceptionName

		/** Returns `true` if this exception has been created by the argument factory (or another with the same name).
		  * Can be used to manually match an exception instance:
		  * {{{
		  *     object TotalPartyKill extends ExceptionFactory
		  *     try { fight(dragon) } catch {
		  *         case e :ExceptionFactory#Base if e is TotalPartyKill => error(e.msg, e)
		  *     }
		  * }}}
		  * The preferred method however is to use [[net.noresttherein.sugar.exceptions.ExceptionFactory.unapply unapply]]
		  * of the owning factory object.
		  */
		def is(factory :ExceptionFactory) :Boolean = factory == ExceptionFactory.this

		/** The implementation source for [[Throwable.getMessage getMessage]].
		  * This class is a ''SAM'' type, so a literal `() => ??? :String` will be promoted to this class
		  * if it is the expected type of the expression. This creates a lightweight way to create exceptions
		  * which are quite likely to get caught and handled, rather than be propagated to a point
		  * where they are only logged.
		  */
		def apply() :String

		override lazy val getMessage :String = apply()

		override def addInfo(msg :String) :SugaredThrowable = ExceptionFactory.this.apply(() => msg, this)
	}


	override def equals(that :Any) :Boolean = that match {
		case other :ExceptionFactory => (this eq other) || (other canEqual this) && name == other.name
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[ExceptionFactory]
	override def hashCode :Int = name.hashCode

	override def toString :String = name
}






/** An interface for `Exception` companion objects providing factory methods accepting various combinations
  * of arguments, allowing the exception class itself to implement only the most general one.
  * Leaves a single `apply` to implement by subclasses, with an argument list matching that of exception's constructor.
  * Note that this is a ''SAM'' type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :FlexibleThrowableFactory[TotalPartyKillException] =
  *         new TotalPartyKillException(_ :String, _ () => String, _ :Throwable, _ :Boolean, _ :Boolean)
  * }}}
  * Additionally, there is a type alias
  * [[net.noresttherein.sugar.exceptions.FlexibleExceptionFactory FlexibleExceptionFactory]]
  * for `FlexibleThrowableFactory[Exception]`, so in the common case where the type of the created `Throwable`
  * does not matter, one can simply substitute it in the example above.
  * Alternative constructors for factories are located
  * in [[net.noresttherein.sugar.exceptions.ThrowableFactory$ ThrowableFactory]] object.
  * @see [[net.noresttherein.sugar.exceptions.LazyThrowableFactory LazyThrowableFactory]]
  * @see [[net.noresttherein.sugar.exceptions.EagerThrowableFactory EagerThrowableFactory]]
  */ //consider: swapping the order of lazy and eager messages
trait FlexibleThrowableFactory[E <: Throwable] extends Serializable {
	protected def apply(message :String, lazyMessage: () => String, cause :Throwable,
	                    enableSuppression :Boolean, writableStackTrace :Boolean) :E

	def apply(message :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) :E =
		this(message, null, cause, enableSuppression, writableStackTrace)

	def apply(message :() => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) :E =
		this(null, message, cause, enableSuppression, writableStackTrace)

	def apply(message :String, cause :Throwable) :E = apply(message, null, cause, true, true)
	def apply(message: () => String, cause :Throwable) :E = apply(null, message, cause, true, true)

	def apply(message :String) :E = apply(message, null, null, true, true)
	def apply(message: () => String) :E = apply(null, message, null, true, true)

	def apply(cause :Throwable) :E = apply(defaultMessage, null, cause, true, true)
	def apply() :E = apply(defaultMessage, null, null, true, true)

	/** Message argument used by factory methods which accept neither a `String` nor a `() => String` argument. */
	protected def defaultMessage :String = null
}


/** An interface for `Exception` companion objects providing factory methods accepting various combinations
  * of arguments, allowing the exception class itself to implement only the most general one.
  * Leaves a single `apply` to implement by subclasses, with an argument list matching that of exception's constructor.
  * Note that this is a ''SAM'' type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :EagerThrowableFactory[TotalPartyKillException] =
  *         new TotalPartyKillException(_, _, _, _)
  * }}}
  * The code snippet assumes that the argument types of the constructor are
  * `message :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean`.
  * Additionally, there is a type alias
  * [[net.noresttherein.sugar.exceptions.EagerExceptionFactory EagerExceptionFactory]]
  * for `EagerThrowableFactory[Exception]`, so in the common case where the type of the created `Throwable`
  * does not matter, one can simply substitute it in the example above.
  * Alternative constructors for factories are located
  * in [[net.noresttherein.sugar.exceptions.ThrowableFactory$ ThrowableFactory]] object.
  * @see [[net.noresttherein.sugar.exceptions.LazyThrowableFactory LazyThrowableFactory]]
  * @see [[net.noresttherein.sugar.exceptions.FlexibleThrowableFactory FlexibleThrowableFactory]]
  */
trait EagerThrowableFactory[E <: Throwable] extends Serializable {
	def apply(message :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) :E
	def apply(message :String, cause :Throwable) :E = apply(message, cause, true, true)
	def apply(message :String) :E = apply(message, null, true, true)
	def apply(cause :Throwable) :E = apply(defaultMessage, cause, true, true)
	def apply() :E = apply(defaultMessage, null, true, true)

	/** Message argument used by factory methods which do not accept a `message` argument. */
	protected def defaultMessage :String = null
}


/** An interface for companion objects of `Throwable` subclasses supporting lazy message arguments.
  * It contains factory methods accepting various combinations of arguments, allowing the exception class itself
  * to implement only the most general one. Leaves a single `apply` to implement by subclasses, with an argument list
  * of `(message :() => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean)`.
  * Note that this is a ''SAM'' type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :LazyThrowableFactory[TotalPartyKillException] =
  *         new TotalPartyKillException(_, _, _, _)
  * }}}
  * Additionally, there is a type alias
  * [[net.noresttherein.sugar.exceptions.LazyExceptionFactory LazyExceptionFactory]]
  * for `LazyThrowableFactory[Exception]`, so in the common case where the type of the created `Throwable`
  * does not matter, one can simply substitute it in the example above.
  * Alternative constructors for factories are located
  * in [[net.noresttherein.sugar.exceptions.ThrowableFactory$ ThrowableFactory]] object.
  * @see [[net.noresttherein.sugar.exceptions.EagerThrowableFactory EagerThrowableFactory]]
  * @see [[net.noresttherein.sugar.exceptions.FlexibleThrowableFactory FlexibleThrowableFactory]]
  */
trait LazyThrowableFactory[E <: Throwable] extends Serializable {
	def apply(message: => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) :E
	def apply(message: => String, cause :Throwable) :E = apply(message, cause, true, true)
	def apply(message: => String) :E = apply(message, null, true, true)
	def apply(cause :Throwable) :E = apply(defaultMessage, cause, true, true)
	def apply() :E = apply(defaultMessage, null, true, true)

	/** Message argument used by factory methods which do not accept a `message` argument. */
	protected def defaultMessage :String = null
}


/** A factory of exception factories. */
@SerialVersionUID(Ver)
object ThrowableFactory extends Serializable {

	/** Creates a factory using the given function as the exception constructor.
	  * @param constructor A constructor function for thrown exception, accepting;
	  *                      1. message
	  *                      1. lazy message constructor
	  *                      1. cause
	  *                      1. enableSuppression
	  *                      1. writableStackTrace
	  *                    Note that the interpretation of these arguments, in particular how to treat the case where
	  *                    both ''message'' and ''lazy message'' arguments are non null lies squarely on the side
	  *                    of the function, and the returned object will simply pass on arguments
	  *                    given to its factory methods in the same order to the constructor.
	  */
	def apply[E <: Throwable](constructor :(String, () => String, Throwable, Boolean, Boolean) => E)
			:FlexibleThrowableFactory[E] =
		new FlexibleThrowableFactory[E] {
			override def apply(message :String, lazyMessage :() => String, cause :Throwable,
			                   enableSuppression :Boolean, writableStackTrace :Boolean) :E =
				constructor(message, lazyMessage, cause, enableSuppression, writableStackTrace)

			override lazy val toString :String = apply().localClassName + "Factory"
		}
//
//	/** Creates a factory using the given function as the exception constructor. */
//	def apply[E <: Throwable](constructor :(String, () => String, Throwable) => E) :FlexibleThrowableFactory[E] =
//		apply { (msg, lzy, cause, _, _) => constructor(msg, lzy, cause) }

	/** Creates a factory using the given function as the exception constructor.
	  * @param constructor A constructor function for thrown exception, accepting;
	  *                      1. lazy message constructor
	  *                      1. cause
	  *                      1. enableSuppression
	  *                      1. writableStackTrace
	  *                    Note that the interpretation of these arguments lies squarely on the side of the function,
	  *                    and the returned object will simply pass on arguments given to its factory methods
	  *                    in the same order to the constructor.
	  */
	def apply[E <: Throwable](constructor :(() => String, Throwable, Boolean, Boolean) => E) :LazyThrowableFactory[E] =
		new LazyThrowableFactory[E] {
			override def apply(message: => String, cause :Throwable,
			                   enableSuppression :Boolean, writableStackTrace :Boolean) :E =
				constructor(() => message, cause, enableSuppression, writableStackTrace)
			override lazy val toString = apply().localClassName + "Factory"
		}
//
//	/** Creates a factory using the given function as the exception constructor. */
//	def apply[E <: Throwable](constructor :(() => String, Throwable) => Throwable) :LazyThrowableFactory[E] =
//		apply { (lzy, cause, _, _) => constructor(lzy, cause) }

	/** Creates a factory using the given function as the exception constructor.
	  * @param constructor A constructor function for thrown exception, accepting;
	  *                      1. message
	  *                      1. cause
	  *                      1. enableSuppression
	  *                      1. writableStackTrace
	  *                    Note that the interpretation of these arguments lies squarely on the side of the function,
	  *                    and the returned object will simply pass on arguments given to its factory methods
	  *                    in the same order to the constructor.
	  */
	def apply[E <: Throwable](constructor :(String, Throwable, Boolean, Boolean) => E) :EagerThrowableFactory[E] =
		new EagerThrowableFactory[E] {
			override def apply(message :String, cause :Throwable,
			                   enableSuppression :Boolean, writableStackTrace :Boolean) :E =
				constructor(message, cause, enableSuppression, writableStackTrace)
			override lazy val toString = apply().localClassName + "Factory"
		}
//
//	def apply[E <: Throwable](constructor :(String, Throwable) => E) :EagerThrowableFactory[E] =
//		apply { (msg, cause, _, _) => constructor(msg, cause) }

	/** Creates a factory of exceptions of type `E`, which must provide one of constructors:
	  *   1. `(String, () => String, Throwable, Boolean, Boolean)`,
	  *   1. `(String, Throwable, Boolean, Boolean)`, or
	  *   1. `(() => String, Throwable, Boolean, Boolean)`.
	  */
	def apply[E <: Throwable :ClassTag] :FlexibleThrowableFactory[E] =
		new FlexibleThrowableFactory[E] {
			private[this] val noArgConstructor =
				defaultConstructor[E].orIllegal(
					"No (), (String), (Throwable), (String, Throwable), (String, Throwable, Boolean, Boolean), " +
						"(() => String), (() => String, Throwable), (() => String, Throwable, Boolean, Boolean) or " +
						"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)
			private[this] val cause = noArgConstructor()

			private[this] val fullConstructor :(String, Throwable, Boolean, Boolean) => E =
				stringThrowableBoolBoolConstructor[E].orIllegal(
					"No (String, Throwable, Boolean, Boolean), " +
					"(String, () => String, Throwable, Boolean, Boolean) or " +
					"(() => String, Throwable, Boolean, Boolean) constructor in " + classTag[E].runtimeClass.name + "."
				)
			fullConstructor("", cause, false, false) //test if it works

			private[this] val lazyFullConstructor =
				lazyStringThrowableBoolBoolConstructor[E] orElse
					stringThrowableBoolBoolConstructor[E].map {
						cons => (s :() => String, e :Throwable, es :Boolean, ws :Boolean) => cons(s(), e, es, ws)
					} orIllegal (
						"No (() => String, Throwable, Boolean, Boolean), (String, Throwable, Boolean, Boolean), or " +
						"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
							classTag[E].runtimeClass.name + "."
					)
			lazyFullConstructor(() => "", cause, false, false) //test if it works

			private[this] val standardConstructor =
				stringThrowableConstructor[E] orIllegal (
					"No (String, Throwable), (String), (String, Throwable, Boolean, Boolean), " +
					"(() => String, Throwable), (() => String), (() => String, Throwable, Boolean Boolean) or " +
					"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)
			standardConstructor("", cause) //test if it works

			private[this] val lazyStandardConstructor =
				lazyStringThrowableConstructor[E] orElse
					stringThrowableConstructor[E].map {
						cons => (s :() => String, e :Throwable) => cons(s(), e)
					} orIllegal (
						"No (() => String, Throwable), (() => String), (() => String, Throwable, Boolean Boolean), " +
						"(String, Throwable), (String), (String, Throwable, Boolean, Boolean), or " +
						"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
							classTag[E].runtimeClass.name + "."
					)
			lazyStandardConstructor(() => "", cause) //test if it works

			private[this] val messageConstructor :String => E =
				stringConstructor[E].orIllegal(
					"No (String), (String, Throwable), (String, Throwable, Boolean, Boolean), " +
					"(() => String), (() => String, Throwable), (() => String, Throwable, Boolean, Boolean) or " +
					"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)
			messageConstructor("") //test if it works

			private[this] val lazyMessageConstructor =
				lazyStringConstructor[E] orElse stringConstructor[E].map {
					cons => (s :() => String) => cons(s())
				} orIllegal (
					"No (() => String), (() => String, Throwable), (() => String, Throwable, Boolean, Boolean) " +
					"or (String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)
			lazyMessageConstructor(() => "") //test if it works

			private[this] val causeConstructor =
				throwableConstructor[E].orIllegal(
					"No (Throwable), (), (String), (String, Throwable), (String, Throwable, Boolean, Boolean), " +
					"(() => String), (() => String, Throwable), (() => String, Throwable, Boolean, Boolean) or " +
					"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)
			causeConstructor(cause) //test if it works

			override def apply(message :String, lazyMessage :() => String, cause :Throwable,
			                   enableSuppression :Boolean, writableStackTrace :Boolean) :E =
				if (message == null)
					lazyFullConstructor(lazyMessage, cause, enableSuppression, writableStackTrace)
				else
					fullConstructor(message, cause, enableSuppression, writableStackTrace)


			override def apply(message :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) =
				fullConstructor(message, cause, enableSuppression, writableStackTrace)

			override def apply(message :() => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) =
				lazyFullConstructor(message, cause, enableSuppression, writableStackTrace)

			override def apply(message :String, cause :Throwable) = standardConstructor(message, cause)
			override def apply(lazyMessage :() => String, cause :Throwable) = lazyStandardConstructor(lazyMessage, cause)

			override def apply(message :String) = messageConstructor(message)
			override def apply(lazyMessage :() => String) = lazyMessageConstructor(lazyMessage)

			override def apply(cause :Throwable) = causeConstructor(cause)
			override def apply() = noArgConstructor()

			override lazy val toString = classTag.runtimeClass.localName + "Factory"
		}
}






//For this to work, we'd need either to duplicate everything (and not be an ExceptionFactory),
// or have a generic supertype for both,
//@SerialVersionUID(Ver)
//class RethrowableFactory(name :String) extends ExceptionFactory(name) {
//	def this() = this(null)
//
//	override
//}




/** An interface of [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] companion objects,
  * providing factory methods accepting various combinations of arguments, allowing the exception class itself
  * to only implement only the most basic one. Leaves a single `apply` to implement by subclasses,
  * with an argument list matching
  * that of [[net.noresttherein.sugar.exceptions.RethrowableException RethrowableException]]'s primary constructor.
  * Note that this is a ''SAM'' type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :FlexibleRethrowableFactory =
  *         new TotalPartyKillException(_ :String, _ () => String, _ :Throwable, _ :Boolean)
  * }}}
  * Factory methods which do not accept an explicit `isRethrown` argument, define it is as `cause != null`
  * (where `cause` argument is present), or `false` for root exceptions without a cause.
  * Alternative constructors for factories are located in object
  * [[net.noresttherein.sugar.exceptions.RethrowableFactory$ RethrowableFactory]].
  * @see [[net.noresttherein.sugar.exceptions.EagerRethrowableFactory EagerRethrowableFactory]]
  * @see [[net.noresttherein.sugar.exceptions.LazyRethrowableFactory LazyRethrowableFactory]]
  */ //consider: swapping the order of message and lazyMessage
trait FlexibleRethrowableFactory extends Serializable {
	protected def apply(message :String, lazyMessage: () => String, cause :Throwable, isRethrown :Boolean) :Rethrowable

	def apply(message :String, cause :Throwable, isRethrown :Boolean) :Rethrowable =
		this(message, null, cause, isRethrown)

	def apply(message :() => String, cause :Throwable, isRethrown :Boolean) :Rethrowable =
		this(null, message, cause, isRethrown)

	def apply(message :String, cause :Throwable) :Rethrowable = apply(message, null, cause, cause ne null)
	def apply(message: () => String, cause :Throwable) :Rethrowable = apply(null, message, cause, cause ne null)

	def apply(message :String) :Rethrowable = apply(message, null, null, false)
	def apply(message: () => String) :Rethrowable = apply(null, message, null, false)

	def apply(cause :Throwable) :Rethrowable = apply(defaultMessage, null, cause, cause ne null)
	def apply() :Rethrowable = apply(defaultMessage, null, null, false)

	/** Message argument used by factory methods which accept neither a `String` nor a `() => String` argument. */
	protected def defaultMessage :String = null
}


/** An interface for [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] companion objects,
  * providing factory methods accepting various combinations of arguments, allowing the exception class itself
  * to only implement only the most basic one. Leaves a single `apply` to implement by subclasses,
  * with an argument list of `(message :String, cause :Throwable, isRethrown :Boolean)`.
  * Note that this is a ''SAM'' type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :EagerRethrowableFactory =
  *         new TotalPartyKillException(_, _, _)
  * }}}
  * Factory methods which do not accept an explicit `isRethrown` argument, define it is as `cause != null`
  * (where `cause` argument is present), or `false` for root exceptions without a cause.
  * Alternative constructors for factories are located in object
  * [[net.noresttherein.sugar.exceptions.RethrowableFactory$ RethrowableFactory]].
  * @see [[net.noresttherein.sugar.exceptions.LazyRethrowableFactory]]
  * @see [[net.noresttherein.sugar.exceptions.FlexibleRethrowableFactory]]
  */
trait EagerRethrowableFactory extends Serializable {
	def apply(message :String, cause :Throwable, isRethrown :Boolean) :Rethrowable
	def apply(message :String, cause :Throwable) :Rethrowable = apply(message, cause, cause ne null)
	def apply(message :String) :Rethrowable = apply(message, null, false)
	def apply(cause :Throwable) :Rethrowable = apply(defaultMessage, cause, cause ne null)
	def apply() :Rethrowable = apply(defaultMessage, null, false)

	/** Message argument used by factory methods which do not accept a `message` argument. */
	protected def defaultMessage :String = null
}


/** An interface for companion objects of [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] subclasses 
  * with a lazily evaluated message argument. It contains factory methods accepting various combinations of arguments, 
  * allowing the exception class itself to only implement only the most basic one. Leaves a single `apply` 
  * to implement by subclasses, with an argument list of `(message: => String, cause :Throwable, isRethrown :Boolean)`.
  * Note that this is a ''SAM'' type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :LazyRethrowableFactory =
  *         new TotalPartyKillException(_, _, _)
  * }}}
  * Factory methods which do not accept an explicit `isRethrown` argument, define it is as `cause != null`
  * (where `cause` argument is present), or `false` for root exceptions without a cause.
  * Alternative constructors for factories are located in object
  * [[net.noresttherein.sugar.exceptions.RethrowableFactory$ RethrowableFactory]].
  * @see [[net.noresttherein.sugar.exceptions.EagerRethrowableFactory]]
  * @see [[net.noresttherein.sugar.exceptions.FlexibleRethrowableFactory]]
  */
trait LazyRethrowableFactory extends Serializable {
	def apply(message: => String, cause :Throwable, isRethrown :Boolean) :Rethrowable
	def apply(message: => String, cause :Throwable) :Rethrowable = apply(message, cause, cause ne null)
	def apply(message: => String) :Rethrowable = apply(message, null, false)
	def apply(cause :Throwable) :Rethrowable = apply(defaultMessage, cause, cause ne null)
	def apply() :Rethrowable = apply(defaultMessage, null, false)

	/** Message argument used by factory methods which do not accept a `message` argument. */
	protected def defaultMessage :String = null
}



/** A factory of exception factories. */
@SerialVersionUID(Ver)
object RethrowableFactory extends Serializable {
	/** Creates a factory using the given function
	  * as the [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] constructor.
	  * @param constructor A constructor function for thrown exception, accepting;
	  *                      1. lazy message constructor
	  *                      1. cause
	  *                      1. isRethrown
	  *                    Note that the interpretation of these arguments lies squarely on the side of the function,
	  *                    and the returned object will simply pass on arguments given to its factory methods
	  *                    in the same order to the constructor.
	  */
	def apply(constructor :(String, () => String, Throwable, Boolean) => Rethrowable) :FlexibleRethrowableFactory =
		new FlexibleRethrowableFactory {
			override def apply(message :String, lazyMessage :() => String, cause :Throwable, isRethrown :Boolean) =
				constructor(message, lazyMessage, cause, isRethrown)

			override lazy val toString :String = apply().localClassName + "Factory"
		}

	/** Creates a factory using the given function
	  * as the [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] constructor.
	  * @param constructor A constructor function for thrown exception, accepting;
	  *                      1. message
	  *                      1. cause
	  *                      1. isRethrown
	  *                    Note that the interpretation of these arguments lies squarely on the side of the function,
	  *                    and the returned object will simply pass on arguments given to its factory methods
	  *                    in the same order to the constructor.
	  */
	def apply(constructor :(String, Throwable, Boolean) => Rethrowable) :EagerRethrowableFactory =
		new EagerRethrowableFactory {
			override def apply(message :String, cause :Throwable, isRethrown :Boolean) :Rethrowable =
				constructor(message, cause, isRethrown)

			override lazy val toString :String = apply().localClassName + "Factory"
		}

	/** Creates a factory using the given function
	  * as the [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] constructor.
	  * @param constructor A constructor function for thrown exception, accepting;
	  *                      1. lazy message constructor
	  *                      1. cause
	  *                      1. isRethrown
	  *                    Note that the interpretation of these arguments lies squarely on the side of the function,
	  *                    and the returned object will simply pass on arguments given to its factory methods
	  *                    in the same order to the constructor.
	  */
	def apply(constructor :(() => String, Throwable, Boolean) => Rethrowable) :LazyRethrowableFactory =
		new LazyRethrowableFactory {
			override def apply(message: => String, cause :Throwable, isRethrown :Boolean) =
				constructor(() => message, cause, isRethrown)

			override lazy val toString :String = apply().localClassName + "Factory"
		}

	/** Creates a factory of exceptions of type `E`, which must provide a constructor with one of the signatures;
	  *   - `(String, () => String, Throwable, Boolean)`
	  *   - `(() => String, Throwable, Boolean)`
	  *   - `(String, Throwable, Boolean)`
	  */
	def apply[E <: Rethrowable :ClassTag] :FlexibleRethrowableFactory =
		new FlexibleRethrowableFactory {
			private[this] val noArgConstructor :() => E =
				defaultRethrowableConstructor[E] orIllegal (
					"No (), (Throwable), (String), (() => String), (String, Throwable), (() => String, Throwable), " +
					"(String, Throwable, Boolean, Boolean), (() => String, Throwable, Boolean, Boolean) or " +
					"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)

			private[this] val newConstructor :String => E =
				newRethrowableConstructor[E] orIllegal {
					"No (String), (() => String), (String, Throwable, Boolean), or (() => String, Throwable, Boolean) " +
					"constructor in " + classTag[E].runtimeClass.name + "."
				}
			private[this] val newLazyConstructor :(() => String) => E =
				newLazyRethrowableConstructor[E].orElse(
					newRethrowableConstructor[E].map(cons => (s :() => String) => cons(s()))
				).orIllegal {
					"No (String), (() => String), (String, Throwable, Boolean), or (() => String, Throwable, Boolean) " +
					"constructor in " + classTag[E].runtimeClass.name + "."
				}
			private[this] val rethrownConstructor :(String, Throwable) => E =
				rethrownRethrowableConstructor[E] orIllegal {
					"No (String, () => String, Throwable, Boolean) constructor in class " +
							classTag[E].runtimeClass.name + "."
				}
			private[this] val rethrownLazyConstructor :(() => String, Throwable) => E = //this is not ideal, as we won't use the preceding constructor
				rethrownLazyRethrowableConstructor[E].orElse(
					rethrownRethrowableConstructor[E].map(cons => (s :() => String, e :Throwable) => cons(s(), e))
				).orIllegal (
					"No (String, Throwable), (String), (String, Throwable, Boolean, Boolean), " +
					"(() => String, Throwable), (() => String), (() => String, Throwable, Boolean, Boolean) or " +
					"(String, () => String, Throwable, Boolean, Boolean) constructor in " +
						classTag[E].runtimeClass.name + "."
				)

			override def apply(message :String, lazyMessage :() => String, cause :Throwable, isRethrown :Boolean) =
				if (message != null)
					if (isRethrown) rethrownConstructor(message, cause) else newConstructor(message)
				else
					if (isRethrown) rethrownLazyConstructor(lazyMessage, cause) else newLazyConstructor(lazyMessage)

			override def apply(message :String, cause :Throwable, isRethrown :Boolean) =
				if (isRethrown) rethrownConstructor(message, cause) else newConstructor(message)

			override def apply(message :() => String, cause :Throwable, isRethrown :Boolean) =
				if (isRethrown) rethrownLazyConstructor(message, cause) else newLazyConstructor(message)

			override def apply(message :String, cause :Throwable) = rethrownConstructor(message, cause)
			override def apply(message :() => String, cause :Throwable) = rethrownLazyConstructor(message, cause)

			override def apply(message :String) = newConstructor(message)
			override def apply(message :() => String) = newLazyConstructor(message)

			override def apply(cause :Throwable) = rethrownConstructor(null, cause)

			override def apply() = noArgConstructor()

			override lazy val toString = classTag[E].runtimeClass.localName + "Factory"
		}
}


