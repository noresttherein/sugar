package net.noresttherein.sugar.exceptions

import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.prettyprint.extensions.classNameExtension




/** A factory of ([[net.noresttherein.sugar.exceptions.SugaredException sugared]]) exceptions which do not evaluate
  * their message unless [[Throwable.getMessage getMessage]] is actually called.
  * The exception class is reported as the [[net.noresttherein.sugar.exceptions.ExceptionFactory.name name]]
  * given to this class; it cannot thus be caught by matching it by type, but rather need to use
  * this object and its [[net.noresttherein.sugar.exceptions.ExceptionFactory.unapply unapply]] as a match pattern.
  * Two factories with the same name are considered equal, and will match each other's exceptions.
  * Example of usage:
  * {{{
  *     object TotalPartyKillException extends ExceptionFactory //will use `TotalPartyKillException` as the exception name
  *
  *     try { fight(dragon) } catch {
  *         case TotalPartyKillException(e) => println(e) //"TotalPartyKillException: heroically slain by Kaladrax!"
  *     }
  *
  *     def fight(opponent :Monster) :Loot =
  *         if (opponent.level > 20)
  *             throw TotalPartyKillException(() => "heroically slain by " + opponent + "!")
  *         else
  *             opponent.loot
  * }}}
  */
@SerialVersionUID(Ver)
class ExceptionFactory private (maybeName :Option[String]) extends Serializable {
	/** Creates a factory using the given name as a simulated class name for exceptions created/thrown by this instance. */
	def this(name :String) = this(Some(name))

	/** Creates an `ExceptionFactory` initialized with the inner name of this class. */
	def this() = this(None)

	/** The name of this factory. It is returned by
	  * method [[net.noresttherein.sugar.exceptions.SugaredException.className className]]
	  * of the exceptions created by this factory. This in turn is used in the exception's `toString` implementation
	  * in place of its actual class name.
	  */
	val name :String = maybeName getOrElse this.className

	/** Creates a new exception with the given message.
	  * `Base` is a SAM type leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Base.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `Base` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  */ //inlined for a nicer stack trace
	@inline final def apply(exception :Base) :StackableException = exception

	/** Creates a new exception with the given message and cause.
	  * `Base` is a SAM type leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Base.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `Base` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  *///inlined for a nicer stack trace
	@inline final def apply(exception :Base, cause :Throwable) :StackableException = {
		exception.initCause(cause)
		exception
	}


	/** Throws a new exception with the given message.
	  * `Base` is a SAM type leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Base.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `Base` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  */ //inlined for a nicer stack trace
	@inline def !(exception :Base) :Nothing = throw exception

	/** Throws a new exception with the given message and cause.
	  * `Base` is a SAM type leaving only [[net.noresttherein.sugar.exceptions.ExceptionFactory.Base.apply apply]]`()`
	  * for subclasses to implement; therefore a `Function0[String]` literal in the form of `() => "danger! danger!"`
	  * in a place where a `Base` is expected - such as the argument of this method - is automatically
	  * promoted to the latter.
	  *///inlined for a nicer stack trace
	@inline final def !(exception :Base, cause :Throwable) :Exception = throw exception.initCause(cause)

	/** Matches exceptions created by this factory - or an equal one. */
	def unapply(e :Throwable) :Opt[Base] = e match {
		case base :Base if base.factory == this => Got(base)
		case _ => Lack
	}

	/** A match pattern for exceptions from this factory, extracting their error messages and causes. */
	@SerialVersionUID(Ver)
	object Extract {
		/** Matches an exception created by this factory, extracting - and evaluating - its message and,
		  * optionally, cause.
		  */
		def unapply(e :Throwable) :Opt[(String, Option[Throwable])] = e match {
			case base :Base if base.factory == ExceptionFactory.this => Got((base.msg, base.cause))
			case _ => Lack
		}
	}


	/** A base class for exceptions associated with this factory.
	  * It is an abstract type, missing implementation solely
	  * for its [[net.noresttherein.sugar.exceptions.ExceptionFactory.Base.apply apply]]`() :String` method introduces
	  * for this purpose. Methods [[net.noresttherein.sugar.exceptions.SugaredException.msg msg]],
	  * [[net.noresttherein.sugar.exceptions.SugaredException.message message]]
	  * and [[net.noresttherein.sugar.exceptions.SugaredException.getMessage getMessage]] will return the `String`
	  * returned by that method. While the class is open for extension, the intention is that instances
	  * are created only through promotion forced by one of the owning factory's `apply` methods:
	  * {{{
	  *     try {
	  *         throw AleIsOutException(() => "Gimli drank everything!")
	  *     } catch {
	  *         case AleIsOutException(e) =>
	  *             throw AleIsOutException(() => "I drank only a dozen mugs, must have been someone else!", e)
	  *     }
	  * }}}
	  */
	@SerialVersionUID(Ver) //todo: this makes most sense if it is at least Rethrowable
	abstract class Base extends StackableException(null, null, null, true, false) {
		private[ExceptionFactory] def factory = ExceptionFactory.this

		override def className :String = ExceptionFactory.this.name

		/** Returns `true` if this exception has been created by the argument factory (or another with the same name).
		  * Can be used to manually match an exception instance:
		  * {{{
		  *     object TotalPartyKill extends ExceptionFactory
		  *     try { fight(dragon) } catch {
		  *         case e :ExceptionFactory#Base if e is TotalPartyKill => error(e.msg, e)
		  *     }
		  * }}}
		  */
		def is(factory :ExceptionFactory) :Boolean = factory == ExceptionFactory.this

		/** The implementation source for [[Throwable.getMessage getMessage]].
		  * This class is a SAM type, so a literal `() => ??? :String` will be promoted to this class
		  * if it is the expected type of the expression. This creates a lightweight way to create exceptions
		  * which are quite likely to get caught and handled, rather than be propagated to a point
		  * where they are only logged.
		  */
		def apply() :String

		override lazy val getMessage :String = apply()

		override def addInfo(msg :String) :StackableThrowable = ExceptionFactory.this.apply(() => msg, this)
	}


	override def equals(that :Any) :Boolean = that match {
		case other :ExceptionFactory => (this eq other) || (other canEqual this) && name == other.name
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[ExceptionFactory]
	override def hashCode :Int = name.hashCode
}






/** An interface for `Exception` companion objects providing factory methods accepting various combinations
  * of arguments, allowing the exception class itself to only implement only the most basic one.
  * Leaves a single `apply` to implement by subclasses, with an argument list matching that of exception's constructor.
  * Note that this is a SAM type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :SpecificThrowableFactory =
  *         new TotalPartyKillException(_ :String, _ () => String, _ :Throwable, _ :Boolean, _ :Boolean)
  * }}}
  * Alternative constructors for factories are located in the companion object.  */
trait SpecificExceptionFactory extends Serializable {
	protected def apply(message :String, lazyMessage: () => String, cause :Throwable,
	                    enableSuppression :Boolean, writableStackTrace :Boolean) :Throwable

	def apply(message :String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean) :Throwable =
		this(message, null, cause, enableSuppression, writableStackTrace)

	def apply(message :() => String, cause :Throwable, enableSuppression :Boolean, writableStackTrace :Boolean)
			:Throwable =
		this(null, message, cause, enableSuppression, writableStackTrace)

	def apply(message :String, cause :Throwable) :Throwable = apply(message, null, cause, true, true)
	def apply(lazyMessage: () => String, cause :Throwable) :Throwable= apply(null, lazyMessage, cause, true, true)

	def apply(message :String) :Throwable = apply(message, null, null, true, true)
	def apply(lazyMessage: () => String) :Throwable = apply(null, lazyMessage, null, true, true)

	def apply(cause :Throwable) :Throwable = apply(defaultMessage, null, cause, true, true)
	def apply() :Throwable = apply(defaultMessage, null, null, true, true)

	protected def defaultMessage :String = null
}


/** A factory of exception factories. */
object SpecificExceptionFactory extends Serializable {
	/** Creates a factory using the given function as the exception constructor. */
	def apply(constructor :(String, () => String, Throwable, Boolean, Boolean) => Throwable) :SpecificExceptionFactory =
		new SpecificExceptionFactory {
			override def apply(message :String, lazyMessage :() => String, cause :Throwable,
			                   enableSuppression :Boolean, writableStackTrace :Boolean) :Throwable =
				constructor(message, lazyMessage, cause, enableSuppression, writableStackTrace)

			override lazy val toString :String = apply().localClassName + "Factory"
		}

	/** Creates a factory of exceptions of type `E`, which must provide
	  * a `(String, () => String, Throwable, Boolean, Boolean)` constructor.
	  */
	def apply[E <: Throwable :ClassTag] :SpecificExceptionFactory =
		new SpecificExceptionFactory {
			private[this] val constructor =
				findConstructor(classTag.runtimeClass, StringLazyStringThrowableBoolBoolArgs) match {
					case Got(cons) => cons
					case _ => throw new IllegalArgumentException(
						"No (String, () => String, Throwable, Boolean, Boolean) constructor in " +
							classTag.runtimeClass.name + "."
					)
				}
			override def apply(message :String, lazyMessage :() => String, cause :Throwable,
			                   enableSuppression :Boolean, writableStackTrace :Boolean) :Throwable =
				constructor.newInstance(
					message, lazyMessage, cause, enableSuppression, writableStackTrace
				).asInstanceOf[Throwable]

			override lazy val toString = classTag.runtimeClass.localName + "Factory"
		}
}






/** An interface for `Exception` companion objects providing factory methods accepting various combinations
  * of arguments, allowing the exception class itself to only implement only the most basic one.
  * Leaves a single `apply` to implement by subclasses, with an argument list matching that of exception's constructor.
  * Note that this is a SAM type, so a new instance can be created from a function literal of the same signature:
  * {{{
  *     val TotalPartyKillException :SpecificRethrowableFactory =
  *         new TotalPartyKillException(_ :String, _ () => String, _ :Throwable, _ :Boolean)
  * }}}
  * Alternative constructors for factories are located in the companion object.
  */
trait SpecificRethrowableFactory extends Serializable {
	protected def apply(message :String, lazyMessage: () => String, cause :Throwable, isRethrown :Boolean) :Rethrowable

	def apply(message :String, cause :Throwable, isRethrown :Boolean) :Rethrowable =
		this(message, null, cause, isRethrown)

	def apply(message :() => String, cause :Throwable, isRethrown :Boolean) :Rethrowable =
		this(null, message, cause, isRethrown)

	def apply(message :String, cause :Throwable) :Rethrowable = apply(message, null, cause, cause ne null)
	def apply(lazyMessage: () => String, cause :Throwable) :Rethrowable = apply(null, lazyMessage, cause, cause ne null)

	def apply(message :String) :Rethrowable = apply(message, null, null, false)
	def apply(lazyMessage: () => String) :Rethrowable = apply(null, lazyMessage, null, false)

	def apply(cause :Throwable) :Rethrowable = apply(defaultMessage, null, cause, cause ne null)
	def apply() :Rethrowable = apply(defaultMessage, null, null, false)

	protected def defaultMessage :String = null
}


/** A factory of exception factories. */
object SpecificRethrowableFactory extends Serializable {
	/** Creates a factory using the given function as the exception constructor. */
	def apply(constructor :(String, () => String, Throwable, Boolean) => Rethrowable) :SpecificRethrowableFactory =
		new SpecificRethrowableFactory {
			override def apply(message :String, lazyMessage :() => String, cause :Throwable, isRethrown :Boolean) =
				constructor(message, lazyMessage, cause, isRethrown)

			override lazy val toString :String = apply().localClassName + "Factory"
		}

	/** Creates a factory of exceptions of type `E`, which must provide
	  * a `(String, () => String, Throwable, Boolean, Boolean)` constructor.
	  */
	def apply[E <: Throwable :ClassTag] :SpecificRethrowableFactory =
		new SpecificRethrowableFactory {
			private[this] val constructor =
				findConstructor(classTag.runtimeClass, StringLazyStringThrowableBoolBoolArgs) match {
					case Got(cons) => cons
					case _ => throw new IllegalArgumentException(
						"No (String, () => String, Throwable, Boolean) constructor in class " +
							classTag[E].runtimeClass.name + "."
					)
				}
			override def apply(message :String, lazyMessage :() => String, cause :Throwable, isRethrown :Boolean) =
				constructor.newInstance(message, lazyMessage, cause, isRethrown).asInstanceOf[Rethrowable]
		}
}


