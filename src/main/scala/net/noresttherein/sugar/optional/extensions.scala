package net.noresttherein.sugar.optional

import scala.reflect.ClassTag

import net.noresttherein.sugar.exceptions.raise
import net.noresttherein.sugar.optional.extensions.{OptionExtension, OptionCompanionExtension, ifTrueMethods, providingMethods, satisfyingMethods}
import net.noresttherein.sugar.vars.{Fallible, Opt, Pill, Potential, Unsure}
import net.noresttherein.sugar.vars.Fallible.{Failed, Passed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}




/** Extension methods for `Option` as well as expressions/values of any type which lift them to an `Option`
  * based on some condition.
  */
trait extensions extends Any {
	/** Adds extension methods to any `Option[T]` providing alternatives if the option is empty. */
	@inline implicit final def OptionExtension[T](self :Option[T]) :OptionExtension[T] = new OptionExtension[T](self)

	/** Adds `ifTrue` and `ifFalse` methods to any `Boolean` value which lift any argument expression to an `Option`. */
	@inline implicit final def ifTrueMethods(self :Boolean) :ifTrueMethods = new ifTrueMethods(self)

	/** Adds `satisfying` and `violating` methods to any object for lifting it to an `Option[T]`
	  * based on a predicate value.
	  */
	@inline implicit final def satisfyingMethods[T](self :T) :satisfyingMethods[T] = new satisfyingMethods[T](self)

	/** Adds `providing` and `unless` methods to any by-name expression, returning it as an option
	  * after testing a boolean condition.
	  */
	@inline implicit final def providingMethods[T](self: => T) :providingMethods[T] = new providingMethods[T](self)

//duplicated in casting
//	/** Adds `ifInstanceOf` and `ifSubclass` methods to any value providing `Option`-returning casting
//	  * and optional execution of a function based on the argument's runtime class.
//	  */
//	@inline implicit final def ifInstanceOfMethods[T](self :T) = new IfInstanceOfMethods[T, R](self)

	@inline implicit final def OptionCompanionExtension(self :Option.type) :OptionCompanionExtension =
		new OptionCompanionExtension {}
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	/** Enriches an `Option[T]` with additional methods providing alternatives. */
	class OptionExtension[T](private val self :Option[T]) extends AnyVal {

		/** Applies the given function to the content of this option and returns the result or the provided alternative
		  * if this option is empty. Equivalent to `this map f getOrElse alternative`, but in one step.
		  */
		@inline def mapOrElse[X](f :T => X, alternative: => X) :X = self match {
			case Some(t) => f(t)
			case _ => alternative
		}

		/** Similar to `map`, but the function is executed in a try-catch block and `None` is returned
		  * in case any exceptions are thrown.
		  */
		@inline def guardMap[X](f :T => X) :Option[X] = self match {
			case Some(t) => try {
				Some(f(t))
			} catch {
				case _ :Exception => None
			}
			case _ => None
		}


		/** Gets the element in the option or throws a `NoSuchElementException` with the given message. */
		@inline def orThrow(msg: => String) :T = self match {
			case Some(t) => t
			case _ => throw new NoSuchElementException(msg)
		}

		/** Gets the element in the option or throws the exception given as the type parameter with the given message.
		  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
		  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  */
		@inline def orThrow[E <: Exception :ClassTag](msg: => String) :T = self getOrElse raise[E](msg)

		/** Gets the element in this `Option` or throws a `NoSuchElementException` with the given message.
		  * @see [[net.noresttherein.sugar.optional.extensions.OptionExtension.orThrow orThrow]]
		  */
		@inline def orNoSuch(msg: => String) :T = self match {
			case Some(t) => t
			case _ => throw new NoSuchElementException(msg)
		}

		/** Gets the element in the option or throws an `IllegalArgumentException` with the given message. */
		@inline def orIllegal(msg: => String) :T = self match {
			case Some(t) => t
			case _ => throw new IllegalArgumentException(msg)
		}

		/** Asserts that this instance is not empty, throwing an `AssertionError` otherwise, and returns its contents. */
		@inline def orError(msg: => String) :T = self match {
			case Some(t) => t
			case _ => throw new AssertionError(msg)
		}


		/** Similarly to `orElse`, returns this `Option` if it is not empty
		  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
		  * no closure would be be created, at the cost of possibly discarding it without use.
		  * @param alt the value to return if this instance is empty.
		  */
		@inline def ifEmpty[O >: T](alt: Option[O]) :Option[O] = self match {
			case None => alt
			case _ => self
		}

		/** Returns this option if the condition is false and `None` if it is true. This is equivalent
		  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
		  */
		@inline def orNoneIf(condition :Boolean) :Option[T] =
			if (condition) None else self

		/** Returns this option if the condition is true and `None` if it is false. This is equivalent
		  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
		  */
		@inline def orNoneUnless(condition :Boolean) :Option[T] =
			if (condition) self else None

		/** Converts this option into an [[net.noresttherein.sugar.vars.Opt Opt]] wrapping the same type/value. */
		@inline def toOpt :Opt[T] = Opt.some_?(self)

		/** Converts this option into an [[net.noresttherein.sugar.vars.Unsure Unsure]] wrapping the same type/value. */
		@inline def toUnsure :Unsure[T] = Unsure.some_?(self) //not specialized

		/** Converts this option into an [[net.noresttherein.sugar.vars.Potential Potential]] wrapping the same type/value. */
		@inline def toPotential :Potential[T] = Potential.some_?(self) //not specialized


		/** Converts this `Option` to `Pill`, returning the content as `Red`, or the value of the given expression
		  * as `Blue` if empty. */
		@inline def toRed[O](blue : => O) :Pill[T, O] =
			if (self.isDefined) Blue(blue) else Red(self.get)

		/** Converts this `Option` to `Pill`, returning the content as `Blue`, or the value of the given expression
		  * as `Red` if empty. */
		@inline def toBlue[O](red : => O) :Pill[O, T] =
			if (self.isDefined) Red(red) else Blue(self.get)

		/** Converts this `Option` to `Fallible`, returning the content as `Passed`,
		  * or the value of the given `String` as `Failed` error message if empty. */
		@inline final def toPassed(err : => String) :Fallible[T] =
			if (self.isDefined) Failed(() => err) else Passed(self.get)
	}



	/** An implicit extension of `Boolean` values providing `ifTrue` method lifting its argument to an `Option` based
	  * on the value of this expression.
	  * {{{
	  *     s == "yes" ifTrue "hurrah"
	  * }}}
	  * is equivalent to
	  * {{{
	  *     Option("hurrah").filter(_ => s == "yes")
	  * }}}
	  * but arguably reads better, especially if the pattern matching order feels more natural in the given place in code.
	  * @param condition boolean value serving as a guard of a conditional expression.
	  */
	class ifTrueMethods(private val condition :Boolean) extends AnyVal {

		/** If `this` boolean expression is true, return the given value in an `Option`. If it evaluates to `false`,
		  * return `None`.
		  * @param thenThis value of the expression to return wrapped in `Option` if this condition is true.
		  * @tparam T value type of this expression
		  * @return `Some(thenThis)` if this condition is true and `thenThis` is not null or `None` otherwise.
		  */
		@inline def ifTrue[T](thenThis: => T) :Option[T] =
			if (condition) Option(thenThis)
			else None


		/** If `this` boolean expression is false, return the given value in an `Option`. If it evaluates to `true`,
		  * return `None`.
		  * @param thenThis value of the expression to return wrapped in `Option` if this condition is false.
		  * @tparam T value type of this expression
		  * @return `Some(thenThis)` if this condition is true and `thenThis` is not null or `None` otherwise.
		  */
		@inline def ifFalse[T](thenThis: => T) :Option[T] =
			if (condition) None else Option(thenThis)


		/** If `this` boolean expression is true, return the value of the by-name parameter. If it evaluates to `false`,
		  * return `None`.
		  * @param thenThis the value to return wrapped if this condition is true.
		  * @tparam T value type of this expression
		  * @return `thenThis` if this condition is true or `None` otherwise.
		  */
		@inline def thenMaybe[T](thenThis: => Option[T]) :Option[T] =
			if (condition) thenThis else None


		/** If `this` boolean expression is false, return the given value in an `Option`. If it evaluates to `true`,
		  * return `None`.
		  * @param thenThis value of the expression to return wrapped in `Option` if this condition is false.
		  * @tparam T value type of this expression
		  * @return `Some(thenThis)` if this condition is true and `thenThis` is not null or `None` otherwise.
		  */
		@inline def otherwiseMaybe[T](thenThis: => Option[T]) :Option[T] =
			if (condition) None else thenThis
	}



	/** Extension methods of an arbitrary value treating it as a default value to be lifted to an `Option`
	  * depending on whether a guard predicate is satisfied. This class eagerly computes `this` expression.
	  * @see [[net.noresttherein.sugar.optional.extensions.providingMethods]]
	  */
	class satisfyingMethods[T](private val self :T) extends AnyVal {

		/** Return `Some(this)` if `condition` is true, `None`  otherwise. Note that `this` is eagerly evaluated. */
		@inline def satisfying(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition(this)` is true, `None` otherwise. Note that `this` is eagerly evaluated.  */
		@inline def satisfying(condition :T => Boolean) :Option[T] =
			if (condition(self)) Some(self) else None


		/** Return `this` if `condition` is true, or the alternative value otherwise. Don't evaluate the alternative
		  * if `condition` is true.  Note that `this` is eagerly evaluated.
		  */
		@inline def satisfyingOrElse(condition :Boolean)(alternative: => T) :T =
			if (condition) self else alternative

		/** Return `this` if `condition(this)` is true, or the alternative value otherwise. Don't evaluate the alternative
		  * if `condition` is true.  Note that `this` is eagerly evaluated.
		  */
		@inline def satisfyingOrElse(condition :T => Boolean)(alternative: => T) :T =
			if (condition(self)) self else alternative


		/** Return `Some(this)` if `condition` is false, `None` otherwise.  Note that `this` is eagerly evaluated. */
		@inline def violating(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)

		/** Return `Some(this)` if `condition(this)` is false, `None` otherwise.  Note that `this` is eagerly evaluated. */
		@inline def violating(condition :T => Boolean) :Option[T] =
			if (condition(self)) None else Some(self)


		/** Return `this` if `condition` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline def violatingOrElse(condition :Boolean)(alternative: => T) :T =
			if (condition) alternative else self

		/** Return `this` if `condition(this)` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline def violatingOrElse(condition :T => Boolean)(alternative: => T) :T =
			if (condition(self)) alternative else self
	}



	/** Extension methods evaluating and returning the value passed as `this` argument only if a given predicate is satisfied.
	  * @see [[net.noresttherein.sugar.optional.extensions.satisfyingMethods]]
	  */
	class providingMethods[T](self : => T)  {

		/** Return `Some(this)` if `condition` is true, or `None` without evaluating this expression otherwise. */
		@inline def providing(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None

		/** Return `Some(this)` if `condition` is false, or `None` without evaluating this expression otherwise. */
		@inline def unless(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)
	}



	////todo: use macros
	///** Extension method for any type performing a cast to a different type, guarded by the target type `ClassTag`. */
	//class IfInstanceOfMethods[T, +R](private val self :T) extends AnyVal {
	//	/** Returns `Some(this.asInstanceOf[X])` if `this.isInstanceOf[X]`. Note that this check is based on class tag
	//	  * only, which will ignore any type parameters that `X` might have.
	//	  * @return `classTag[X].unapply(this)`
	//	  */
	//	@inline def ifInstanceOf[X :ClassTag] :Option[X] = classTag[X].unapply(self)
	//
	//	/** Executes the given function for `this` value if it is an instance of type `X`. Note that this check
	//	  * ignores erasure and is thus purely class-based, as implemented by the implicit `ClassTag`.
	//	  * @return `classTag[X].unapply(this).map(f)`.
	//	  */
	//	@inline def ifInstanceOf[X :ClassTag, Y](f :X => Y) :Option[Y] = classTag[X].unapply(self).map(f)
	//
	//	/** Returns `Some(this.asInstanceOf[X])` if `this.isInstanceOf[X]`. Note that this check is based on class tag
	//	  * only, which will ignore any type parameters that `X` might have. This is essentially the same as
	//	  * `this.ifInstanceOf[X]`, but requires `X` to be verified as the subtype of the cast expression.
	//	  * This additional bound guards against casting to an unrelated type accidentally or as a result of a refactor.
	//	  * While of course `x :Any` and `X <: Any` are always true, this method isn't any more strict, but the type
	//	  * of this expression is typically inferred as the most specific non-singleton type, and the compiler will
	//	  * not implicitly convert `t :T` to `Any` in order to conform to the bound on this method's type parameter.
	//	  * @return `classTag[X].unapply(this)`
	//	  */
	//	@inline def ifSubclass[X <: T :ClassTag] :Option[X] = classTag[X].unapply(self)
	//
	//	/** Executes the given function for `this` value if it is an instance of type `X`
	//	  * . Note that this check is based on class tag only, which will ignore any type parameters that `X` might have.
	//	  * This is essentially the same as `this.ifInstanceOf[X]`, but requires `X` to be verified as the subtype
	//	  * of the cast expression. This additional bound guards against casting to an unrelated type accidentally
	//	  * or as a result of a refactor. While of course `x :Any` and `X <: Any` are always true, this method
	//	  * isn't any more strict, but the type of this expression is typically inferred as the most specific
	//	  * non-singleton type, and the compiler will not implicitly convert `t :T` to `Any` in order to conform
	//	  * to the bound on this method's type parameter.
	//	  * @return `classTag[X].unapply(this).map(f)`
	//	  */
	//	@inline def ifSubclass[X <: T :ClassTag, Y](f :X => Y) :Option[Y] = classTag[X].unapply(self).map(f)
	//}


	sealed trait OptionCompanionExtension extends Any {
		/** Returns the first argument in `Some` if it satisfies the predicate given as the second argument.
		  * @return `Some(value).filter(p)`.
		  */
		@inline final def satisfying[A](value :A)(p :A => Boolean) :Option[A] =
			if (p(value)) Some(value) else None
	}
}
