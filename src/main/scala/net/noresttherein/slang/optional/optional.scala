package net.noresttherein.slang

import scala.reflect.{classTag, ClassTag}


/** Extension methods providing new, more succinct and hopefully more readable syntax for creating 'Option' values
  * and conditionally guarded values in general.
  * @author Marcin Mościcki
  */
package object optional {


	type ?[+X] = Opt[X]


	/** Finds the first defined optional value in the given list. Equivalent to `alternatives.find(_.isDefined)`.
	  * It is particularly useful with the guarded expressions producing options, defined here:
	  * {{{
	  *     firstOf(<expression1> providing <guard1>, ... <expressionN> providing <guardN>)
	  * }}}
	  * @return first element for which `o.isDefined` or `None` if no such exists.
	  */
	def firstOf[V](alternatives :Option[V]*) :Option[V] =
		alternatives.find(_.isDefined).flatten


	/** Finds the first defined optional value satisfying the given condition.
	  * Equivalent to `alternatives.find(_.exists(condition)).flatten` but declares the intent upfront.
	  * @return first option among `alternatives` which element satisfies `condition` or `None` if no such exists.
	  */
	def firstWhere[V](alternatives :Option[V]*)(condition :V => Boolean) :Option[V] =
		alternatives.find(_.exists(condition)).flatten


	/** Collects all non-empty values in a `Seq`. While this is equivalent to simply `results.flatten`, it is
	  * useful, due to its variable arguments, in conjunction with `providing` or `satisfying` expression to
	  * find all values passing required condition:
	  * {{{
	  *     allOf(
	  *         <expression1> providing <guard1>,
	  *         ...
	  *         <expressionN> providing <guardN>
	  *     )
	  * }}}
	  * It also better conveys the intent than using simply `Seq(...).flatten`.
	  */
	@inline def allOf[V](results :Option[V]*) :Seq[V] = results.flatten






	/** Enriches an `Option[T]` with additional methods providing alternatives. */
	implicit class optionMethods[T](private val self :Option[T]) extends AnyVal {

		/** Applies the given function to the content of this option and returns the result or the provided alternative
		  * if this option is empty. Equivalent to `this map f getOrElse alternative`, but in one step.
		  */
		@inline def mapOrElse[X](f :T => X, alternative: => X) :X = self match {
			case Some(t) => f(t)
			case _ => alternative
		}


		/** Gets the element in the option or throws a `NoSuchElementException` with the given message. */
		@inline def getOrThrow(msg: => String) :T = self getOrElse {
			throw new NoSuchElementException(msg)
		}

		/** Gets the element in the option or throws an `IllegalArgumentException` with the given message. */
		@inline def illegalIfEmpty(msg: => String) :T = self getOrElse {
			throw new IllegalArgumentException(msg)
		}

		/** Gets the element in the option or throws the exception given as the type parameter with the given message.
		  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
		  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  */
		@inline def getOrThrow[E <: Exception :ClassTag](msg: => String) :T = self getOrElse raise[E](msg)


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
	}




	/** Creates an `Option[T]` based on the given value and a condition guard.
	  * @param condition condition which must hold for the value to be returned
	  * @param ifTrueThen default value to return if `condition` holds.
	  * @return `if (condition) Some(ifTrueThen) else None`
	  */
	@inline final def ifTrue[T](condition :Boolean)(ifTrueThen: => T) :Option[T] =
		if (condition) Some(ifTrueThen) else None


	/** Creates an `Option[T]` based on the given value and a condition guard.
	  * @param condition condition which must hold for the value to be returned
	  * @param ifFalseThen default value to return if `condition` doesn't hold.
	  * @return `if (condition) None else Some(ifFalseThen)`
	  */
	@inline final def ifFalse[T](condition :Boolean)(ifFalseThen: => T) :Option[T] =
		if (condition) None else Some(ifFalseThen)


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
	implicit class ifTrueMethods(private val condition :Boolean) extends AnyVal {

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




	/** An optional value returned as `Some` only if it passes the given condition.
	  * Same as `Some(value).filter(condition)`, but in a single step.
	  * @param value possible return value.
	  * @param condition predicate applied to `value`.
	  * @return `Some(value).filter(condition)`
	  */
	@inline final def satisfying[T](value :T)(condition :T => Boolean) :Option[T] =
		if (condition(value)) Some(value) else None


	/** Extension methods of an arbitrary value treating it as a default value to be lifted to an `Option`
	  * depending on whether a guard predicate is satisfied. This class eagerly computes `this` expression.
	  * @see [[net.noresttherein.slang.optional.providingMethods]]
	  */
	implicit class satisfyingMethods[T](private val self :T) extends AnyVal {

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
	  * @see [[net.noresttherein.slang.optional.satisfyingMethods]]
	  */
	implicit class providingMethods[T](self : => T)  {

		/** Return `Some(this)` if `condition` is true, or `None` without evaluating this expression otherwise. */
		@inline def providing(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None

		/** Return `Some(this)` if `condition` is false, or `None` without evaluating this expression otherwise. */
		@inline def unless(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)
	}




	//todo: use macros
	/** Extension method for any type performing a cast to a different type, guarded by the target type `ClassTag`. */
	implicit class ifInstanceOfMethods[T](private val self :T) extends AnyVal {
		/** Returns `Some(this.asInstanceOf[X])` if `this.isInstanceOf[X]`. Note that this check is based on class tag
		  * only, which will ignore any type parameters that `X` might have.
		  * @return `classTag[X].unapply(this)`
		  */
		@inline def ifInstanceOf[X :ClassTag] :Option[X] = classTag[X].unapply(self)

		@inline def ifInstanceOf[X :ClassTag, Y](f :X => Y) :Option[Y] = classTag[X].unapply(self).map(f)

		/** Returns `Some(this.asInstanceOf[X])` if `this.isInstanceOf[X]`. Note that this check is based on class tag
		  * only, which will ignore any type parameters that `X` might have. This is essentially the same as
		  * `this.ifInstanceOf[X]`, but requires `X` to be verified as the subtype of the cast expression.
		  * This additional bound guards against casting to an unrelated type accidentally or as a result of a refactor.
		  * While of course `x :Any` and `X <: Any` are always true, this method isn't any more strict, but the type
		  * of this expression is typically inferred as the most specific non-singleton type, and the compiler will
		  * not implicitly convert `t :T` to `Any` in order to conform to the bound on this method's type parameter.
		  * @return `classTag[X].unapply(this)`
		  */
		@inline def ifSubclass[X <: T :ClassTag] :Option[X] = classTag[X].unapply(self)

		@inline def ifSubclass[X <: T :ClassTag, Y](f :X => Y) :Option[Y] = classTag[X].unapply(self).map(f)
	}



	/** A class extending `scala.Some` with a `some` method as an alias to `get`.
	  * The purpose is to use it instead of the latter, as code refactoring can easily change the type of a value
	  * from `Some` to `Option` making `get` unsafe in places where it wasn't before.
	  */
	implicit class someMethod[T](private val opt :Some[T]) extends AnyVal {
		/** Returns the value of this option as returned by `get`.
		  * Using a separate method emphasises that this call is safe.
		  */
		@inline def some :T = opt.get
	}




	//todo: new method name and a macro to include the actual expression used as a string.
	/** Extension methods overloading `scala.ensuring`, allowing for passing an arbitrary exception to be thrown. */
	implicit class ensuringMethods[T](private val self :T) extends AnyVal {

		/** Return `this` if `condition` is true, or throw the give exception otherwise. */
		@inline def ensuring(condition :Boolean, ex :Exception) :T =
			if (condition) self else throw ex


		/** Return `this` if `condition` is false, or throw the given exception otherwise. */
		@inline def ensuring(condition :T => Boolean, ex :Exception) :T =
			if (condition(self)) self else throw ex


		/** Return `this` if `condition` is true, or raise the exception given as type parameter (must provide
		  * a default constructor). */
		@inline def ensuring[E <: Exception :ClassTag](condition :Boolean) :T =
			if (condition) self else raise[E]

		/** Return `this` if `condition` is true, or raise the exception specified as the type parameter,
		  * passing the given string to its constructor. */
		@inline def ensuring[E <: Exception :ClassTag](condition :Boolean, msg: => String) :T =
			if (condition) self else raise[E](msg)

	}


}
