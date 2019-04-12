package net.noresttherein.slang

import scala.annotation.tailrec
import scala.reflect.ClassTag


/** Extension methods providing new, hopefully more readable syntax for creating 'Option' values and conditionally
  * guarded values in general.
  * @author Marcin Mościcki
  */
package object optional {


	/** Finds first defined optional value in the given list. Equivalent to `alternatives.find(_.isDefined)`.
	  * It is particularly useful with the guarded expressions producing options, defined here:
	  * {{{
	  *     firstOf(<expression1> providing <guard1>, ... <expressionN> providing <guardN>)
	  * }}}
	  * @return first element for which `o.isDefined` or `None` if no such exists.
	  */
	def firstOf[V](alternatives :Option[V]*) :Option[V] = alternatives match {
		case list :List[Option[V]] =>
			@tailrec def get(l :List[Option[V]]) :Option[V] =
				if (l.isEmpty) None
				else {
					val h = l.head
					if (h.isDefined) h else get(l.tail)
				}
			get(list)
		case _ =>
			alternatives.find(_.isDefined).flatten
	}


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
	def allOf[V](results :Option[V]*) :Seq[V] = results.flatten


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
	implicit class IfTrue(private val condition :Boolean) extends AnyVal {

		/** If `this` boolean expression is true, return the given value in an `Option`. If it evaluates to `false`,
		  * return `None`.
		  * @param thenThis alternative value to return wrapped in `Option` if this condition is true.
		  * @tparam T value type of this expression
		  * @return `Some(thenThis)` if this condition is true and `thenThis` is not null or `None` otherwise.
		  */
		@inline def ifTrue[T](thenThis : => T) :Option[T] =
			if (condition) Option(thenThis)
			else None


		/** If `this` boolean expression is false, return the given value in an `Option`. If it evaluates to `true`,
		  * return `None`.
		  * @param thenThis alternative value to return wrapped in `Option` if this condition is false.
		  * @tparam T value type of this expression
		  * @return `Some(thenThis)` if this condition is true and `thenThis` is not null or `None` otherwise.
		  */
		@inline def ifFalse[T](thenThis : => T) :Option[T] =
			if (condition) None else Option(thenThis)


	}


	/** Creates an `Option[T]` based on the given value and a condition guard.
	  * @param condition condition which must hold for the value to be returned
	  * @param ifTrueThen default value to return if `condition` holds.
	  * @return `if (condition) Some(ifTrueThen) else None`
	  */
	@inline final def ifTrue[T](condition :Boolean)(ifTrueThen : =>T) :Option[T] =
		if (condition) Some(ifTrueThen) else None


	/** Creates an `Option[T]` based on the given value and a condition guard.
	  * @param condition condition which must hold for the value to be returned
	  * @param ifFalseThen default value to return if `condition` doesn't hold.
	  * @return `if (condition) None else Some(ifFalseThen)`
	  */
	@inline final def ifFalse[T](condition :Boolean)(ifFalseThen : =>T) :Option[T] =
		if (condition) None else Some(ifFalseThen)




	/** An optional value returned as `Some` only if it passes the given condition.
	  * Same as `Some(value).filter(condition)`, but in a single step.
	  * @param value possible return value.
	  * @param condition predicate applied to `value`.
	  * @return `Some(value).filter(condition)`
	  */
	@inline final def satisfying[T](value :T)(condition :T=>Boolean) :Option[T] =
		if (condition(value)) Some(value) else None


	/** Extension methods of an arbitrary value treating it as a default value to be lifted to an `Option`
	  * depending on whether a guard predicate is satisfied. This class eagerly computes `this` expression.
	  * @see [[net.noresttherein.slang.optional.Providing]]
	  */
	implicit class Satisfying[T](private val self :T) extends AnyVal {

		/** Return `Some(this)` if `condition` is true, `None`  otherwise. Note that `this` is eagerly evaluated. */
		@inline def satisfying(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition(this)` is true, `None` otherwise. Note that `this` is eagerly evaluated.  */
		@inline def satisfying(condition :T=>Boolean) :Option[T] =
			if (condition(self)) Some(self) else None



		/** Return `this` if `condition` is true, or the alternative value otherwise. Don't evaluate the alternative
		  * if `condition` is true.  Note that `this` is eagerly evaluated.
		  */
		@inline def satisfyingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) self else alternative

		/** Return `this` if `condition(this)` is true, or the alternative value otherwise. Don't evaluate the alternative
		  * if `condition` is true.  Note that `this` is eagerly evaluated.
		  */
		@inline def satisfyingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) self else alternative




		/** Return `Some(this)` if `condition` is false, `None` otherwise.  Note that `this` is eagerly evaluated. */
		@inline def dissatisfying(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)


		/** Return `Some(this)` if `condition(this)` is false, `None` otherwise.  Note that `this` is eagerly evaluated. */
		@inline def dissatisfying(condition :T=>Boolean) :Option[T] =
			if (condition(self)) None else Some(self)


		/** Return `this` if `condition` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline def dissatisfyingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) alternative else self

		/** Return `this` if `condition(this)` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline def dissatisfyingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) alternative else self


	}






	/** Extension methods evaluating and returning the value passed as `this` argument only if a given predicate is satisfied.
	  * @see [[net.noresttherein.slang.optional.Providing]]
	  */
	implicit class Providing[T](self : =>T)  {

		/** Return `Some(this)` if `condition` is true, or `None` without evaluating this expression otherwise. */
		@inline def providing(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition` is false, or `None` without evaluating this expression otherwise. */
		@inline def unless(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)

	}



	/** Extension methods overloading `scala.ensuring`, allowing for passing an arbitrary exception to be thrown. */
	implicit class CustomEnsuring[T](private val self :T) extends AnyVal {

		/** Return `this` if `condition` is true, or throw the give exception otherwise. */
		@inline
		def ensuring(condition :Boolean, ex :Exception) :T =
			if (condition) self else throw ex


		/** Return `this` if `condition` is false, or throw the given exception otherwise. */
		@inline
		def ensuring(condition :T=>Boolean, ex :Exception) :T =
			if (condition(self)) self else throw ex


		/** Return `this` if `condition` is true, or raise the exception given as type parameter (must provide a default constructor). */
		@inline
		def ensuring[E<:Exception :ClassTag](condition :Boolean) :T =
			if (condition) self else raise[E]

		/** Return `this` if `condition` is true, or raise the exception specified as the type parameter, passing the given string to its constructor. */
		@inline
		def ensuring[E<:Exception :ClassTag](condition :Boolean, msg : =>String) :T =
			if (condition) self else raise[E](msg)


	}


}
