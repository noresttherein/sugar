package net.noresttherein.sugar.optional




/** Factory methods providing new, more succinct and hopefully more readable syntax for creating 'Option' values
  * and conditionally guarded values in general.
  * @author Marcin Mościcki
  */
trait imports {

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


	/** An optional value returned as `Some` only if it passes the given condition.
	  * Same as `Some(value).filter(condition)`, but in a single step.
	  * @param value possible return value.
	  * @param condition predicate applied to `value`.
	  * @return `Some(value).filter(condition)`
	  */
	@inline final def satisfying[T](value :T)(condition :T => Boolean) :Option[T] =
		if (condition(value)) Some(value) else None

}
