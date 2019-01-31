package net.turambar.slang

import net.turambar.slang.optional.Conditional.OrElse

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}
import scala.util.Try

/** Extension methods for implementing inverted conditional expressions, where a default value is followed by a boolean
  * guard specifying when it should or shouldn't be returned.
  * @author Marcin Mościcki
  */
object optional {


	/** A value class treating nullable reference types as option-like monads erased in runtime. */
	final class Nullable[+T >: Null](/** A wrapped reference value which may be null. */val orNull :T) extends AnyVal {
		/** Tests if this value is null representing 'no value'. */
		@inline def isEmpty: Boolean = orNull==null

		/** Tests if this value is not null. */
		@inline def nonEmpty: Boolean = orNull!=null

		/** Tests if this value is not null. */
		@inline def isDefined: Boolean = orNull!=null

		/** Tests if this instance is `null`, Same as `isEmpty`. */
		@inline def isNull: Boolean = orNull==null
		
		/** Forces extraction of the value.
		  * @return contained value, if it not null
		  * @throws NoSuchElementException if this value is `null`.
		  */
		@inline def get :T =
			if (orNull==null) throw new NoSuchElementException("Nullable.Empty.get")
			else orNull

		/** Returns this value if it is not null or the lazily computed alternative passed as the argument in the opposite case. */
		@inline def getOrElse[O>:T](alt : =>O) :O =
			if (orNull==null) alt else orNull

		/** Returns this `Nullable` if it is not null or the lazily computed alternative for null values. */
		@inline def orElse[O>:T](alt : =>Nullable[O]) :Nullable[O] =
			if (orNull==null) alt else this

		/** Similarly to [[getOrElse]], returns the value if non-null and `alt` otherwise. The difference is that the
		  * alternative value is not lazily computed and avoids creating of a closure at the cost of possibly discarding
		  * it without use.
		  * @param alt the value to return if this instance is `null`.
		  */
		@inline def ifNull[O >: T](alt: O) :O =
			if (orNull==null) alt else orNull



		/** Wraps the value in an option.
		  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
		  */
		@inline def toOption = Option(orNull)

		/** Executes the given block for this value if it is not null. */
		@inline def foreach[O](f :T=>O) :Unit = if (orNull!=null) f(orNull)

		/** Tests if this value is not null and satisfies the given predicate. */
		@inline def exists(p :T=>Boolean): Boolean = orNull!=null && p(orNull)

		/** Tests if this value is null or satisfies the given predicate. */
		@inline def forall(p :T=>Boolean): Boolean = orNull==null || p(orNull)

		/** Tests if this value is not null and equal to the given argument. */
		@inline def contains[O>:T](o :O): Boolean = orNull!=null && orNull == o

		/** Returns a new `Nullable` containing this value if it is not null and satisfies the given predicate,
		  * or a null value otherwise.
		  */
		@inline def filter(p :T=>Boolean) :Nullable[T] =
			if (orNull!=null && p(orNull)) this else new Nullable(null)

		/** Equivalent to `this.`[[Nullable#filter]]`(p)` - a variant for use in for-comprehensions. */
		@inline def withFilter(p :T=>Boolean) :Nullable[T] =
			if (orNull!=null && p(orNull)) this else new Nullable(null)

		/** Returns a new `Nullable` which is empty if this value is null or contains the result of applying
		  * the given function to it otherwise.
		  */
		@inline def map[O >: Null](p :T=>O) :Nullable[O] =
			if (orNull==null) new Nullable(null)
			else new Nullable(p(orNull))

		/** Returns the result of applying the given function to this value if it is not null or
		  * `this` if `this.isEmpty`
		  */
		@inline def flatMap[O>:Null](p :T=>Nullable[O]) :Nullable[O] =
			if (orNull==null) new Nullable(null)
			else p(orNull)

		/** Flattens `Nullable[Nullable[O]]` to a single `Nullable[O]`. */
		@inline def flatten[O>:Null](implicit isOpt :T<:<Nullable[O]) :Nullable[O] =
			if (orNull==null) new Nullable(null)
			else orNull

		/** Returns an empty `Nullable` if this value is null or `f` is not defined for it, otherwise
		  * applying the given partial function and wrapping it in a new `Nullable`.
		  */
		@inline def collect[O>:Null](f :PartialFunction[T, O]) :Nullable[O] =
			if (orNull!=null) new Nullable(f.applyOrElse(orNull, null))
			else new Nullable(null)

		/** An iterator returning this value as the only element if `this.nonEmpty`. */
		@inline def iterator :Iterator[T] =
			if (orNull==null) Iterator.single(orNull) else Iterator.empty

		/** Returns `Nil` if this value is null or or `this.get::Nil` otherwise. */
		@inline def toList :List[T] = if (orNull==null) Nil else orNull::Nil
	}


	/** Companion object providing factory methods and extractors working with [[Nullable]]s. */
	object Nullable {
		@inline final implicit def nullableToOption[T>:Null](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
		@inline final implicit def optionToNullable[T>:Null](opt :Option[T]) :Nullable[T] = new Nullable(opt.orNull)

		object ExtraImplicits {
			@inline final implicit def nullableToList[T >: Null](opt :Nullable[T]) :List[T] =
				if (opt.orNull == null) Nil else opt.orNull :: Nil
		}

		/** Wraps the given reference in a purely syntactic option-like object erased in the runtime. */
		@inline final def apply[T>:Null](value :T) :Nullable[T] = new Nullable(value)


		/** Extractor for non-null values of [[Nullable]] to be used in pattern matching. */
		object NotNull {
			@inline final def unapply[T>:Null](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
		}

		/** A `null` value representing an empty [[Nullable]] for any true reference type. */
		final val Empty :Nullable[Null] = new Nullable(null)
	}



	/** Finds first defined optional value in the given list.
	  * Equivalent to `alternatives.find(_.isDefined).flatten`.
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






	/** An `if-then-else` expression mimicking java's conditional expression ''condition'' `?` ''ifTrue'' `:` ''ifFalse''.
	  * Extends implicitly `Boolean` values providing a `?=` method which accepts the 'then' clause of the expression
	  * and returns a simple control object awaiting the 'or else' expression for its `/=` operator.
	  * Thanks to the naming convention same as for assignment operators, both these operators have the lowest precedence
	  * possible and are left-associative, making parentheses generally unnecessary.
	  * @param condition boolean value switching between two alternative values returned by the whole expression.
	  */
	@deprecated("just use if-else", "1.0")
	implicit class Conditional(private val condition :Boolean) extends AnyVal {

		/** If `this` boolean expression is true, return the given value. If it evaluates to `false`,
		  * return the value given to the returned object.
		  * @param ifTrue alternative value to return if this condition is true
		  * @tparam T type of this expression
		  * @return a control object providing `/=` method accepting the alternative to be returned if this condition is false.
		  */
		@inline def ?=[T](ifTrue : => T) :OrElse[T] =
			if (condition) new OrElse(ifTrue)
			else new OrElse(null.asInstanceOf[T])

		@inline def ?!=[T](ifTrue :T) :OrElse[T] =
			if (condition) new OrElse(ifTrue)
			else new OrElse(null.asInstanceOf[T])

		@inline def ifTrue[T](thenThis : => T) :OrElse[T] =
			if (condition) new OrElse(thenThis)
			else new OrElse(null.asInstanceOf[T])

	}



	object Conditional {

		/** Light wrapper accepting the 'if false' value of the conditional expression created by [[Conditional]]'s `?=` operator. */
		class OrElse[T] private[Conditional] (private val ifTrue :T) extends AnyVal {

			@inline def /=(ifFalse : =>T) :T = if (ifTrue!=null) ifTrue else ifFalse

			@inline def /!=(ifFalse :T) :T = if (ifTrue!=null) ifTrue else ifFalse

			@inline def orElse(ifFalse: =>T) :T = if (ifTrue!=null) ifTrue else ifFalse
		}
	}




	/** An optional value returned as `Some` only if it passes the given condition.
	  * Same as `Some(value).filter(condition)`, but in a single step.
	  * @param value possible return value
	  * @param condition predicate applied to `value`.
	  * @return `Some(value).filter(condition)`
	  */
	@inline final def satisfying[T](value :T)(condition :T=>Boolean) :Option[T] =
		if (condition(value)) Some(value) else None


	/** Extension methods of an arbitrary value treating it as a default value to be lifted to an `Option`
	  * depending on whether a guard predicate is satisfied. This class eagerly computes `this` expression.
	  */
	implicit class Satisfying[T](private val self :T) extends AnyVal {

		/** Return `Some(this)` if `condition` is true, `None`  otherwise. */
		@inline def satisfying(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition(this)` is true, `None` otherwise. */
		@inline def satisfying(condition :T=>Boolean) :Option[T] =
			if (condition(self)) Some(self) else None



		/** Return `this` if `condition` is true, or the alternative value otherwise. Don't evaluate the alternative if `condition` is true. */
		@inline def satisfyingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) self else alternative

		/** Return `this` if `condition(this)` is true, or the alternative value otherwise. Don't evaluate the alternative if `condition` is true. */
		@inline def satisfyingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) self else alternative




		/** Return `Some(this)` if `condition` is false, `None` otherwise. */
		@inline def dissatisfying(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)


		/** Return `Some(this)` if `condition(this)` is false, `None` otherwise. */
		@inline def dissatisfying(condition :T=>Boolean) :Option[T] =
			if (condition(self)) None else Some(self)


		/** Return `this` if `condition` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline def dissatisfyingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) alternative else self

		/** Return `this` if `condition(this)` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline def dissatisfyingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) alternative else self





	}

	/** Creates an `Option[T]` based on the given value and a condition guard.
	  * @param condition condition which must hold for the value to be returned
	  * @param ifTrueThen default value to return if `condition` holds.
	  * @return `if (condition) Some(ifTrueThen) else None`
	  */
	@inline final def providing[T](condition :Boolean)(ifTrueThen : =>T) :Option[T] =
		if (condition) Some(ifTrueThen) else None

	/** Creates an `Option[T]` based on the given value and a condition guard.
	  * @param condition condition which must hold for the value to be returned
	  * @param ifFalseThen default value to return if `condition` doesn't hold.
	  * @return `if (condition) None else Some(ifFalseThen)`
	  */
	@inline final def unless[T](condition :Boolean)(ifFalseThen : =>T) :Option[T] =
		if (condition) None else Some(ifFalseThen)


	/** Extension methods evaluating and returning the value passed as `this` argument only if a given predicate is satisfied. */
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

		/** Return `this` if `condition` is true, or throw the givne exception otherwise. */
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
