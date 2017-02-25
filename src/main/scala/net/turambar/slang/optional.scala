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

	class Nullable[+T >: Null](val orNull :T) extends AnyVal {
		@inline final def isEmpty: Boolean = orNull==null
		@inline final def nonEmpty: Boolean = orNull!=null
		@inline final def isDefined: Boolean = orNull!=null

		@inline final def get :T =
			if (orNull==null) throw new NoSuchElementException("Opt.Empty.get")
			else orNull

		@inline final def getOrElse[O>:T](alt : =>O) :O =
			if (orNull==null) alt else orNull

		@inline final def orElse[O>:T](alt : =>Nullable[O]) :Nullable[O] =
			if (orNull==null) alt else this

		@inline final def toOption = Option(orNull)

		@inline final def foreach[O](f :T=>O) :Unit = if (orNull!=null) f(orNull)
		@inline final def exists(p :T=>Boolean): Boolean = orNull!=null && p(orNull)
		@inline final def forall(p :T=>Boolean): Boolean = orNull==null || p(orNull)
		@inline final def contains[O>:T](o :O): Boolean = orNull!=null && orNull == o

		@inline final def filter(p :T=>Boolean) :Nullable[T] =
			if (orNull!=null && p(orNull)) this else new Nullable(null)

		@inline final def withFilter(p :T=>Boolean) :Nullable[T] =
			if (orNull!=null && p(orNull)) this else new Nullable(null)

		@inline final def map[O >: Null](p :T=>O) :Nullable[O] =
			if (orNull==null) new Nullable(null)
			else new Nullable(p(orNull))

		@inline final def flatMap[O>:Null](p :T=>Nullable[O]) :Nullable[O] =
			if (orNull==null) new Nullable(null)
			else p(orNull)

		@inline final def flatten[O>:Null](implicit isOpt :T<:<Nullable[O]) :Nullable[O] =
			if (orNull==null) new Nullable(null)
			else orNull

		@inline final def collect[O>:Null](f :PartialFunction[T, O]) :Nullable[O] =
			if (orNull!=null) new Nullable(f.applyOrElse(orNull, null))
			else new Nullable(null)

		@inline final def iterator :Iterator[T] =
			if (orNull==null) Iterator.single(orNull) else Iterator.empty

		@inline final def toList :List[T] = if (orNull==null) Nil else orNull::Nil
	}

	object Nullable {
		@inline final implicit def nullableToOption[T>:Null](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
		@inline final implicit def optionToNullable[T>:Null](opt :Option[T]) :Nullable[T] = new Nullable(opt.orNull)
		@inline final implicit def nullableToList[T>:Null](opt :Nullable[T]) :List[T] =
			if (opt.orNull==null) Nil else opt.orNull::Nil

		@inline final def apply[T>:Null](value :T) :Nullable[T] = new Nullable(value)

		object NotNull {
			@inline final def unapply[T>:Null](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
		}

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
	implicit class Conditional(private val condition :Boolean) extends AnyVal {
		/** If `this` boolean expression is true, return the given value. If it evaluates to `false`,
		  * return the value given to the returned object.
		  * @param ifTrue alternative value to return if this condition is true
		  * @tparam T type of this expression
		  * @return a control object providing `/=` method accepting the alternative to be returned if this condition is false.
		  */
		@inline def ?=[T](ifTrue : =>T) :OrElse[T] =
			if (condition) new OrElse(ifTrue)
			else new OrElse(null.asInstanceOf[T])

		@inline def ?!=[T](ifTrue :T) :OrElse[T] =
			if (condition) new OrElse(ifTrue)
			else new OrElse(null.asInstanceOf[T])

		@inline def ifTrue[T](thenThis : =>T) :OrElse[T] =
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
		@inline
		def satisfying(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition(this)` is true, `None` otherwise. */
		@inline
		def satisfying(condition :T=>Boolean) :Option[T] =
			if (condition(self)) Some(self) else None



		/** Return `this` if `condition` is true, or the alternative value otherwise. Don't evaluate the alternative if `condition` is true. */
		@inline def satisfyingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) self else alternative

		/** Return `this` if `condition(this)` is true, or the alternative value otherwise. Don't evaluate the alternative if `condition` is true. */
		@inline def satisfyingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) self else alternative




		/** Return `Some(this)` if `condition` is false, `None` otherwise. */
		@inline
		def dissatisfying(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)


		/** Return `Some(this)` if `condition(this)` is false, `None` otherwise. */
		@inline
		def dissatisfying(condition :T=>Boolean) :Option[T] =
			if (condition(self)) None else Some(self)


		/** Return `this` if `condition` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline
		def dissatisfyingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) alternative else self

		/** Return `this` if `condition(this)` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline
		def dissatisfyingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
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
		@inline
		def providing(condition :Boolean) :Option[T] =
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
