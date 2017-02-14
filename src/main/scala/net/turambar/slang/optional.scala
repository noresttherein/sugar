package net.turambar.slang

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

/** Extension methods for implementing inverted conditional expressions, where a default value is followed by a boolean
  * guard specifying when it should or shouldn't be returned.
  * @author Marcin Mościcki
  */
object optional {

	def providing[T](condition :Boolean)(expr : => T) :Option[T] =
		if (condition) Some(expr)
		else None

	def onlyIf[T](condition :Boolean)(expr : =>Option[T]) :Option[T] =
		if (condition) expr
		else None


	/** Extension methods of an arbitrary value treating it as a default value to be lifted to an `Option`
	  * depending on whether a guard predicate is satisfied. This class eagerly computes `this` expression.
	  */
	implicit class ProvidingExpression[T](private val self :T) extends AnyVal {

		/** Return `Some(this)` if `condition` is true, `None`  otherwise. */
		@inline
		def providing(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition(this)` is true, `None` otherwise. */
		@inline
		def providing(condition :T=>Boolean) :Option[T] =
			if (condition(self)) Some(self) else None



		/** Return `this` if `condition` is true, or the alternative value otherwise. Don't evaluate the alternative if `condition` is true. */
		@inline def providingOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) self else alternative

		/** Return `this` if `condition(this)` is true, or the alternative value otherwise. Don't evaluate the alternative if `condition` is true. */
		@inline def providingOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) self else alternative




		/** Return `Some(this)` if `condition` is false, `None` otherwise. */
		@inline
		def unless(condition :Boolean) :Option[T] =
			if (condition) None else Some(self)


		/** Return `Some(this)` if `condition(this)` is false, `None` otherwise. */
		@inline
		def unless(condition :T=>Boolean) :Option[T] =
			if (condition(self)) None else Some(self)


		/** Return `this` if `condition` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline
		def unlessOrElse(condition :Boolean)(alternative : =>T) :T =
			if (condition) alternative else self

		/** Return `this` if `condition(this)` is false or the alternative value otherwise. Don't evaluate the alternative if `condition` is false. */
		@inline
		def unlessOrElse(condition :T=>Boolean)(alternative : =>T) :T =
			if (condition(self)) alternative else self





	}




	/** Extension methods evaluating and returning the value passed as `this` argument only if a given predicate is satisfied. */
	implicit class LazyOptionalExpression[T](self : =>T)  {

		/** Return `Some(this)` if `condition` is true, or `None` and don't evaluate this expression otherwise. */
		@inline
		def onlyIf(condition :Boolean) :Option[T] =
			if (condition) Some(self) else None


		/** Return `Some(this)` if `condition` is false, or `None` without evaluating this expression otherwise. */
		@inline def onlyIfNot(condition :Boolean) :Option[T] =
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



		private def raise[E<:Exception :ClassTag] :Nothing =
			throw (Try {
				classTag[E].runtimeClass.newInstance().asInstanceOf[Exception]
			} recover {
				case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure on $self", ex)
			}).get


		private def raise[E<:Exception :ClassTag](msg :String) :Nothing =
			throw (Try {
				classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Exception]
			} recover {
				case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure on $self", ex)
			}).get

	}
}
