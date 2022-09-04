 package net.noresttherein.sugar

import scala.reflect.ClassTag

import net.noresttherein.sugar.vars.Exam.{Failed, Passed}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/**
  * @define Exam [[net.noresttherein.sugar.vars.Exam! Exam]]
  * @define Passed [[net.noresttherein.sugar.vars.Exam.Passed Passed]]
  * @define Failed [[net.noresttherein.sugar.vars.Exam.Failed Failed]]
  */
package object vars {

	/** A variant of a non boxing `Either`, with instances of two categories: $Passed`[A]` containing a value of `A`,
	  * or $Failed with an error message. In order to avoid the creation of `Right` (successful) instance
	  * each time in a monadic composition of chained operations on an `Either`, a `Passed` is encoded
	  * as its erased (and boxed) contents, i.e. the value itself. A `Failed` corresponds to `Left[String]`.
	  * This solution brings two limitations:
	  *   1. Instances of `Exam` cannot be nested: an attempt to create a `Passed(exam)` if `exam` is a `Failed`
	  *      will result in an [[IllegalArgumentException]]. `Passed(Passed(value))` is however valid, and `Failed`
	  *      conforms to `Exam[Exam[Nothing]]`, so the non nesting rule must be enforced by the application.
	  *   2. No `Passed` (i.e. `Right`) type exist, because it is erased,
	  *      and construction and checking must happen using `apply` and `unapply` methods of singleton
	  *      object $Passed.
	  * [[net.noresttherein.sugar.vars.ExamExtension Extension]] methods are provided
	  */
	type Exam[+A] >: Failed

	implicit class ExamExtension[A](private val exam :Exam[A]) extends AnyVal {
		/** Checks if this $Exam is $Passed (successful) result containing a value. */
		@inline def isPassed :Boolean = !exam.isInstanceOf[Failed]
		/** Checks if this $Exam is $Failed containing an error message. */
		@inline def isFailed :Boolean = exam.isInstanceOf[Failed]


		/** Forces extraction of the result.
		  * @return contained value, if `this` is $Passed.
		  * @throws NoSuchElementException if this $Exam is $Failed. */
		def get :A = (exam :Any) match {
			case fail :Failed => throw new NoSuchElementException(fail.toString)
			case pass :Passed[A @unchecked] => pass.value
			case _ => exam.asInstanceOf[A]
		}
		/** Returns the result if it is $Passed, or the lazily computed alternative passed as an argument otherwise. */
		@inline def getOrElse[O >: A](alt: => O) :O = exam match {
			case _ :Failed => alt
			case _ => get
		}
		/** Similarly to [[net.noresttherein.sugar.vars.ExamExtension.getOrElse getOrElse]], returns the result
		  * of this $Exam if it is $Passed, or `alt` if it is $Failed. The difference is that the alternative value
		  * is not lazily computed and guarantees no closure will be created,
		  * at the cost of possibly discarding it without use.
		  * @param alt the value to return if this instance is a failure. */
		@inline def orDefault[O >: A](alt :O) :O = exam match {
			case _ :Failed => alt
			case _ => get
		}
		/** Assuming that `A` is a nullable type, return `null` if this $Exam is $Failed,
		  * or a wrapped result of $Passed otherwise. */
		@inline def orNull[O >: A](implicit isNullable :O <:< Null) :O = exam match {
			case _ :Failed => null.asInstanceOf[O]
			case _ => get
		}


		/** Returns the exam result if it is $Passed, or throws an exception given as the type parameter with
		  * the error message contained in $Failed.
		  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
		  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  * @see [[net.noresttherein.sugar.vars.ExamExtension.orNoSuch orNoSuch]]
		  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
		@inline def orThrow[E <: Throwable :ClassTag] :A = exam match {
			case fail :Failed => raise[E](fail.error)
			case _ => get
		}
		/** Gets the element in this $Exam if it is $Passed, or throws a [[NoSuchElementException]]
		  * with the error message contained in $Failed.
		  * @see [[net.noresttherein.sugar.vars.ExamExtension.orThrow orThrow]] */
		@inline def orNoSuch :A = exam match {
			case fail :Failed => throw new NoSuchElementException(fail.error)
			case _ => get
		}
		/** Gets the element in this $Exam if it is $Passed, or throws an [[IllegalArgumentException]]
		  * with the error message contained in $Failed.
		  * @see [[net.noresttherein.sugar.vars.ExamExtension.orThrow orThrow]] */
		@inline def orIllegal :A = exam match {
			case fail :Failed => throw new IllegalArgumentException(fail.error)
			case _ => get
		}
		/** Asserts that this instance is $Passed and returns its contents, throwing an [[AssertionError]]
		  * with the error message contained in $Failed otherwise. */
		@inline def orError :A = {
			assert(!exam.isInstanceOf[Failed], exam.asInstanceOf[Failed].error)
			get
		}

		/** Returns this $Exam if it is $Passed, or the lazily computed alternative otherwise. */
		@inline def orElse[O >: A](alt: => Exam[O]) :Exam[O] = exam match {
			case _ :Failed => alt
			case _ => exam
		}

		/** Similarly to [[net.noresttherein.sugar.vars.ExamExtension.orElse orElse]], returns this $Exam
		  * if it is $Passed, or `alt` otherwise. The difference is that the alternative value is not lazily computed
		  * and guarantees no closure would be be created, at the cost of possibly discarding it without use.
		  * @param alt the value to return if this instance is empty. */
		@inline def ifFailed[O >: A](alt: Exam[O]) :Exam[O] = exam match {
			case _ :Failed => alt
			case _ => exam
		}



		/** Returns $Passed with a the result of applying the given function to the result of this $Exam,
		  * or this instance ''iff'' it is $Failed. */
		@inline def map[O](f :A => O) :Exam[O] = exam match {
			case fail :Failed => fail
			case _ => Passed(f(get))
		}
		/** Applies the given function to the result of this $Exam if it is $Passed, or returns `alternative`
		  * if it is $Failed. Equivalent to `this map f getOrElse alternative`, but in one step. */
		@inline def mapOrElse[O](f :A => O, alternative: => O) :O = exam match {
			case _ :Failed => alternative
			case _ => f(get)
		}
		/** Returns the result of applying the given function to the value of this $Exam if it is $Passed,
		  * or `this` if it is $Failed. */
		@inline def flatMap[O](f :A => Exam[O]) :Exam[O] = exam match {
			case fail :Failed => fail
			case _ => f(get)
		}
		/** Flattens `Exam[Exam[O]]` to a single `Exam[O]`. */
		@inline def flatten[O](implicit isExam :A <:< Exam[O]) :Exam[O] =
			exam match {
				case fail :Failed => fail
				case _ => get
			}


		/** Tests if this $Exam is $Passed with a result equal to the given argument. */
		@inline def contains[O >: A](o :O): Boolean = o match {
			case _ :Failed => false
			case _ => get == o
		}
		/** Tests if this $Exam is $Passed with a result satisfying the given predicate. */
		@inline def exists(p :A => Boolean): Boolean = exam match {
			case _ :Failed => false
			case _ => p(get)
		}
		/** Tests if this $Exam is $Failed or $Passed with a value satisfying the given predicate. */
		@inline def forall(p :A => Boolean): Boolean = exam match {
			case _ :Failed => true
			case _ => p(get)
		}
		/** Executes the given block for this $Exam's result if it is $Passed. */
		@inline def foreach[O](f :A => O) :Unit = exam match {
			case _ :Failed =>
			case _ => f(get)
		}


		@inline def opt :Opt[A] = exam match {
			case _ :Failed => Lack
			case _ => Got(get)
		}

		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isPassed` or `None` otherwise. */
		@inline def ? :Option[A] = exam match {
			case _ :Failed => None
			case _ => Some(get)
		}

		/** Conversion to an `Unsure` carrying the same value as this instance, if any.
		  * Note that the result will not be `specialized` for value types, but neither will it require boxing,
		  * as $Passed already contains boxed values.
		  */
		@inline def unsure :Unsure[A] = exam match {
			case _ :Failed => Blank
			case _ => Sure(get)
		}

		/** Conversion to [[scala.Either]], with $Passed being returned as [[scala.Right Right]],
		  * while $Failed as [[scala.Left Left]] with its error message.
		  */
		@inline def toEither :Either[String, A] = exam match {
			case fail :Failed => Left(fail.error)
			case _ => Right(get)
		}

//		@inline def toOpt :Opt[A] = exam match {
//			case fail :Failed => Lack
//			case _ => Got(exam.asInstanceOf[A])
//		}
//
//		@inline def toOption :Option[A] = exam match {
//			case fail :Failed => None
//			case _ => Some(exam.asInstanceOf[A])
//		}
	}
}

 


 package vars {

	object Exam {
		/** Factory and matching pattern for [[net.noresttherein.sugar.vars.Exam! Exam]] instances
		  * representing a passed result (containing a value).
		  */
		object Passed {
			def apply[T](value :T) :Exam[T] = value match {
				case _ :Failed | _ :Passed[_] => new Passed(value).asInstanceOf[Exam[T]]
				case _ => value.asInstanceOf[Exam[T]]
			}

			def unapply[T](exam :Exam[T]) :Opt[T] = (exam :Any) match {
				case _ :Failed => Lack
				case pass :Passed[T @unchecked] => Got(pass.value)
				case _ => Got(exam.asInstanceOf[T])
			}
		}

		/** A reified `Passed` case used when `value` is a `Failed` or `Passed` instance.
		  * Used to differentiate between `Passed(Failed)` and `Failed`.
		  */
		@SerialVersionUID(1L)
		private[vars] class Passed[+T](val value :T) extends Serializable {
			override def equals(that :Any) :Boolean = that match {
				case other :Passed[_] => value == other.value
				case _ => false
			}
			override def hashCode :Int = value.hashCode
			override def toString :String = "Passed(" + value + ")"
		}

		/** Factory and matching pattern for [[net.noresttherein.sugar.vars.Exam! Exam]] instances
		  * representing a failed result (containing an error message).
		  */
		object Failed {
			def apply(error :String) :Exam[Nothing] = new Failed(error).asInstanceOf[Exam[Nothing]]

			def unapply(exam :Exam[Any]) :Opt[String] = exam match {
				case fail :Failed => Got(fail.error)
				case _ => Lack
			}
		}

		@SerialVersionUID(1L)
		final class Failed(val error :String) extends Serializable {
			override def equals(that :Any) :Boolean = that match {
				case fail :Failed => error == fail.error
				case _ => false
			}
			override def hashCode :Int = error.hashCode
			override def toString :String = "Failed(" + error + ")"
		}
	}
}
