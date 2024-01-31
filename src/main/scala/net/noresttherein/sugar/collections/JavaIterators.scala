package net.noresttherein.sugar.collections

import java.util.Spliterator

import scala.annotation.{nowarn, tailrec}
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}
import scala.collection.View

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JLong, JShort}
import net.noresttherein.sugar.extensions.{BooleanExtension, castTypeParamMethods}
import net.noresttherein.sugar.noSuch_!




@SerialVersionUID(Ver)
object JavaIterator {
	final val Types :Specializable.Group[(Int, Long, Double)] = null

	def from[T, I <: JavaIterator[_]](elems :IterableOnce[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		IteratorStepper(elems)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def over[T, I <: JavaIterator[_]](seq :collection.IndexedSeq[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		IndexedSeqStepper(seq)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def over[T, I <: JavaIterator[_]](array :Array[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		ArrayStepper(array)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: JavaIterator[_]](seq :collection.IndexedSeq[T], from :Int, until :Int)
	                               (implicit shape :JavaIteratorShape[T, I]) :I =
		IndexedSeqStepper(seq, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: JavaIterator[_]](array :Array[T], from :Int, until :Int)
	                                  (implicit shape :JavaIteratorShape[T, I]) :I =
		ArrayStepper(array, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

	//Using I without an upper bound of JavaIterator[_] saves us casting the argument in some places,
	// which could create another closure.
	def delay[T, I](iterator: => I)(implicit shape :JavaIteratorShape[T, I]) :I =
		(shape.shape : @nowarn) match {
			case ReferenceShape =>
				new JavaIterator[T] {
					private lazy val iter = iterator.asInstanceOf[JavaIterator[T]]
					override def hasNext = iter.hasNext
					override def next() = iter.next()
				}.asInstanceOf[I]
			case IntShape | CharShape | ByteShape | ShortShape =>
				new JavaIntIterator {
					private lazy val iter = iterator.asInstanceOf[JavaIntIterator]
					override def hasNext = iter.hasNext
					override def nextInt() = iter.nextInt()
				}.asInstanceOf[I]
			case LongShape =>
				new JavaLongIterator {
					private lazy val iter = iterator.asInstanceOf[JavaLongIterator]
					override def hasNext = iter.hasNext
					override def nextLong() = iter.nextLong()
				}.asInstanceOf[I]
			case DoubleShape | FloatShape =>
				new JavaDoubleIterator {
					private lazy val iter = iterator.asInstanceOf[JavaDoubleIterator]
					override def hasNext = iter.hasNext
					override def nextDouble() = iter.nextDouble()
				}.asInstanceOf[I]
		}


	def empty[T] :JavaIterator[T] = emptyPrototype.asInstanceOf[JavaIterator[T]]

	/** Returns an object with an `apply()` function returning a Java iterator type specific to type `T`.
	  * This degree of separation allows the iterator type to be inferred based on an implicit
	  * [[net.noresttherein.sugar.collections.JavaIteratorShape JavaIteratorShape]]
	  * based on an explicitly provided type `T`: `JavaIterator[Int]()` will return a `PrimitiveIterator.OfInt`
	  */
	def apply[T] :EmptyJavaIteratorFactory[T] = new EmptyJavaIteratorFactory[T] {}

	sealed trait EmptyJavaIteratorFactory[T] extends Any {
		final def apply[I <: JavaIterator[_]]()(implicit shape :JavaIteratorShape[T, I]) :I =
			shape.shape match {
				case LongShape => ofLong().asInstanceOf[I]
				case DoubleShape | FloatShape => ofDouble().asInstanceOf[I]
				case IntShape | CharShape | ShortShape | ByteShape => ofInt().asInstanceOf[I]
				case _ => ofAny().asInstanceOf[I]
			}
	}

	def one[T, I <: JavaIterator[_]](elem :T)(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case IntShape | CharShape | ByteShape | ShortShape => ofInt(elem.asInstanceOf[Int]).asInstanceOf[I]
			case LongShape   => ofLong(elem.asInstanceOf[Long]).asInstanceOf[I]
			case DoubleShape | FloatShape => ofDouble(elem.asInstanceOf[Double]).asInstanceOf[I]
			case _ => ofAny(elem).asInstanceOf[I]
		}

	def two[T, I <: JavaIterator[_]](first :T, second :T)(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case IntShape | CharShape | ByteShape | ShortShape =>
				ofInt(first.asInstanceOf[Int], second.asInstanceOf[Int]).asInstanceOf[I]
			case LongShape   =>
				ofLong(first.asInstanceOf[Long], second.asInstanceOf[Long]).asInstanceOf[I]
			case DoubleShape | FloatShape =>
				ofDouble(first.asInstanceOf[Double], second.asInstanceOf[Double]).asInstanceOf[I]
			case _ => ofAny(first, second).asInstanceOf[I]
		}


	def ofAny[T]() :JavaIterator[T] = emptyPrototype.asInstanceOf[JavaIterator[T]]

	def ofAny[T](elem :T) :JavaIterator[T] = new JavaIterator[T] {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def next() :T =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				empty[T].next()
		override def toString = if (nonEmpty) "JavaIterator(" + elem + ")" else "JavaIterator()"
	}
	def ofAny[T](first :T, second :T) :JavaIterator[T] = new JavaIterator[T] {
		private[this] var left = 2
		override def hasNext = left > 0
		override def next() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => empty[T].next()
		}
		override def toString = left match {
			case 2 => "JavaIterator(" + first + ", " + second + ")"
			case 1 => "JavaIterator(" + second + ")"
			case _ => "JavaIterator()"
		}
	}
	def ofAny[T](seq :IndexedSeq[T]) :JavaIterator[T] = slice(seq, 0, seq.length)
	def ofAny[T](seq :IndexedSeq[T], from :Int, until :Int) :JavaIterator[T] = slice(seq, from, until)
//	def ofAny[T](array :Array[T]) :JavaIterator[T] = slice(array, 0, array.length)
	def ofAny[T](array :Array[T], from :Int = 0, until :Int = Int.MaxValue) :JavaIterator[T] = slice(array, from, until)

	def ofInt() :JavaIntIterator = emptyInt

	def ofInt(elem :Int) :JavaIntIterator = new JavaIntIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextInt() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty[JInt].next()
		override def toString =
			if (nonEmpty) "JavaIntIterator(" + elem + ")" else "JavaIntIterator()"
	}
	def ofInt(first :Int, second :Int) :JavaIntIterator = new JavaIntIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextInt() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty[JInt].next()
		}
		override def toString :String = left match {
			case 2 => "JavaIntIterator(" + first + ", " + second + ")"
			case 1 => "JavaIntIterator(" + second + ")"
			case _ => "JavaIntIterator()"
		}
	}
	def ofInt(seq :IndexedSeq[Int]) :JavaIntIterator = slice(seq, 0, seq.length)
	def ofInt(seq :IndexedSeq[Int], from :Int, until :Int) :JavaIntIterator = slice(seq, from, until)
//	def ofInt(array :Array[Int]) :JavaIntIterator = slice(array, 0, array.length)
	def ofInt(array :Array[Int], from :Int = 0, until :Int = Int.MaxValue) :JavaIntIterator = slice(array, from, until)
	def ofInt(string :String) :JavaIntIterator = ofInt(string, 0, string.length)

	@throws[IndexOutOfBoundsException]("if from < 0 or until > string.length.")
	def ofInt(string :String, from :Int, until :Int) :JavaIntIterator =
		new StringStepper(string, from, until)

	def ofLong() :JavaLongIterator = emptyLong

	def ofLong(elem :Long) :JavaLongIterator = new JavaLongIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextLong() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty[JLong].next()
		override def toString =
			if (nonEmpty) "JavaLongIterator(" + elem + ")" else "JavaLongIterator"
	}
	def ofLong(first :Long, second :Long) :JavaLongIterator = new JavaLongIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextLong() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty[JLong].next()
		}
		override def toString = left match {
			case 2 => "JavaLongIterator(" + first + ", " + second + ")"
			case 1 => "JavaLongIterator(" + second + ")"
			case _ => "JavaLongIterator()"
		}
	}
	def ofLong(seq :IndexedSeq[Long]) :JavaLongIterator = slice(seq, 0, seq.length)
	def ofLong(seq :IndexedSeq[Long], from :Int, until :Int) :JavaLongIterator = slice(seq, from, until)
//	def ofLong(array :Array[Long]) :JavaLongIterator = slice(array, 0, array.length)
	def ofLong(array :Array[Long], from :Int = 0, until :Int = Int.MaxValue) :JavaLongIterator = slice(array, from, until)

	def ofDouble() :JavaDoubleIterator = emptyDouble

	def ofDouble(elem :Double) :JavaDoubleIterator = new JavaDoubleIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextDouble() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty.next()
		override def toString =
			if (nonEmpty) "JavaDoubleIterator(" + elem + ")" else "JavaDoubleIterator()"
	}

	def ofDouble(first :Double, second :Double) :JavaDoubleIterator = new JavaDoubleIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextDouble() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty.next()
		}
		override def toString = left match {
			case 2 => "JavaDoubleIterator(" + first + ", " + second + ")"
			case 1 => "JavaDoubleIterator(" + second + ")"
			case _ => "JavaDoubleIterator()"
		}
	}
	def ofDouble(seq :IndexedSeq[Double]) :JavaDoubleIterator = slice(seq, 0, seq.length)
	def ofDouble(seq :IndexedSeq[Double], from :Int, until :Int) :JavaDoubleIterator = slice(seq, from, until)
//	def ofDouble(array :Array[Double]) :JavaDoubleIterator = slice(array, 0, array.length)
	def ofDouble(array :Array[Double], from :Int = 0, until :Int = Int.MaxValue) :JavaDoubleIterator =
		slice(array, from, until)


	private[this] val emptyInt :JavaIntIterator = new JavaIntIterator {
		override def nextInt() = JavaIterator.empty.next()
		override def hasNext = false
		override def toString = "JavaIntIterator()"
	}
	private[this] val emptyLong :JavaLongIterator = new JavaLongIterator {
		override def nextLong() = JavaIterator.empty.next()
		override def hasNext = false
		override def toString = "JavaLongIterator()"
	}
	private[this] val emptyDouble :JavaDoubleIterator = new JavaDoubleIterator {
		override def nextDouble() = JavaIterator.empty.next()
		override def hasNext = false
		override def toString = "JavaDoubleIterator()"
	}
	private[this] val emptyPrototype = new JavaIterator[Nothing] {
		override def hasNext = false
		override def next() = noSuch_!("Empty Iterator")
		override def toString = "JavaIterator()"
	}

	def apply[E](spliterator :Spliterator[E]) :JavaIterator[E] = SpliteratorStepper.ofRef(spliterator).javaIterator
	def apply[E](spliterator :Spliterator.OfInt) :JavaIntIterator = SpliteratorStepper.ofInt(spliterator).javaIterator
	def apply[E](spliterator :Spliterator.OfLong) :JavaLongIterator = SpliteratorStepper.ofLong(spliterator).javaIterator
	def apply[E](spliterator :Spliterator.OfDouble) :JavaDoubleIterator =
		SpliteratorStepper.ofDouble(spliterator).javaIterator


	object conversions {
		@inline implicit def intIteratorToJavaIterator(i :JavaIntIterator) :JavaIterator[Int] =
			i.asInstanceOf[JavaIterator[Int]]

		@inline implicit def longIteratorToJavaIterator(i :JavaLongIterator) :JavaIterator[Long] =
			i.asInstanceOf[JavaIterator[Long]]

		@inline implicit def doubleIteratorToJavaIterator(i :JavaDoubleIterator) :JavaIterator[Double] =
			i.asInstanceOf[JavaIterator[Double]]


		@inline implicit def boxedByteJavaIteratorToJavaIterator(i :JavaIterator[JByte]) :JavaIterator[Byte] =
			i.asInstanceOf[JavaIterator[Byte]]

		@inline implicit def boxedShortJavaIteratorToJavaIterator(i :JavaIterator[JShort]) :JavaIterator[Short] =
			i.asInstanceOf[JavaIterator[Short]]

		@inline implicit def boxedCharJavaIteratorToJavaIterator(i :JavaIterator[JChar]) :JavaIterator[Char] =
			i.asInstanceOf[JavaIterator[Char]]

		@inline implicit def boxedIntJavaIteratorToJavaIterator(i :JavaIterator[JInt]) :JavaIterator[Int] =
			i.asInstanceOf[JavaIterator[Int]]

		@inline implicit def boxedLongJavaIteratorToJavaIterator(i :JavaIterator[JLong]) :JavaIterator[Long] =
			i.asInstanceOf[JavaIterator[Long]]

		@inline implicit def boxedFloatJavaIteratorToJavaIterator(i :JavaIterator[JFloat]) :JavaIterator[Float] =
			i.asInstanceOf[JavaIterator[Float]]

		@inline implicit def boxedDoubleJavaIteratorToJavaIterator(i :JavaIterator[JDouble]) :JavaIterator[Double] =
			i.asInstanceOf[JavaIterator[Double]]

		@inline implicit def boxedBooleanJavaIteratorToJavaIterator(i :JavaIterator[JBoolean]) :JavaIterator[Boolean] =
			i.asInstanceOf[JavaIterator[Boolean]]
	}
}




private object JavaIteratorAdapters {
	class BooleanIterator(underlying :JavaIntIterator) extends Iterator[Boolean] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Boolean = underlying.nextInt() != 0
	}
	class ByteIterator(underlying :JavaIntIterator) extends Iterator[Byte] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Byte = underlying.nextInt().toByte
	}
	class ShortIterator(underlying :JavaIntIterator) extends Iterator[Short] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Short = underlying.nextInt().toShort
	}
	class CharIterator(underlying :JavaIntIterator) extends Iterator[Char] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Char = underlying.nextInt().toChar
	}
	class IntIterator(underlying :JavaIntIterator) extends Iterator[Int] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Int = underlying.nextInt()
	}
	class LongIterator(underlying :JavaLongIterator) extends Iterator[Long] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Long = underlying.nextLong()
	}
	class DoubleIterator(underlying :JavaDoubleIterator) extends Iterator[Double] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Double = underlying.nextDouble()
	}
	class FloatIterator(underlying :JavaDoubleIterator) extends Iterator[Float] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Float = underlying.nextDouble().toFloat
	}
}




private class JavaConcatIterator[A, I <: JavaIterator[A]](iters :IndexedSeq[I], private[this] var idx :Int = 0)
	extends JavaIterator[A]
{ this :I =>
	def this(first :I, second :I) = this(RelayArray.two(first, second))

	private[this] var state = -1 //1 => hasNext, 0 => !hasNext, -1 => unknown
	private[this] var curr :I = _
	def iterators :IndexedSeq[I] = iters

	@inline protected final def current :I = {
		state = -1
		val res = curr
		if (res != null) {
			curr = null.asInstanceOf[I]
			res
		} else {
			val count = iters.length - 1
			var iter :I = iters(idx)
			while (idx < count && !iter.hasNext) {
				idx += 1
				iter = iters(idx)
			}
			iter
		}
	}

	override def hasNext :Boolean = {
		if (state < 0) {
			curr = current
			state = curr.hasNext.toInt
		}
		state == 1
	}
	override def next() = current.next()

	override def toString :String = iters.mkString("", "++", "")
}


@SerialVersionUID(Ver)
private object JavaConcatIterator {
	def apply[A, I](first :I, second :I)(implicit shape :JavaIteratorShape[A, I]) :I =
		(shape.shape match {
			case ReferenceShape =>
				ofRef(first.asInstanceOf[JavaIterator[A]], second.asInstanceOf[JavaIterator[A]])
			case IntShape | CharShape | ByteShape | ShortShape =>
				ofInt(first.asInstanceOf[JavaIntIterator], second.asInstanceOf[JavaIntIterator])
			case LongShape =>
				ofLong(first.asInstanceOf[JavaLongIterator], second.asInstanceOf[JavaLongIterator])
			case DoubleShape | FloatShape =>
				ofDouble(first.asInstanceOf[JavaDoubleIterator], second.asInstanceOf[JavaDoubleIterator])
			case _ => ofAny(first.asInstanceOf[JavaIterator[A]], second.asInstanceOf[JavaIterator[A]])
		}).asInstanceOf[I]

	def apply[A, I](iterators :Seq[I])(implicit shape :JavaIteratorShape[A, I]) :I =
		(shape.shape match {
			case ReferenceShape =>
				ofRef(iterators.castParam[JavaIterator[A]])
			case IntShape | CharShape | ByteShape | ShortShape =>
				ofInt(iterators.castParam[JavaIntIterator])
			case LongShape =>
				ofLong(iterators.castParam[JavaLongIterator])
			case DoubleShape | FloatShape =>
				ofDouble(iterators.castParam[JavaDoubleIterator])
			case _ =>
				ofAny(iterators.castParam[JavaIterator[A]])
		}).asInstanceOf[I]

	def ofAny[A](first :JavaIterator[A], second :JavaIterator[A]) :JavaIterator[A] =
		(first, second) match {
			case (i :JavaIntIterator, j :JavaIntIterator) => ofInt(i, j).asInstanceOf[JavaIterator[A]]
			case (i :JavaLongIterator, j :JavaLongIterator) => ofLong(i, j).asInstanceOf[JavaIterator[A]]
			case (i :JavaDoubleIterator, j :JavaDoubleIterator) => ofDouble(i, j).asInstanceOf[JavaIterator[A]]
			case _ => ofRef(first, second)
		}

	def ofAny[A](iterators :Seq[JavaIterator[A]]) :JavaIterator[A] =
		if (iterators.forall(_.isInstanceOf[JavaIntIterator]))
			ofInt(iterators.castParam[JavaIntIterator]).asInstanceOf[JavaIterator[A]]
		else if (iterators.forall(_.isInstanceOf[JavaLongIterator]))
			ofLong(iterators.castParam[JavaLongIterator]).asInstanceOf[JavaIterator[A]]
		else if (iterators.forall(_.isInstanceOf[JavaDoubleIterator]))
			ofDouble(iterators.castParam[JavaDoubleIterator]).asInstanceOf[JavaIterator[A]]
		else
			ofRef(iterators)

	def ofInt(first :JavaIntIterator, second :JavaIntIterator) :JavaIntIterator = 
		if (!first.hasNext) 
			second 
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfInt, cat2 :OfInt) => new OfInt(cat1.iterators ++ cat2.iterators)
			case (cat :OfInt, _)            => new OfInt(cat.iterators :+ second)
			case (_, cat :OfInt)            => new OfInt(first +: cat.iterators)
			case _                          => new OfInt(RelayArray.two(first, second))
		}

	def ofInt(iterators :Seq[JavaIntIterator]) :JavaIntIterator =
		new OfInt(iterators.toIndexedSeq)

	def ofLong(first :JavaLongIterator, second :JavaLongIterator) :JavaLongIterator =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfLong, cat2 :OfLong) => new OfLong(cat1.iterators ++ cat2.iterators)
			case (cat :OfLong, _)             => new OfLong(cat.iterators :+ second)
			case (_, cat :OfLong)             => new OfLong(first +: cat.iterators)
			case _                            => new OfLong(RelayArray.two(first, second))
		}

	def ofLong(iterators :Seq[JavaLongIterator]) :JavaLongIterator =
		new OfLong(iterators.toIndexedSeq)

	def ofDouble(first :JavaDoubleIterator, second :JavaDoubleIterator) :JavaDoubleIterator =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfDouble, cat2 :OfDouble) => new OfDouble(cat1.iterators ++ cat2.iterators)
			case (cat :OfDouble, _)               => new OfDouble(cat.iterators :+ second)
			case (_, cat :OfDouble)               => new OfDouble(first +: cat.iterators)
			case _                                => new OfDouble(RelayArray.two(first, second))
		}

	def ofDouble(iterators :Seq[JavaDoubleIterator]) :JavaDoubleIterator =
		new OfDouble(iterators.toIndexedSeq)

	def ofRef[A](first :JavaIterator[A], second :JavaIterator[A]) :JavaIterator[A] =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfRef[A] @unchecked, cat2 :OfRef[A] @unchecked) =>
				new OfRef(cat1.iterators ++ cat2.iterators)
			case (cat :OfRef[A] @unchecked, _) =>
				new OfRef(cat.iterators :+ second)
			case (_, cat :OfRef[A] @unchecked) =>
				new OfRef[A](first +: cat.iterators)
			case _ => new OfRef[A](RelayArray.two(first, second))
		}

	def ofRef[A](iterators :Seq[JavaIterator[A]]) :JavaIterator[A] =
		new OfRef(iterators.toIndexedSeq)


	@SerialVersionUID(Ver)
	private class OfInt(iters :IndexedSeq[JavaIntIterator])
		extends JavaConcatIterator[JInt, JavaIntIterator](iters) with JavaIntIterator
	{
		override def nextInt() = current.next()
		override def toString = iterators.mkString("", "++[Int]", "")
	}
	@SerialVersionUID(Ver)
	private class OfLong(iters :IndexedSeq[JavaLongIterator])
		extends JavaConcatIterator[JLong, JavaLongIterator](iters) with JavaLongIterator
	{
		override def nextLong() = current.next()
		override def toString = iterators.mkString("", "++[Long]", "")
	}
	@SerialVersionUID(Ver)
	private class OfDouble(iters :IndexedSeq[JavaDoubleIterator])
		extends JavaConcatIterator[JDouble, JavaDoubleIterator](iters) with JavaDoubleIterator
	{
		override def nextDouble() = current.next()
		override def toString = iterators.mkString("", "++[Double]", "")
	}
	private type OfRef[A] = JavaConcatIterator[A, JavaIterator[A]]
}






@SerialVersionUID(Ver)
object Jterator {
	def from[T, I <: Jterator[_]](elems :IterableOnce[T])(implicit shape :JteratorShape[T, I]) :I =
		JavaIterator.from(elems)(shape.javaIteratorShape).asInstanceOf[I]

	def over[T, I <: Jterator[_]](seq :collection.IndexedSeq[T])(implicit shape :JteratorShape[T, I]) :I =
		IndexedSeqStepper(seq)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def over[T, I <: Jterator[_]](array :Array[T])(implicit shape :JteratorShape[T, I]) :I =
		ArrayStepper(array)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: Jterator[_]](seq :collection.IndexedSeq[T], from :Int, until :Int)
	                              (implicit shape :JteratorShape[T, I]) :I =
		IndexedSeqStepper(seq, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: Jterator[_]](array :Array[T], from :Int, until :Int)
	                              (implicit shape :JteratorShape[T, I]) :I =
		ArrayStepper(array, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

	//Using I without an upper bound of JavaIterator[_] saves us casting the argument in some places,
	// which could create another closure.
	def delay[T, I](iterator: => I)(implicit shape :JteratorShape[T, I]) :I =
		JavaIterator.delay(iterator)(shape.javaIteratorShape.asInstanceOf[JavaIteratorShape[T, I]])

	def empty[T, I](implicit shape :JteratorShape[T, I]) :I = JavaIterator()(shape.javaIteratorShape).asInstanceOf[I]

	/** Returns an object with an `apply()` function returning a Java iterator type specific to type `T`.
	  * This degree of separation allows the iterator type to be inferred based on an implicit
	  * [[net.noresttherein.sugar.collections.JavaIteratorShape JavaIteratorShape]]
	  * based on an explicitly provided type `T`: `JavaIterator[Int]()` will return a `PrimitiveIterator.OfInt`
	  */
	def apply[T] :EmptyJteratorFactory[T] = new EmptyJteratorFactory[T] {}

	sealed trait EmptyJteratorFactory[T] extends Any {
		final def apply[I <: Jterator[_]]()(implicit shape :JteratorShape[T, I]) :I =
			JavaIterator()(shape.javaIteratorShape).asInstanceOf[I]
	}

	def one[T, I <: Jterator[_]](elem :T)(implicit shape :JteratorShape[T, I]) :I =
		JavaIterator.one(elem)(shape.javaIteratorShape).asInstanceOf[I]

	def two[T, I <: Jterator[_]](first :T, second :T)(implicit shape :JteratorShape[T, I]) :I =
		JavaIterator.two(first, second)(shape.javaIteratorShape).asInstanceOf[I]

}
