package net.noresttherein.sugar.collections

import java.util.Spliterator

import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JDoubleIterator, JFloat, JInt, JIntIterator, JIterator, JLong, JLongIterator, JShort}
import net.noresttherein.sugar.extensions.{BooleanExtension, castTypeParamMethods}




@SerialVersionUID(Ver)
object JavaIterator {
	final val Types :Specializable.Group[(Int, Long, Double)] = null

	def over[T, I <: JIterator[_]](seq :collection.IndexedSeq[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		IndexedSeqStepper(seq)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def over[T, I <: JIterator[_]](array :Array[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		ArrayStepper(array)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: JIterator[_]](seq :collection.IndexedSeq[T], from :Int, until :Int) //consider: renaming to over
	                               (implicit shape :JavaIteratorShape[T, I]) :I =
		IndexedSeqStepper(seq, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: JIterator[_]](array :Array[T], from :Int, until :Int)(implicit shape :JavaIteratorShape[T, I]) :I =
		ArrayStepper(array, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

//	def slice(string :String, start :Int, end :Int) :JIntIterator = new StringStepper(string, start, end)


	def empty[T] :JIterator[T] = emptyPrototype.asInstanceOf[JIterator[T]]

	/** Returns an object with an `apply()` function returning a Java iterator type specific to type `T`.
	  * This degree of separation allows the iterator type to be inferred based on an implicit
	  * [[net.noresttherein.sugar.collections.JavaIteratorShape JavaIteratorShape]]
	  * based on an explicitly provided type `T`: `JavaIterator[Int]()` will return a `PrimitiveIterator.OfInt`
	  */
	def apply[T] :EmptyJavaIteratorFactory[T] = new EmptyJavaIteratorFactory[T] {}

	sealed trait EmptyJavaIteratorFactory[T] extends Any {
		final def apply[I <: JIterator[_]]()(implicit shape :JavaIteratorShape[T, I]) :I =
			shape.shape match {
				case LongShape => ofLong().asInstanceOf[I]
				case DoubleShape | FloatShape => ofDouble().asInstanceOf[I]
				case IntShape | CharShape | ShortShape | ByteShape => ofInt().asInstanceOf[I]
				case _ => ofAny().asInstanceOf[I]
			}
	}

	def one[T, I <: JIterator[_]](elem :T)(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case IntShape | CharShape | ByteShape | ShortShape => ofInt(elem.asInstanceOf[Int]).asInstanceOf[I]
			case LongShape   => ofLong(elem.asInstanceOf[Long]).asInstanceOf[I]
			case DoubleShape | FloatShape => ofDouble(elem.asInstanceOf[Double]).asInstanceOf[I]
			case _ => ofAny(elem).asInstanceOf[I]
		}

	def two[T, I <: JIterator[_]](first :T, second :T)(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case IntShape | CharShape | ByteShape | ShortShape =>
				ofInt(first.asInstanceOf[Int], second.asInstanceOf[Int]).asInstanceOf[I]
			case LongShape   =>
				ofLong(first.asInstanceOf[Long], second.asInstanceOf[Long]).asInstanceOf[I]
			case DoubleShape | FloatShape =>
				ofDouble(first.asInstanceOf[Double], second.asInstanceOf[Double]).asInstanceOf[I]
			case _ => ofAny(first, second).asInstanceOf[I]
		}


	def ofAny[T]() :JIterator[T] = emptyPrototype.asInstanceOf[JIterator[T]]

	def ofAny[T](elem :T) :JIterator[T] = new JIterator[T] {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def next() :T =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				empty[T].next()
		override def toString = if (nonEmpty) "JIterator(" + elem + ")" else "JIterator()"
	}
	def ofAny[T](first :T, second :T) :JIterator[T] = new JIterator[T] {
		private[this] var left = 2
		override def hasNext = left > 0
		override def next() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => empty[T].next()
		}
		override def toString = left match {
			case 2 => "JIterator(" + first + ", " + second + ")"
			case 1 => "JIterator(" + second + ")"
			case _ => "JIterator()"
		}
	}
	def ofAny[T](seq :IndexedSeq[T]) :JIterator[T] = slice(seq, 0, seq.length)
	def ofAny[T](seq :IndexedSeq[T], from :Int, until :Int) :JIterator[T] = slice(seq, from, until)
//	def ofAny[T](array :Array[T]) :JIterator[T] = slice(array, 0, array.length)
	def ofAny[T](array :Array[T], from :Int = 0, until :Int = Int.MaxValue) :JIterator[T] = slice(array, from, until)

	def ofInt() :JIntIterator = emptyInt

	def ofInt(elem :Int) :JIntIterator = new JIntIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextInt() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty[JInt].next()
		override def toString =
			if (nonEmpty) "JIntIterator(" + elem + ")" else "JIntIterator()"
	}
	def ofInt(first :Int, second :Int) :JIntIterator = new JIntIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextInt() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty[JInt].next()
		}
		override def toString :String = left match {
			case 2 => "JIntIterator(" + first + ", " + second + ")"
			case 1 => "JIntIterator(" + second + ")"
			case _ => "JIntIterator()"
		}
	}
	def ofInt(seq :IndexedSeq[Int]) :JIntIterator = slice(seq, 0, seq.length)
	def ofInt(seq :IndexedSeq[Int], from :Int, until :Int) :JIntIterator = slice(seq, from, until)
//	def ofInt(array :Array[Int]) :JIntIterator = slice(array, 0, array.length)
	def ofInt(array :Array[Int], from :Int = 0, until :Int = Int.MaxValue) :JIntIterator = slice(array, from, until)
	def ofInt(string :String) :JIntIterator = ofInt(string, 0, string.length)

	@throws[IndexOutOfBoundsException]("if from < 0 or until > string.length.")
	def ofInt(string :String, from :Int, until :Int) :JIntIterator =
		new StringStepper(string, from, until)

	def ofLong() :JLongIterator = emptyLong

	def ofLong(elem :Long) :JLongIterator = new JLongIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextLong() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty[JLong].next()
		override def toString =
			if (nonEmpty) "JLongIterator(" + elem + ")" else "JLongIterator"
	}
	def ofLong(first :Long, second :Long) :JLongIterator = new JLongIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextLong() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty[JLong].next()
		}
		override def toString = left match {
			case 2 => "JLongIterator(" + first + ", " + second + ")"
			case 1 => "JLongIterator(" + second + ")"
			case _ => "JLongIterator()"
		}
	}
	def ofLong(seq :IndexedSeq[Long]) :JLongIterator = slice(seq, 0, seq.length)
	def ofLong(seq :IndexedSeq[Long], from :Int, until :Int) :JLongIterator = slice(seq, from, until)
//	def ofLong(array :Array[Long]) :JLongIterator = slice(array, 0, array.length)
	def ofLong(array :Array[Long], from :Int = 0, until :Int = Int.MaxValue) :JLongIterator = slice(array, from, until)

	def ofDouble() :JDoubleIterator = emptyDouble

	def ofDouble(elem :Double) :JDoubleIterator = new JDoubleIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextDouble() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty.next()
		override def toString =
			if (nonEmpty) "JDoubleIterator(" + elem + ")" else "JDoubleIterator()"
	}

	def ofDouble(first :Double, second :Double) :JDoubleIterator = new JDoubleIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextDouble() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty.next()
		}
		override def toString = left match {
			case 2 => "JDoubleIterator(" + first + ", " + second + ")"
			case 1 => "JDoubleIterator(" + second + ")"
			case _ => "JDoubleIterator()"
		}
	}
	def ofDouble(seq :IndexedSeq[Double]) :JDoubleIterator = slice(seq, 0, seq.length)
	def ofDouble(seq :IndexedSeq[Double], from :Int, until :Int) :JDoubleIterator = slice(seq, from, until)
//	def ofDouble(array :Array[Double]) :JDoubleIterator = slice(array, 0, array.length)
	def ofDouble(array :Array[Double], from :Int = 0, until :Int = Int.MaxValue) :JDoubleIterator =
		slice(array, from, until)


	private[this] val emptyInt :JIntIterator = new JIntIterator {
		override def nextInt() = JavaIterator.empty.next()
		override def hasNext = false
		override def toString = "JIntIterator()"
	}
	private[this] val emptyLong :JLongIterator = new JLongIterator {
		override def nextLong() = JavaIterator.empty.next()
		override def hasNext = false
		override def toString = "JLongIterator()"
	}
	private[this] val emptyDouble :JDoubleIterator = new JDoubleIterator {
		override def nextDouble() = JavaIterator.empty.next()
		override def hasNext = false
		override def toString = "JDoubleIterator()"
	}
	private[this] val emptyPrototype = new JIterator[Nothing] {
		override def hasNext = false
		override def next() = throw new NoSuchElementException("Empty Iterator")
		override def toString = "JIterator()"
	}

	def apply[E](spliterator :Spliterator[E]) :JavaIterator[E] = SpliteratorStepper.ofRef(spliterator).javaIterator
	def apply[E](spliterator :Spliterator.OfInt) :JIntIterator = SpliteratorStepper.ofInt(spliterator).javaIterator
	def apply[E](spliterator :Spliterator.OfLong) :JLongIterator = SpliteratorStepper.ofLong(spliterator).javaIterator
	def apply[E](spliterator :Spliterator.OfDouble) :JDoubleIterator =
		SpliteratorStepper.ofDouble(spliterator).javaIterator

/*
	def apply[E](spliterator :Spliterator[E]) :JavaIterator[E] = spliterator match {
		case ints    :Spliterator.OfInt    => new IntSpliteratorAdapter(ints).castParam[E]
		case longs   :Spliterator.OfLong   => new LongSpliteratorAdapter(longs).castParam[E]
		case doubles :Spliterator.OfDouble => new DoubleSpliteratorAdapter(doubles).castParam[E]
		case _                             => new SpliteratorAdapter(spliterator)
	}
	def apply[E](spliterator :Spliterator.OfInt) :JIntIterator = new IntSpliteratorAdapter(spliterator)
	def apply[E](spliterator :Spliterator.OfLong) :JLongIterator = new LongSpliteratorAdapter(spliterator)
	def apply[E](spliterator :Spliterator.OfDouble) :JDoubleIterator = new DoubleSpliteratorAdapter(spliterator)

	private class SpliteratorAdapter[E](underlying :Spliterator[E]) extends JavaIterator[E] with Consumer[E] {
		private[this] var head :Opt[E] = Lack

		override def hasNext = head.isDefined || { underlying.tryAdvance(this); head.isDefined }

		override def next() = head match {
			case Got(hd) => head = Lack; hd
			case _       => underlying.tryAdvance(this); head.get
		}
		override def accept(t :E) :Unit = head = Got(t)
	}

	private abstract class PrimitiveSpliteratorAdapter
	                       [@specialized(JavaIterator.Types) A, B, S <: Spliterator.OfPrimitive[B, C, S], C]
	                       (underlying :S)
		extends JavaIterator[B] with PrimitiveIterator[B, C]
	{ this :C =>
		private[this] var hd :A    = _
		private[this] var buffered = false

		protected final def head_=(value :A) :Unit = {
			hd = value
			buffered = true
		}
		@inline protected final def head :A =
			if (buffered) hd else throw new NoSuchElementException("empty java iterator")

		override def forEachRemaining(action :Consumer[_ >: B]) :Unit = {
			if (buffered) {
				action.accept(box(hd))
				buffered = false
			}
			underlying.forEachRemaining(action)
		}
		override def forEachRemaining(action :C) :Unit = {
			if (buffered) {
				apply(hd, action)
				buffered = false
			}
			underlying.forEachRemaining(action)
		}

		override def hasNext = buffered || {
			underlying.tryAdvance(this)
			buffered
		}
		override def next() :B = box(nextPrimitive())

		protected def nextPrimitive() :A = {
			if (!buffered) {
				buffer(underlying)
				if (!buffered)
					throw new NoSuchElementException("empty java iterator")
			}
			buffered = false
			hd
		}

		protected def apply(elem :A, action :C) :Unit
		protected def buffer(spliterator :S) :Unit
		protected def box(elem :A) :B
		protected def unbox(elem :B) :A
	}

	private class IntSpliteratorAdapter(underlying :Spliterator.OfInt)
		extends PrimitiveSpliteratorAdapter[Int, JInt, Spliterator.OfInt, IntConsumer](underlying)
			with PrimitiveIterator.OfInt with IntConsumer
	{
		override def nextInt() = nextPrimitive()
		override def accept(value :Int) = head = value

		protected override def apply(elem :Int, action :IntConsumer) :Unit = action.accept(elem)
		protected override def buffer(spliterator :Spliterator.OfInt) :Unit = spliterator.tryAdvance(this :IntConsumer)
		protected override def box(elem :Int) :JInt = elem
		protected override def unbox(elem :JInt) :Int = elem
	}

	private class LongSpliteratorAdapter(underlying :Spliterator.OfLong)
		extends PrimitiveSpliteratorAdapter[Long, JLong, Spliterator.OfLong, LongConsumer](underlying)
			with PrimitiveIterator.OfLong with LongConsumer
	{
		override def nextLong() = nextPrimitive()
		override def accept(value :Long) = head = value

		protected override def apply(elem :Long, action :LongConsumer) :Unit = action.accept(elem)
		protected override def buffer(spliterator :Spliterator.OfLong) :Unit = spliterator.tryAdvance(this :LongConsumer)
		protected override def box(elem :Long) :JLong = elem
		protected override def unbox(elem :JLong) :Long = elem
	}

	private class DoubleSpliteratorAdapter(underlying :Spliterator.OfDouble)
		extends PrimitiveSpliteratorAdapter[Double, JDouble, Spliterator.OfDouble, DoubleConsumer](underlying)
			with PrimitiveIterator.OfDouble with DoubleConsumer
	{
		override def nextDouble() = nextPrimitive()
		override def accept(value :Double) = head = value

		protected override def apply(elem :Double, action :DoubleConsumer) :Unit = action.accept(elem)
		protected override def buffer(spliterator :Spliterator.OfDouble) :Unit =
			spliterator.tryAdvance(this :DoubleConsumer)

		protected override def box(elem :Double) :JDouble = elem
		protected override def unbox(elem :JDouble) :Double = elem
	}
*/


	object conversions {
		@inline implicit def intIteratorToJavaIterator(i :JIntIterator) :JavaIterator[Int] =
			i.asInstanceOf[JavaIterator[Int]]

		@inline implicit def longIteratorToJavaIterator(i :JLongIterator) :JavaIterator[Long] =
			i.asInstanceOf[JavaIterator[Long]]

		@inline implicit def doubleIteratorToJavaIterator(i :JDoubleIterator) :JavaIterator[Double] =
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




@SerialVersionUID(Ver) 
private class JavaConcatIterator[A, I <: JIterator[A]](iters :IndexedSeq[I], private[this] var idx :Int = 0)
	extends JIterator[A]
{ this :I =>
	def this(first :I, second :I) = this(PassedArray.two(first, second))

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
	def apply[A, I <: JIterator[_]](first :I, second :I)(implicit shape :JavaIteratorShape[A, I]) :I =
		(shape.shape match {
			case ReferenceShape =>
				ofRef(first.asInstanceOf[JIterator[A]], second.asInstanceOf[JIterator[A]])
			case IntShape | CharShape | ByteShape | ShortShape =>
				ofInt(first.asInstanceOf[JIntIterator], second.asInstanceOf[JIntIterator])
			case LongShape =>
				ofLong(first.asInstanceOf[JLongIterator], second.asInstanceOf[JLongIterator])
			case DoubleShape | FloatShape =>
				ofDouble(first.asInstanceOf[JDoubleIterator], second.asInstanceOf[JDoubleIterator])
			case _ => ofAny(first.asInstanceOf[JIterator[A]], second.asInstanceOf[JIterator[A]])
		}).asInstanceOf[I]

	def apply[A, I <: JIterator[_]](iterators :Seq[I])(implicit shape :JavaIteratorShape[A, I]) :I =
		(shape.shape match {
			case ReferenceShape =>
				ofRef(iterators.castParam[JIterator[A]])
			case IntShape | CharShape | ByteShape | ShortShape =>
				ofInt(iterators.castParam[JIntIterator])
			case LongShape =>
				ofLong(iterators.castParam[JLongIterator])
			case DoubleShape | FloatShape =>
				ofDouble(iterators.castParam[JDoubleIterator])
			case _ =>
				ofAny(iterators.castParam[JIterator[A]])
		}).asInstanceOf[I]

	def ofAny[A](first :JIterator[A], second :JIterator[A]) :JIterator[A] =
		(first, second) match {
			case (i :JIntIterator, j :JIntIterator) => ofInt(i, j).asInstanceOf[JIterator[A]]
			case (i :JLongIterator, j :JLongIterator) => ofLong(i, j).asInstanceOf[JIterator[A]]
			case (i :JDoubleIterator, j :JDoubleIterator) => ofDouble(i, j).asInstanceOf[JIterator[A]]
			case _ => ofRef(first, second)
		}

	def ofAny[A](iterators :Seq[JIterator[A]]) :JIterator[A] =
		if (iterators.forall(_.isInstanceOf[JIntIterator]))
			ofInt(iterators.castParam[JIntIterator]).asInstanceOf[JIterator[A]]
		else if (iterators.forall(_.isInstanceOf[JLongIterator]))
			ofLong(iterators.castParam[JLongIterator]).asInstanceOf[JIterator[A]]
		else if (iterators.forall(_.isInstanceOf[JDoubleIterator]))
			ofDouble(iterators.castParam[JDoubleIterator]).asInstanceOf[JIterator[A]]
		else
			ofRef(iterators)

	def ofInt(first :JIntIterator, second :JIntIterator) :JIntIterator = 
		if (!first.hasNext) 
			second 
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfInt, cat2 :OfInt) => new OfInt(cat1.iterators ++ cat2.iterators)
			case (cat :OfInt, _)            => new OfInt(cat.iterators :+ second)
			case (_, cat :OfInt)            => new OfInt(first +: cat.iterators)
			case _                          => new OfInt(PassedArray.two(first, second)) 
		}

	def ofInt(iterators :Seq[JIntIterator]) :JIntIterator =
		new OfInt(iterators.toIndexedSeq)

	def ofLong(first :JLongIterator, second :JLongIterator) :JLongIterator =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfLong, cat2 :OfLong) => new OfLong(cat1.iterators ++ cat2.iterators)
			case (cat :OfLong, _)             => new OfLong(cat.iterators :+ second)
			case (_, cat :OfLong)             => new OfLong(first +: cat.iterators)
			case _                            => new OfLong(PassedArray.two(first, second))
		}

	def ofLong(iterators :Seq[JLongIterator]) :JLongIterator =
		new OfLong(iterators.toIndexedSeq)

	def ofDouble(first :JDoubleIterator, second :JDoubleIterator) :JDoubleIterator =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else (first, second) match {
			case (cat1 :OfDouble, cat2 :OfDouble) => new OfDouble(cat1.iterators ++ cat2.iterators)
			case (cat :OfDouble, _)               => new OfDouble(cat.iterators :+ second)
			case (_, cat :OfDouble)               => new OfDouble(first +: cat.iterators)
			case _                                => new OfDouble(PassedArray.two(first, second))
		}

	def ofDouble(iterators :Seq[JDoubleIterator]) :JDoubleIterator =
		new OfDouble(iterators.toIndexedSeq)

	def ofRef[A](first :JIterator[A], second :JIterator[A]) :JIterator[A] =
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
			case _ => new OfRef[A](PassedArray.two(first, second))
		}

	def ofRef[A](iterators :Seq[JIterator[A]]) :JIterator[A] =
		new OfRef(iterators.toIndexedSeq)


	@SerialVersionUID(Ver)
	private class OfInt(iters :IndexedSeq[JIntIterator])
		extends JavaConcatIterator[JInt, JIntIterator](iters) with JIntIterator
	{
		override def nextInt() = current.next()
		override def toString = iterators.mkString("", "++[Int]", "")
	}
	@SerialVersionUID(Ver)
	private class OfLong(iters :IndexedSeq[JLongIterator])
		extends JavaConcatIterator[JLong, JLongIterator](iters) with JLongIterator
	{
		override def nextLong() = current.next()
		override def toString = iterators.mkString("", "++[Long]", "")
	}
	@SerialVersionUID(Ver)
	private class OfDouble(iters :IndexedSeq[JDoubleIterator])
		extends JavaConcatIterator[JDouble, JDoubleIterator](iters) with JDoubleIterator
	{
		override def nextDouble() = current.next()
		override def toString = iterators.mkString("", "++[Double]", "")
	}
	private type OfRef[A] = JavaConcatIterator[A, JIterator[A]]
}

