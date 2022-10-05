package net.noresttherein.sugar.collections

import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JDouble, JDoubleIterator, JInt, JIntIterator, JIterator, JLong, JLongIterator}




@SerialVersionUID(ver)
object JavaIterator {
	def over[T, I <: JIterator[_]](seq :collection.IndexedSeq[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		IndexedSeqStepper(seq)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def over[T, I <: JIterator[_]](array :Array[T])(implicit shape :JavaIteratorShape[T, I]) :I =
		ArrayStepper(array)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: JIterator[_]](seq :collection.IndexedSeq[T], from :Int, until :Int)
	                               (implicit shape :JavaIteratorShape[T, I]) :I =
		IndexedSeqStepper(seq, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

	def slice[T, I <: JIterator[_]](array :Array[T], from :Int, until :Int)(implicit shape :JavaIteratorShape[T, I]) :I =
		ArrayStepper(array, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]


	def empty[T] :JIterator[T] = emptyPrototype.asInstanceOf[JIterator[T]]

	def apply[T, I <: JIterator[_]]()(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case LongShape => ofLong().asInstanceOf[I]
			case DoubleShape | FloatShape => ofDouble().asInstanceOf[I]
			case IntShape | CharShape | ShortShape | ByteShape => ofInt().asInstanceOf[I]
			case _ => ofRef().asInstanceOf[I]
		}

	def apply[T, I <: JIterator[_]](elem :T)(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case IntShape    => ofInt(elem.asInstanceOf[Int]).asInstanceOf[I]
			case LongShape   => ofLong(elem.asInstanceOf[Long]).asInstanceOf[I]
			case DoubleShape => ofDouble(elem.asInstanceOf[Double]).asInstanceOf[I]
			case CharShape   => ofInt(elem.asInstanceOf[Char]).asInstanceOf[I]
			case ByteShape   => ofInt(elem.asInstanceOf[Byte]).asInstanceOf[I]
			case ShortShape  => ofInt(elem.asInstanceOf[Short]).asInstanceOf[I]
			case FloatShape  => ofDouble(elem.asInstanceOf[Float]).asInstanceOf[I]
			case _ => ofRef(elem).asInstanceOf[I]
		}
		
	def apply[T, I <: JIterator[_]](first :T, second :T)(implicit shape :JavaIteratorShape[T, I]) :I =
		shape.shape match {
			case IntShape    => ofInt(first.asInstanceOf[Int], second.asInstanceOf[Int]).asInstanceOf[I]
			case LongShape   => ofLong(first.asInstanceOf[Long], second.asInstanceOf[Long]).asInstanceOf[I]
			case DoubleShape => ofDouble(first.asInstanceOf[Double], second.asInstanceOf[Double]).asInstanceOf[I]
			case CharShape   => ofInt(first.asInstanceOf[Char], second.asInstanceOf[Char]).asInstanceOf[I]
			case ByteShape   => ofInt(first.asInstanceOf[Byte], second.asInstanceOf[Byte]).asInstanceOf[I]
			case ShortShape  => ofInt(first.asInstanceOf[Short], first.asInstanceOf[Short]).asInstanceOf[I]
			case FloatShape  => ofDouble(first.asInstanceOf[Float], first.asInstanceOf[Float]).asInstanceOf[I]
			case _ => ofRef(first, second).asInstanceOf[I]
		}


	def ofRef[T]() :JIterator[T] = emptyPrototype.asInstanceOf[JIterator[T]]

	def ofRef[T](elem :T) :JIterator[T] = new JIterator[T] {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def next() :T =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				empty[T].next()
	}
	def ofRef[T](first :T, second :T) :JIterator[T] = new JIterator[T] {
		private[this] var left = 2
		override def hasNext = left > 0
		override def next() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => empty[T].next()
		}
	}

	def ofInt() :JIntIterator = emptyInt

	def ofInt(elem :Int) :JIntIterator = new JIntIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextInt() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty[JInt].next()
	}
	def ofInt(first :Int, second :Int) :JIntIterator = new JIntIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextInt() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty[JInt].next()
		}
	}

	def ofChar() :JIntIterator = ofInt()
	def ofChar(elem :Char) :JIntIterator = ofInt(elem)
	def ofChar(first :Char, second :Char) :JIntIterator = ofInt(first, second)
	def ofChar(string :String) :JIntIterator = ofChar(string, 0, string.length)
	def ofChar(string :String, from :Int, until :Int) :JIntIterator = new JIntIterator {
		private[this] var i = from
		override def nextInt() = { val old = i; i += 1; string.charAt(old) }
		override def hasNext = i < until
	}

	def ofByte() :JIntIterator = ofInt()
	def ofByte(elem :Byte) :JIntIterator = ofInt(elem)
	def ofByte(first :Byte, second :Byte) :JIntIterator = ofInt(first, second)
	
	def ofShort() :JIntIterator = ofInt()
	def ofShort(elem :Short) :JIntIterator = ofInt(elem)
	def ofShort(first :Short, second :Short) :JIntIterator = ofInt(first, second)
	
	def ofLong() :JLongIterator = emptyLong

	def ofLong(elem :Long) :JLongIterator = new JLongIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextLong() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty[JLong].next()
	}
	def ofLong(first :Long, second :Long) :JLongIterator = new JLongIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextLong() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty[JLong].next()
		}
	}

	def ofDouble() :JDoubleIterator = emptyDouble

	def ofDouble(elem :Double) :JDoubleIterator = new JDoubleIterator {
		private[this] var nonEmpty = true
		override def hasNext = nonEmpty
		override def nextDouble() =
			if (nonEmpty) {
				nonEmpty = false; elem
			} else
				JavaIterator.empty.next()
	}

	def ofDouble(first :Double, second :Double) :JDoubleIterator = new JDoubleIterator {
		private[this] var left = 2
		override def hasNext = left > 0
		override def nextDouble() = left match {
			case 2 => left = 1; first
			case 1 => left = 0; second
			case _ => JavaIterator.empty.next()
		}
	}

	def ofFloat() :JDoubleIterator = ofDouble()
	def ofFloat(elem :Float) :JDoubleIterator = ofDouble(elem)
	def ofFloat(first :Float, second :Float) :JDoubleIterator = ofDouble(first, second)


	private[this] val emptyInt :JIntIterator = new JIntIterator {
		override def nextInt() = JavaIterator.empty.next()

		override def hasNext = false
	}
	private[this] val emptyLong :JLongIterator = new JLongIterator {
		override def nextLong() = JavaIterator.empty.next()
		override def hasNext = false
	}
	private[this] val emptyDouble :JDoubleIterator = new JDoubleIterator {
		override def nextDouble() = JavaIterator.empty.next()
		override def hasNext = false
	}
	private[this] val emptyPrototype = new JIterator[Nothing] {
		override def hasNext = false
		override def next() = throw new NoSuchElementException("Empty Iterator")
	}
}




@SerialVersionUID(ver)
private class JavaConcatIterator[A, I <: JIterator[A]](iter1 :I, iter2 :I) extends JIterator[A] { this :I =>
	private[this] var firstHasNext = false
	private[this] var firstHasNextValid = false

	@inline protected final def current = {
		if (!firstHasNextValid) {
			firstHasNext = iter1.hasNext
			firstHasNextValid = true
		}
		if (firstHasNext) {
			firstHasNextValid = false
			iter1
		} else
			iter2
	}

	override def hasNext :Boolean = {
		if (!firstHasNextValid)
			firstHasNext = iter1.hasNext
		firstHasNext || iter2.hasNext
	}

	override def next() = current.next()
}


@SerialVersionUID(ver)
object JavaConcatIterator {
	def apply[A, I <: JIterator[_]](first :I, second :I)(implicit shape :JavaIteratorShape[A, I]) :I =
		(shape.shape match {
			case IntShape    => ofInt(first.asInstanceOf[JIntIterator], second.asInstanceOf[JIntIterator])
			case LongShape   => ofLong(first.asInstanceOf[JLongIterator], second.asInstanceOf[JLongIterator])
			case DoubleShape => ofDouble(first.asInstanceOf[JDoubleIterator], second.asInstanceOf[JDoubleIterator])
			case CharShape   => ofInt(first.asInstanceOf[JIntIterator], second.asInstanceOf[JIntIterator])
			case ByteShape   => ofInt(first.asInstanceOf[JIntIterator], second.asInstanceOf[JIntIterator])
			case ShortShape  => ofInt(first.asInstanceOf[JIntIterator], second.asInstanceOf[JIntIterator])
			case FloatShape  => ofDouble(first.asInstanceOf[JDoubleIterator], second.asInstanceOf[JDoubleIterator])
			case _           => ofAny(first.asInstanceOf[JIterator[A]], second.asInstanceOf[JIterator[A]])
		}).asInstanceOf[I]

	def ofAny[A](first :JIterator[A], second :JIterator[A]) :JIterator[A] =
		(first, second) match {
			case (i :JIntIterator, j :JIntIterator) => new OfInt(i, j).asInstanceOf[JIterator[A]]
			case (i :JLongIterator, j :JLongIterator) => new OfLong(i, j).asInstanceOf[JIterator[A]]
			case (i :JDoubleIterator, j :JDoubleIterator) => new OfDouble(i, j).asInstanceOf[JIterator[A]]
			case _ => new JavaConcatIterator[A, JIterator[A]](first, second)
		}

	def ofInt(first :JIntIterator, second :JIntIterator) :JIntIterator = new OfInt(first, second)
	def ofLong(first :JLongIterator, second :JLongIterator) :JLongIterator = new OfLong(first, second)
	def ofDouble(first :JDoubleIterator, second :JDoubleIterator) :JDoubleIterator = new OfDouble(first, second)
	def ofRef[A](first :JIterator[A], second :JIterator[A]) :JIterator[A] =
		new JavaConcatIterator[A, JIterator[A]](first, second)

	@SerialVersionUID(ver)
	private class OfInt(_1 :JIntIterator, _2 :JIntIterator)
		extends JavaConcatIterator[JInt, JIntIterator](_1, _2) with JIntIterator
	{
		override def nextInt() = current.next()
	}
	@SerialVersionUID(ver)
	private class OfLong(_1 :JLongIterator, _2 :JLongIterator)
		extends JavaConcatIterator[JLong, JLongIterator](_1, _2) with JLongIterator
	{
		override def nextLong() = current.next()
	}
	@SerialVersionUID(ver)
	private class OfDouble(_1 :JDoubleIterator, _2 :JDoubleIterator)
		extends JavaConcatIterator[JDouble, JDoubleIterator](_1, _2) with JDoubleIterator
	{
		override def nextDouble() = current.next()
	}
}
