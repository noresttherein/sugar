package net.noresttherein.sugar.collections

import java.util.PrimitiveIterator

import scala.Specializable.AllNumeric
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, ShortShape}
import scala.collection.{BufferedIterator, Stepper, StepperShape}

import net.noresttherein.sugar.collections.ValIterator.{BooleanJavaIteratorAdapter, ByteJavaIteratorAdapter, CharJavaIteratorAdapter, DoubleJavaIteratorAdapter, FloatJavaIteratorAdapter, IntJavaIteratorAdapter, JavaIteratorAdapter, LongJavaIteratorAdapter, ShortJavaIteratorAdapter}
import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.extensions.castingMethods
import net.noresttherein.sugar.reflect.Specialized.Fun2Arg
import net.noresttherein.sugar.vars.{Maybe, Missing, Sure, Unsure}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




//todo: extend IterableOnceOps[E, ValIterator, ValIterator[E]]
/**
  * @author Marcin Mościcki
  */ //consider: specializing for Boolean
trait ValIterator[@specialized(AllNumeric) +E] extends Iterator[E] { outer =>
	override def next() :E

	override def reduceLeft[B >: E](op :(B, E) => B) :B =
		if (!hasNext)
			unsupported_!("Iterator.empty.reduceLeft")
		else
			foldLeft[B](next())(op)

	override def foldLeft[@specialized(Fun2Arg) B](z :B)(op :(B, E) => B) :B = {
		var res = z
		while (hasNext) {
			res = op(res, next())
		}
		res
	}

	override def filterNot(p :E => Boolean) :ValIterator[E] = filter(p, false)
	override def filter(p :E => Boolean) :ValIterator[E] = filter(p, true)
	private def filter(p :E => Boolean, truth :Boolean) :ValIterator[E] = new ValIterator.Buffered[E] {
		private[this] var hasMore = false
		private[this] var hd :E = _
		override def head =
			if (hasMore) hd
			else noSuch_!("Empty iterator " + this)

		while (outer.hasNext && { hd = outer.next(); hasMore = p(hd) == truth; !hasMore })
			{}
		override def hasNext = hasMore
		override def next() :E =
			if (!hasMore)
				noSuch_!("Empty iterator " + this)
			else {
				val res = hd
				hasMore = false
				while (outer.hasNext && { hd = outer.next(); hasMore = p(hd) == truth; !hasMore })
					()
				res
		}
		override def toString :String = outer.toString + (if (truth) ".filter(" else ".filterNot(") + p + ")"
	}

	override def buffered :ValIterator.Buffered[E] = new ValIterator.Buffered[E] {
		private[this] var hd :E = _
		private[this] var notEmpty :Boolean = ValIterator.this.hasNext
		override def hasNext :Boolean = notEmpty
		override def head :E = hd
		override def next() :E =
			if (notEmpty) {
				val res = hd
				notEmpty = ValIterator.this.hasNext
				if (notEmpty)
					hd = ValIterator.this.next()
				res
			} else
				noSuch_!("Empty ValIterator")

		override def jterator[J](implicit shape :JteratorShape[E, J]) :J = ValIterator.this.jterator

		override def toString :String =
			ValIterator.this.toString + ".buffered" + (if (nonEmpty) "(" + hd + ")" else "")
	}

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S = ValIteratorStepper(this, 0)

	def jterator[J](implicit shape :JteratorShape[E, J]) :J = (shape.shape match {
		case Yes(ReferenceShape) => new JavaIteratorAdapter(this)
		case Yes(IntShape)       => new IntJavaIteratorAdapter(this.asInstanceOf[ValIterator[Int]])
		case Yes(LongShape)      => new LongJavaIteratorAdapter(this.asInstanceOf[ValIterator[Long]])
		case Yes(DoubleShape)    => new DoubleJavaIteratorAdapter(this.asInstanceOf[ValIterator[Double]])
		case Yes(CharShape)      => new CharJavaIteratorAdapter(this.asInstanceOf[ValIterator[Char]])
		case Yes(ByteShape)      => new ByteJavaIteratorAdapter(this.asInstanceOf[ValIterator[Byte]])
		case Yes(FloatShape)     => new FloatJavaIteratorAdapter(this.asInstanceOf[ValIterator[Float]])
		case Yes(ShortShape)     => new ShortJavaIteratorAdapter(this.asInstanceOf[ValIterator[Short]])
		case _ /* Boolean */     => new BooleanJavaIteratorAdapter(this.asInstanceOf[ValIterator[Boolean]])
	}).asInstanceOf[J]
}



@SerialVersionUID(Ver)
object ValIterator {
	def one[@specialized(AllNumeric) T](value :T) :ValIterator[T] = new Buffered[T] {
		var hasNext = true
		private[this] val hd = value
		override def head = if (!hasNext) noSuch_!("empty iterator") else hd
		override def knownSize = if (hasNext) 1 else 0

		override def next() :T =
			if (!hasNext) noSuch_!("empty iterator")
			else { hasNext = false; hd }
	}

	def two[@specialized(AllNumeric) T](first :T, second :T) :ValIterator[T] = new Buffered[T] {
		private[this] var remaining = 2
		override def hasNext = remaining > 0
		override def knownSize = remaining
		override def head = remaining match {
			case 2 => first
			case 1 => second
			case _ => noSuch_!("empty iterator")
		}
		override def next() = remaining match {
			case 2 => remaining = 1; first
			case 1 => remaining = 0; second
			case _ => noSuch_!("empty iterator")
		}
	}

	trait Buffered[@specialized(AllNumeric) +E] extends ValIterator[E] with BufferedIterator[E] {
		override def head :E
		def headOpt :Maybe[E] = if (hasNext) Yes(head) else No
		def unsureHead :Unsure[E] = if (hasNext) Sure(head) else Missing
		override def buffered :this.type = this
	}
	
	private class IntJavaIteratorAdapter(underlying :ValIterator[Int]) extends PrimitiveIterator.OfInt {
		override def hasNext :Boolean = underlying.hasNext
		override def nextInt() :Int = underlying.next()
		override def toString :String = underlying.toString
	}
	private class LongJavaIteratorAdapter(underlying :ValIterator[Long]) extends PrimitiveIterator.OfLong {
		override def hasNext :Boolean = underlying.hasNext
		override def nextLong() :Long = underlying.next()
		override def toString :String = underlying.toString
	}
	private class DoubleJavaIteratorAdapter(underlying :ValIterator[Double]) extends PrimitiveIterator.OfDouble {
		override def hasNext :Boolean = underlying.hasNext
		override def nextDouble() :Double = underlying.next()
		override def toString :String = underlying.toString
	}
	private class FloatJavaIteratorAdapter(underlying :ValIterator[Float]) extends PrimitiveIterator.OfDouble {
		override def hasNext :Boolean = underlying.hasNext
		override def nextDouble() :Double = underlying.next()
		override def toString :String = underlying.toString
	}
	private class CharJavaIteratorAdapter(underlying :ValIterator[Char]) extends PrimitiveIterator.OfInt {
		override def hasNext :Boolean = underlying.hasNext
		override def nextInt() :Int = underlying.next()
		override def toString :String = underlying.toString
	}
	private class ByteJavaIteratorAdapter(underlying :ValIterator[Byte]) extends PrimitiveIterator.OfInt {
		override def hasNext :Boolean = underlying.hasNext
		override def nextInt() :Int = underlying.next()
		override def toString :String = underlying.toString
	}
	private class ShortJavaIteratorAdapter(underlying :ValIterator[Short]) extends PrimitiveIterator.OfInt {
		override def hasNext :Boolean = underlying.hasNext
		override def nextInt() :Int = underlying.next()
		override def toString :String = underlying.toString
	}
	private class BooleanJavaIteratorAdapter(underlying :ValIterator[Boolean]) extends PrimitiveIterator.OfInt {
		override def hasNext :Boolean = underlying.hasNext
		override def nextInt() :Int = if (underlying.next()) 1 else 0
		override def toString :String = underlying.toString
	}
	private class JavaIteratorAdapter[E](underlying :ValIterator[E]) extends java.util.Iterator[E] {
		override def hasNext :Boolean = underlying.hasNext
		override def next() :E = underlying.next()
		override def toString :String = underlying.toString
	}
}




trait IntIterator extends ValIterator[Int]
object IntIterator {
	def from(javaIterator :JavaIntIterator) :IntIterator =
		new IntIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextInt()
			override def toString = javaIterator.toString
		}

	trait Buffered extends IntIterator with ValIterator.Buffered[Int]
}

trait LongIterator extends ValIterator[Long]
object LongIterator {
	def from(javaIterator :JavaLongIterator) :LongIterator =
		new LongIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextLong()
			override def toString = javaIterator.toString
		}

	trait Buffered extends LongIterator with ValIterator.Buffered[Long]
}

trait DoubleIterator extends ValIterator[Double]
object DoubleIterator {
	def from(javaIterator :JavaDoubleIterator) :DoubleIterator =
		new DoubleIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextDouble()
			override def toString = javaIterator.toString
		}

	trait Buffered extends DoubleIterator with ValIterator.Buffered[Double]
}

trait FloatIterator extends ValIterator[Float]
object FloatIterator {
	def from(javaIterator :JavaDoubleIterator) :FloatIterator =
		new FloatIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextDouble().toFloat
			override def toString = javaIterator.toString
		}

	trait Buffered extends FloatIterator with ValIterator.Buffered[Float]
}

trait ByteIterator extends ValIterator[Byte]
object ByteIterator {
	def from(javaIterator :JavaIntIterator) :ByteIterator =
		new ByteIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextInt().toByte
			override def toString = javaIterator.toString
		}
	trait Buffered extends ByteIterator with ValIterator.Buffered[Byte]
}

trait CharIterator extends ValIterator[Char]
object CharIterator {
	def from(javaIterator :JavaIntIterator) :CharIterator =
		new CharIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextInt().toChar
			override def toString = javaIterator.toString
		}
	trait Buffered extends CharIterator with ValIterator.Buffered[Char]
}

trait ShortIterator extends ValIterator[Short]
object ShortIterator {
	def from(javaIterator :JavaIntIterator) :ShortIterator =
		new ShortIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextInt().toShort
			override def toString = javaIterator.toString
		}
	trait Buffered extends ShortIterator with ValIterator.Buffered[Short]
}

trait BooleanIterator extends ValIterator[Boolean]
object BooleanIterator {
	@inline def from(javaIterator :JavaIntIterator) :BooleanIterator =
		from(javaIterator, 0)
	def from(javaIterator :JavaIntIterator, falseValue :Int) :BooleanIterator =
		new BooleanIterator {
			override def hasNext  = javaIterator.hasNext
			override def next()   = javaIterator.nextInt() != falseValue
			override def toString = javaIterator.toString
		}
	trait Buffered extends BooleanIterator with ValIterator.Buffered[Boolean]
}
