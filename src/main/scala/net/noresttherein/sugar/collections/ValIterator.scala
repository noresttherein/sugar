package net.noresttherein.sugar.collections

import scala.collection.{BufferedIterator, Stepper, StepperShape}

import net.noresttherein.sugar.vars.{Missing, Opt, Sure, Unsure}
import net.noresttherein.sugar.vars.Opt.{Got, Lack, existent_?}




/**
  * @author Marcin Mościcki
  */
trait ValIterator[@specialized(ElemTypes) +E] extends Iterator[E] {
	override def next() :E

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
				throw new NoSuchElementException("Empty ValIterator")

		override def toString :String =
			ValIterator.this.toString + ".buffered" + (if (nonEmpty) "(" + hd + ")" else "")
	}

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S = ValIteratorStepper(this, 0)
}


object ValIterator {
	trait Buffered[@specialized(ElemTypes) +E] extends ValIterator[E] with BufferedIterator[E] {
		override def head :E
		def headOpt :Opt[E] = if (hasNext) Got(head) else Lack
		def unsureHead :Unsure[E] = if (hasNext) Sure(head) else Missing
		override def buffered :this.type = this
	}
}




trait IntIterator extends ValIterator[Int] {

}
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