package net.noresttherein.sugar.arrays

import java.util.Arrays

import net.noresttherein.sugar.casting.castTypeParamMethods




@SerialVersionUID(Ver)
private class ArrayOrdering[E](implicit ordering :Ordering[E]) extends Ordering[Array[E]] {
	override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
		case (a :Array[AnyRef], b :Array[AnyRef]) => Arrays.compare(a, b, ordering.castParam[AnyRef])
//		case (a :Array[Int], b :Array[Int])       => Arrays.compare(a, b, ordering.castParam[Int])
//		case (a :Array[Long], b :Array[Long])     => Arrays.compare(a, b, ordering.castParam[Long])
//		case (a :Array[Double], b :Array[Double]) => Arrays.compare(a, b, ordering.castParam[Double])
//		case (a :Array[Byte], b :Array[Byte])     => Arrays.compare(a, b, ordering.castParam[Byte])
//		case (a :Array[Float], b :Array[Float])   => Arrays.compare(a, b, ordering.castParam[Float])
//		case (a :Array[Char], b :Array[Char])     => Arrays.compare(a, b, ordering.castParam[Char])
//		case (a :Array[Short], b :Array[Short])   => Arrays.compare(a, b, ordering.castParam[Short])
		case _ =>
			val end = math.min(x.length, y.length)
			var i = 0
			var cmp = 0
			while (i < end & cmp == 0) {
				cmp = ordering.compare(x(i), y(i))
				i += 1
			}
			cmp
	}
}



private object ArrayOrdering {
	@SerialVersionUID(Ver)
	class ByteArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Byte], b :Array[Byte]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver)
	class ShortArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Short], b :Array[Short]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver)
	class CharArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Char], b :Array[Char]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver)
	class IntArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Int], b :Array[Int]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver)
	class LongArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Long], b :Array[Long]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver)
	class DoubleArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Double], b :Array[Double]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver)
	class FloatArrayOrdering[E :Ordering] extends ArrayOrdering[E] {
		override def compare(x :Array[E], y :Array[E]) :Int = (x :Array[_], y :Array[_]) match {
			case (a :Array[Float], b :Array[Float]) => Arrays.compare(a, b)
			case _ => super.compare(x, y)
		}
	}
	@SerialVersionUID(Ver) object ByteArrayOrdering extends ByteArrayOrdering[Byte]
	@SerialVersionUID(Ver) object ShortArrayOrdering extends ShortArrayOrdering[Short]
	@SerialVersionUID(Ver) object CharArrayOrdering extends CharArrayOrdering[Char]
	@SerialVersionUID(Ver) object IntArrayOrdering extends IntArrayOrdering[Int]
	@SerialVersionUID(Ver) object LongArrayOrdering extends LongArrayOrdering[Long]
	@SerialVersionUID(Ver) object FloatArrayTotalOrdering extends FloatArrayOrdering[Float]()(Ordering.Float.TotalOrdering)
	@SerialVersionUID(Ver) object FloatArrayIEEEOrdering extends FloatArrayOrdering[Float]()(Ordering.Float.IeeeOrdering)
	@SerialVersionUID(Ver)
	object DoubleArrayTotalOrdering extends DoubleArrayOrdering[Double]()(Ordering.Double.TotalOrdering)
	@SerialVersionUID(Ver)
	object DoubleArrayIEEEOrdering extends DoubleArrayOrdering[Double]()(Ordering.Double.IeeeOrdering)
}
