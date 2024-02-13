package net.noresttherein.sugar.arrays

import java.util.random.RandomGenerator

import scala.Specializable.Everything
import scala.annotation.tailrec

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{null_!, unsupported_!}
import net.noresttherein.sugar.extensions.cast2TypeParamsMethods
import net.noresttherein.sugar.outOfBounds_!




/** Includes utility methods creating, and working on arrays.
  * These are primarily duplicates or overloads of methods in object `Array`,
  * and methods which internally use specialization. Separate compilation with Scala 2
  * allows to use them also in Scala 3, without needing to resort to 'manual specialization'.
  * @author Marcin Mościcki
  */
private[sugar] object ArrayLikeSpecOps {
	def foreach[E, U](array :Array[E], from :Int, until :Int)(f :E => U) :Unit = {
		def foreach[@specialized(Everything) X](arr :Array[X])(f :X => U) :Unit = {
			val end = math.min(until, arr.length)
			var i   = math.max(from, 0)
			while (i < end) {
				f(arr(i))
				i += 1
			}
		}
		val g = f.asInstanceOf[Any => U]
		array match {
			case a :Array[AnyRef]   => foreach(a)(g)
			case a :Array[Int]      => foreach(a)(g)
			case a :Array[Long]     => foreach(a)(g)
			case a :Array[Double]   => foreach(a)(g)
			case a :Array[Char]     => foreach(a)(g)
			case a :Array[Byte]     => foreach(a)(g)
			case a :Array[Float]    => foreach(a)(g)
			case a :Array[Short]    => foreach(a)(g)
			case a :Array[Boolean]  => foreach(a)(g)
			case null               => null_!("null array")
		}
	}
	
	def indexOf[E](array :Array[_ <: E], offset :Int, length :Int)(elem :E, from :Int) :Int = {
		def indexOf[@specialized(Everything) X](arr :Array[X], elem :Any, from :Int) :Int = {
			val end = offset + length
			var i = offset + math.max(from, 0)
			if (array.getClass.getComponentType isAssignableFrom Unbox(elem.getClass)) {
				val x = elem.asInstanceOf[X]
				while (i < end && arr(i) != x)
					i += 1
			} else
				while (i < end && arr(i) != elem)
					i += 1
			if (i == end) -1 else i - offset
		}
		if (from >= length)
			-1
		else array match {
			case a :Array[AnyRef]  => indexOf(a, elem, from)
			case a :Array[Int]     => indexOf(a, elem, from)
			case a :Array[Long]    => indexOf(a, elem, from)
			case a :Array[Double]  => indexOf(a, elem, from)
			case a :Array[Char]    => indexOf(a, elem, from)
			case a :Array[Byte]    => indexOf(a, elem, from)
			case a :Array[Float]   => indexOf(a, elem, from)
			case a :Array[Short]   => indexOf(a, elem, from)
			case a :Array[Boolean] => indexOf(a, elem, from)
			case null              => null_!("null array")
		}
	}

	def lastIndexOf[E](array :Array[_ <: E], offset :Int, length :Int)(elem :E, end :Int) :Int = {
		def lastIndexOf[@specialized(Everything) X](arr :Array[X], elem :Any, start :Int) :Int = {
			var i = offset + start
			if (array.getClass.getComponentType isAssignableFrom Unbox(elem.getClass)) {
				val x = elem.asInstanceOf[X]
				while (i >= offset && arr(i) != x)
					i -= 1
			} else
				while (i >= offset && arr(i) != elem)
					i -= 1
			i - offset
		}
		if (end < 0)
			-1
		else array match {
			case a :Array[AnyRef]  => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Int]     => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Long]    => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Double]  => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Char]    => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Byte]    => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Float]   => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Short]   => lastIndexOf(a, elem, math.min(end, length - 1))
			case a :Array[Boolean] => lastIndexOf(a, elem, math.min(end, length - 1))
			case null              => null_!("null array")
		}
	}

	def lastIndexWhere[E](array :Array[E], offset :Int, length :Int)(p :E => Boolean, end :Int) :Int = {
		def lastIndexWhere[@specialized(Everything) X](arr :Array[X], p :X => Boolean, from :Int, until :Int) :Int = {
			var i = until
			while (i >= from && !p(arr(i)))
				i -= 1
			i - from
		}
		val start = offset + math.min(end, length - 1)
		if (end < 0)
			-1
		else array match {
			case a :Array[AnyRef]  => lastIndexWhere(a, p.asInstanceOf[AnyRef => Boolean], offset, start)
			case a :Array[Int]     => lastIndexWhere(a, p.asInstanceOf[Int => Boolean], offset, start)
			case a :Array[Long]    => lastIndexWhere(a, p.asInstanceOf[Long => Boolean], offset, start)
			case a :Array[Double]  => lastIndexWhere(a, p.asInstanceOf[Double => Boolean], offset, start)
			case a :Array[Char]    => lastIndexWhere(a, p.asInstanceOf[Char => Boolean], offset, start)
			case a :Array[Byte]    => lastIndexWhere(a, p.asInstanceOf[Byte => Boolean], offset, start)
			case a :Array[Float]   => lastIndexWhere(a, p.asInstanceOf[Float => Boolean], offset, start)
			case a :Array[Short]   => lastIndexWhere(a, p.asInstanceOf[Short => Boolean], offset, start)
			case a :Array[Boolean] => lastIndexWhere(a, p.asInstanceOf[Boolean => Boolean], offset, start)
			case null              => null_!("null array")
		}
	}

	def indexWhere[E](array :Array[E], offset :Int, length :Int)(p :E => Boolean, from :Int) :Int =
		if (from >= length)
			-1
//		else
//			indexWhere(array, offset + math.max(from, 0), offset + length, false)(p)
		else {
			val from0 = math.max(from, 0)
			val res = segmentLength(array, offset + from0, offset + length, true)(p)
			if (res == length - from0) -1 else res + from0
		}

/*
	def indexWhere[E](array :Array[E], from :Int, until :Int, flipped :Boolean)(p :E => Boolean) :Int = {
		def indexWhere[@specialized(Everything) X](arr :Array[X], p :X => Boolean) :Int = {
			var i = from
			while (i < until && p(arr(i)) == flipped)
				i += 1
			i - from
		}
		val length = array.length
		if (from >= until | until < 0 || from >= length)
			-1
		else array match {
			case a :Array[AnyRef]  => indexWhere(a, p.asInstanceOf[AnyRef => Boolean])
			case a :Array[Int]     => indexWhere(a, p.asInstanceOf[Int => Boolean])
			case a :Array[Long]    => indexWhere(a, p.asInstanceOf[Long => Boolean])
			case a :Array[Double]  => indexWhere(a, p.asInstanceOf[Double => Boolean])
			case a :Array[Char]    => indexWhere(a, p.asInstanceOf[Char => Boolean])
			case a :Array[Byte]    => indexWhere(a, p.asInstanceOf[Byte => Boolean])
			case a :Array[Float]   => indexWhere(a, p.asInstanceOf[Float => Boolean])
			case a :Array[Short]   => indexWhere(a, p.asInstanceOf[Short => Boolean])
			case a :Array[Boolean] => indexWhere(a, p.asInstanceOf[Boolean => Boolean])
			case null               => throw new NullPointerException()
		}
	}
*/

	@inline def segmentLength[E](array :Array[E], offset :Int, length :Int)(p :E => Boolean, from :Int) :Int =
		if (from >= length)
			0
//		else {
//			val from0 = offset + math.max(from, 0)
//			val idx   = indexWhere(array, from0, offset + length, true)(p)
//			if (idx < 0) length - from0 else idx
//		}
		else
			segmentLength(array, offset + math.max(from, 0), offset + length, false)(p)

	def segmentLength[E](array :Array[E], from :Int, until :Int, flipped :Boolean)(p :E => Boolean) :Int = {
		def segmentLen[@specialized(Everything) X](arr :Array[X], p :X => Boolean) :Int = {
			var i = from
			while (i < until && p(arr(i)) != flipped)
				i += 1
			i - from
		}
		val length = array.length
		if (from >= until | until < 0 || from >= length)
			0
		else array match {
			case a :Array[AnyRef]  => segmentLen(a, p.asInstanceOf[AnyRef => Boolean])
			case a :Array[Int]     => segmentLen(a, p.asInstanceOf[Int => Boolean])
			case a :Array[Long]    => segmentLen(a, p.asInstanceOf[Long => Boolean])
			case a :Array[Double]  => segmentLen(a, p.asInstanceOf[Double => Boolean])
			case a :Array[Char]    => segmentLen(a, p.asInstanceOf[Char => Boolean])
			case a :Array[Byte]    => segmentLen(a, p.asInstanceOf[Byte => Boolean])
			case a :Array[Float]   => segmentLen(a, p.asInstanceOf[Float => Boolean])
			case a :Array[Short]   => segmentLen(a, p.asInstanceOf[Short => Boolean])
			case a :Array[Boolean] => segmentLen(a, p.asInstanceOf[Boolean => Boolean])
			case null              => null_!("null array")
		}
	}
/*
	def forall[E](array :Array[E], from :Int, until :Int, flipped :Boolean = false)(f :E => Boolean) :Boolean = {
		val length = array.length
		until <= from | until <= 0 || from >= length || {
			def forall[@specialized(Everything) X](a :Array[X], f :X => Boolean) :Boolean = {
				val until0 = math.min(until, length)
				var i      = math.max(from, 0)
				while (i < until0 && f(a(i)) != flipped)
					i += 1
				i == until0
			}
			(array :Array[_]) match {
				case a :Array[AnyRef]   => forall(a, f.castParam1[AnyRef])
				case a :Array[Int]      => forall(a, f.castParam1[Int])
				case a :Array[Long]     => forall(a, f.castParam1[Long])
				case a :Array[Double]   => forall(a, f.castParam1[Double])
				case a :Array[Char]     => forall(a, f.castParam1[Char])
				case a :Array[Byte]     => forall(a, f.castParam1[Byte])
				case a :Array[Float]    => forall(a, f.castParam1[Float])
				case a :Array[Short]    => forall(a, f.castParam1[Short])
				case a :Array[Boolean]  => forall(a, f.castParam1[Boolean])
				case null               => throw new NullPointerException()
			}
		}
	}
*/
//
//	def forall[E](array :Array[E], from :Int, until :Int)(f :E => Boolean) :Boolean =
//		indexWhere(array, from, until)(f, 0) == -1
//

	def updateAll[E](array :Array[E], from :Int, until :Int)(f :Int => E) :Int = {
		val length = array.length
		if (until > from & until > 0 & from < length) {
			val from0 = math.max(from, 0)
			val until0 = math.min(until, length)
			def fill[@specialized(Everything) X](a :Array[X], f :Int => X) :Unit = {
				var i = from0
				while (i < until0) {
					a(i) = f(i)
					i += 1
				}
			}
			(array :Array[_]) match {
				case a :Array[AnyRef]   => fill(a, f.castParam2[AnyRef])
				case a :Array[Int]      => fill(a, f.castParam2[Int])
				case a :Array[Long]     => fill(a, f.castParam2[Long])
				case a :Array[Double]   => fill(a, f.castParam2[Double])
				case a :Array[Char]     => fill(a, f.castParam2[Char])
				case a :Array[Byte]     => fill(a, f.castParam2[Byte])
				case a :Array[Float]    => fill(a, f.castParam2[Float])
				case a :Array[Short]    => fill(a, f.castParam2[Short])
				case a :Array[Boolean]  => fill(a, f.castParam2[Boolean])
				case null               => null_!("null array")
			}
			until0 - from0
		} else
			0

	}

	def foldLeft[A, E](array :Array[E], from :Int, until :Int)(z :A)(op :(A, E) => A) :A = 
		if (until <= from)
			z
		else {
			def foldl[@specialized(Specializable.Everything) X](arr :Array[X], op :(A, X) => A) :A = {
				var i   = from
				var res = z
				while (i < until) {
					res = op(res, arr(i))
					i  += 1
				}
				res
			}
			(array :Array[_]) match {
				case a :Array[AnyRef]  => foldl(a, op.asInstanceOf[(A, AnyRef) => A])
				case a :Array[Int]     => foldl(a, op.asInstanceOf[(A, Int) => A])
				case a :Array[Long]    => foldl(a, op.asInstanceOf[(A, Long) => A])
				case a :Array[Double]  => foldl(a, op.asInstanceOf[(A, Double) => A])
				case a :Array[Byte]    => foldl(a, op.asInstanceOf[(A, Byte) => A])
				case a :Array[Char]    => foldl(a, op.asInstanceOf[(A, Char) => A])
				case a :Array[Float]   => foldl(a, op.asInstanceOf[(A, Float) => A])
				case a :Array[Short]   => foldl(a, op.asInstanceOf[(A, Short) => A])
				case a :Array[Boolean] => foldl(a, op.asInstanceOf[(A, Boolean) => A])
				case null              => null_!("null array")
			}
	}

	def foldRight[E, A](array :Array[E], from :Int, until :Int)(z :A)(op :(E, A) => A) :A =
		if (until <= from)
			z
		else {
			def foldr[@specialized(Specializable.Everything) X](arr :Array[X], op :(X, A) => A) :A = {
				var i   = until
				var res = z
				while (i > from) {
					i  -= 1
					res = op(arr(i), res)
				}
				res
			}
			(array :Array[_]) match {
				case a :Array[AnyRef]  => foldr(a, op.asInstanceOf[(AnyRef, A) => A])
				case a :Array[Int]     => foldr(a, op.asInstanceOf[(Int, A) => A])
				case a :Array[Long]    => foldr(a, op.asInstanceOf[(Long, A) => A])
				case a :Array[Double]  => foldr(a, op.asInstanceOf[(Double, A) => A])
				case a :Array[Byte]    => foldr(a, op.asInstanceOf[(Byte, A) => A])
				case a :Array[Char]    => foldr(a, op.asInstanceOf[(Char, A) => A])
				case a :Array[Float]   => foldr(a, op.asInstanceOf[(Float, A) => A])
				case a :Array[Short]   => foldr(a, op.asInstanceOf[(Short, A) => A])
				case a :Array[Boolean] => foldr(a, op.asInstanceOf[(Boolean, A) => A])
				case null              => null_!("null array")
			}
	}
	
	def reduceLeft[A >: E, E](array :Array[E], from :Int, until :Int)(op :(A, E) => A) :A =
		if (until <= from)
			unsupported_!("ArrayLike().reduceLeft")
		else
			foldLeft[A, E](array, from + 1, until)(array(from))(op)

	def reduceRight[E, A >: E](array :Array[E], from :Int, until :Int)(op :(E, A) => A) :A =
		if (until <= from)
			unsupported_!("ArrayLike().reduceRight")
		else
			foldRight[E, A](array, from, until - 1)(array(until - 1))(op)

	def shuffle[E](array :Array[E], from :Int, until :Int)(random :RandomGenerator) :Unit = {
		val from0 = math.max(from, 0)
		val until0 = math.min(until, array.length)
		val downto = from0 + 1

		def swap[@specialized(Specializable.Everything) T](arr :Array[T]) :Unit = {
			var i = until0
			while (i > downto) {
				val j = random.nextInt(i)
				i -= 1
				val boo = arr(i)
				arr(i)  = arr(j)
				arr(j)  = boo
			}
		}
		if (from0 < until0)
			(array :Array[_]) match {
				case a :Array[AnyRef]  => swap(a)
				case a :Array[Int]     => swap(a)
				case a :Array[Long]    => swap(a)
				case a :Array[Double]  => swap(a)
				case a :Array[Byte]    => swap(a)
				case a :Array[Char]    => swap(a)
				case a :Array[Float]   => swap(a)
				case a :Array[Short]   => swap(a)
				case a :Array[Boolean] => swap(a)
				case null              => null_!("null array")
			}
	}

	def addString[E](array :Array[E], b :JStringBuilder, start :String, sep :String, end :String) :Unit = {
		def specAddString[@specialized(Specializable.Everything) A](a :Array[A]) :Unit = {
			if (start.length != 0)
				b append start
			val until = a.length - 1
			var i = 0
			if (until >= 0) {
				while (i < until) {
					b append a(i) //todo :check if this will specialize the call!
					b append sep
					i += 1
				}
				b append a(until)
			}
			if (end.length != 0)
				b append end
		}
		(array :Array[_]) match {
			case a :Array[AnyRef]  => specAddString(a)
			case a :Array[Int]     => specAddString(a)
			case a :Array[Long]    => specAddString(a)
			case a :Array[Double]  => specAddString(a)
			case a :Array[Byte]    => specAddString(a)
			case a :Array[Char]    => specAddString(a)
			case a :Array[Float]   => specAddString(a)
			case a :Array[Short]   => specAddString(a)
			case a :Array[Boolean] => specAddString(a)
			case null              => null_!("null array")
		}
	}


	def boxingCopy[V, B](unboxedSrc :Array[V], srcIdx :Int, boxedDst :Array[B], dstIdx :Int, length :Int) :Unit = {
		if (length > 0) {
			validateCopyIndices(unboxedSrc, srcIdx, boxedDst, dstIdx, length)
			val dst = boxedDst.asInstanceOf[Array[Any]]
			def cpy[@specialized X](src :Array[X]) :Unit = {
				var i = 0
				while (i < length) {
					dst(dstIdx + i) = src(srcIdx + i)
					i += 1
				}
			}
			(unboxedSrc :ArrayLike[Any] @unchecked) match {
				case a :Array[AnyRef]  => cpy(a)
				case a :Array[Int]     => cpy(a)
				case a :Array[Long]    => cpy(a)
				case a :Array[Double]  => cpy(a)
				case a :Array[Char]    => cpy(a)
				case a :Array[Byte]    => cpy(a)
				case a :Array[Float]   => cpy(a)
				case a :Array[Boolean] => cpy(a)
				case a :Array[Short]   => cpy(a)
			}
		}
	}

	def unboxingCopy[B, V](boxedSrc :Array[B], srcIdx :Int, unboxedDst :Array[V], dstIdx :Int, length :Int) :Unit =
		if (length > 0) {
			validateCopyIndices(boxedSrc, srcIdx, unboxedDst, dstIdx, length)
			val src = boxedSrc.asInstanceOf[Array[Any]]
			def cpy[@specialized X](dst :Array[X]) :Unit = {
				var i = 0
				while (i < length) {
					dst(dstIdx + i) = src(srcIdx + i).asInstanceOf[X]
					i += 1
				}
			}
			(unboxedDst :ArrayLike[Any] @unchecked) match {
				case a :Array[AnyRef]  => cpy(a)
				case a :Array[Int]     => cpy(a)
				case a :Array[Long]    => cpy(a)
				case a :Array[Double]  => cpy(a)
				case a :Array[Char]    => cpy(a)
				case a :Array[Byte]    => cpy(a)
				case a :Array[Float]   => cpy(a)
				case a :Array[Boolean] => cpy(a)
				case a :Array[Short]   => cpy(a)
			}
		}

	@inline def validateCopyIndices(src :Array[_], srcStart :Int, dst :Array[_], dstStart :Int, length :Int) :Unit =
		if (srcStart < 0 | dstStart < 0 | srcStart > src.length - length | dstStart > dst.length - length)
			outOfBounds_!(
				"ArrayLike.copy(" + errorString(src) + ", " + srcStart + ", " +
					errorString(dst) + ", " + dstStart + ", " + length + ")"
			)

	/** Maps all classes representable in `JVM` to their boxed representations. */
	private[this] val Unbox = Map[Class[_], Class[_]](
		classOf[java.lang.Byte] -> classOf[Byte],
		classOf[java.lang.Short] -> classOf[Short],
		classOf[java.lang.Character] -> classOf[Char],
		classOf[java.lang.Integer] -> classOf[Int],
		classOf[java.lang.Long] -> classOf[Long],
		classOf[java.lang.Float] -> classOf[Float],
		classOf[java.lang.Double] -> classOf[Double],
		classOf[java.lang.Boolean] -> classOf[Boolean],
	) withDefault identity[Class[_]]

}




private object BinarySearch {

	@tailrec def apply(array :Array[Byte], from :Int, until :Int, key :Byte) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Short], from :Int, until :Int, key :Short) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Char], from :Int, until :Int, key :Char) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Int], from :Int, until :Int, key :Int) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Long], from :Int, until :Int, key :Long) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Float], from :Int, until :Int, key :Float) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[Double], from :Int, until :Int, key :Double) :Int =
		if (until == from) {
			val candidate = array(from)
			if (key == candidate) from
			else if (key < candidate) -from - 1
			else -from - 2
		} else {
			val middle = from + (until - from >> 1)
			if (key <= array(middle)) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply(array :Array[AnyRef], from :Int, until :Int, key :AnyRef)(implicit ordering :Ordering[AnyRef]) :Int =
		if (until == from)
			math.signum(ordering.compare(key, array(from))) match {
				case  0 => from
				case -1 => -from - 1
				case  1 => -from - 2
			}
		else {
			val middle = from + (until - from >> 1)
			if (ordering.lteq(key, array(middle))) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

	@tailrec def apply[X, U >: X](array :Array[X], from :Int, until :Int, key :U)(implicit ordering :Ordering[U]) :Int =
		if (until == from)
			math.signum(ordering.compare(key, array(from))) match {
				case  0 => from
				case -1 => -from - 1
				case  1 => -from - 2
			}
		else {
			val middle = from + (until - from >> 1)
			if (ordering.lteq(key, array(middle))) apply(array, from, middle, key)
			else apply(array, middle + 1, until, key)
		}

}
