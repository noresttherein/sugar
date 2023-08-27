package net.noresttherein.sugar.collections

import scala.Specializable.Everything

import net.noresttherein.sugar.extensions.cast2TypeParamsMethods




/** Includes utility methods creating, and working on arrays.
  * These are primarily duplicates or overloads of methods in object `Array`,
  * and methods which internally use specialization. Separate compilation with Scala 2
  * allows to use them also in Scala 3, without needing to resort to 'manual specialization'.
  * @author Marcin Mościcki
  */
private object ArrayLikeOps {
	def foreach[E, U](array :Array[E], from :Int, until :Int)(f :E => U) :Unit = {
		def forEach[@specialized(Everything) X](arr :Array[X])(f :X => U) :Unit = {
			var i = from
			while (i < until) {
				f(arr(i))
				i += 1
			}
		}
		val g = f.asInstanceOf[Any => U]
		array match {
			case a :Array[AnyRef]   => forEach(a)(g)
			case a :Array[Int]      => forEach(a)(g)
			case a :Array[Long]     => forEach(a)(g)
			case a :Array[Double]   => forEach(a)(g)
			case a :Array[Char]     => forEach(a)(g)
			case a :Array[Byte]     => forEach(a)(g)
			case a :Array[Float]    => forEach(a)(g)
			case a :Array[Short]    => forEach(a)(g)
			case a :Array[Boolean]  => forEach(a)(g)
		}
	}
	
	def indexOf[E](array :Array[_ <: E], offset :Int, length :Int)(elem :E, from :Int) :Int = {
		def index[@specialized(Everything) X](arr :Array[X], elem :Any, from :Int) :Int = {
			val end = offset + length
			var i = offset + from
			if (array.getClass.getComponentType isAssignableFrom Unbox(elem.getClass)) {
				val x = elem.asInstanceOf[X]
				while (i < end && arr(i) != elem)
					i += 1
			} else
				while (i < end && arr(i) != elem)
					i += 1
			if (i == end) -1 else i
		}
		if (from >= length)
			-1
		else array match {
			case a :Array[AnyRef]  => index(a, elem, from)
			case a :Array[Int]     => index(a, elem, from)
			case a :Array[Long]    => index(a, elem, from)
			case a :Array[Double]  => index(a, elem, from)
			case a :Array[Char]    => index(a, elem, from)
			case a :Array[Byte]    => index(a, elem, from)
			case a :Array[Float]   => index(a, elem, from)
			case a :Array[Short]   => index(a, elem, from)
			case a :Array[Boolean] => index(a, elem, from)
		}
	}

	def lastIndexOf[E](array :Array[_ <: E], offset :Int, length :Int)(elem :E, end :Int) :Int = {
		def lastIndex[@specialized(Everything) X](arr :Array[X], elem :Any, start :Int) :Int = {
			var i = start
			if (array.getClass.getComponentType isAssignableFrom Unbox(elem.getClass)) {
				val x = elem.asInstanceOf[X]
				while (i >= offset && arr(i) != elem)
					i -= 1
			} else
				while (i >= offset && arr(i) != elem)
					i -= 1
			if (i < offset) -1 else i
		}
		if (end < 0)
			-1
		else array match {
			case a :Array[AnyRef]  => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Int]     => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Long]    => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Double]  => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Char]    => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Byte]    => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Float]   => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Short]   => lastIndex(a, elem, math.min(end, length - 1))
			case a :Array[Boolean] => lastIndex(a, elem, math.min(end, length - 1))
		}
	}

	def lastIndexWhere[E](array :Array[E], offset :Int, length :Int)(p :E => Boolean, end :Int) :Int = {
		def lastIdx[@specialized(Everything) X](arr :Array[X], p :X => Boolean, from :Int, until :Int) :Int = {
			var i = until
			while (i > from && !p(arr(i)))
				i -= 1
			if (i < from) -1 else i
		}
		val start = math.min(end, length - 1)
		if (end < 0)
			0
		else array match {
			case a :Array[AnyRef]  => lastIdx(a, p.asInstanceOf[AnyRef => Boolean], offset, end)
			case a :Array[Int]     => lastIdx(a, p.asInstanceOf[Int => Boolean], offset, end)
			case a :Array[Long]    => lastIdx(a, p.asInstanceOf[Long => Boolean], offset, end)
			case a :Array[Double]  => lastIdx(a, p.asInstanceOf[Double => Boolean], offset, end)
			case a :Array[Char]    => lastIdx(a, p.asInstanceOf[Char => Boolean], offset, end)
			case a :Array[Byte]    => lastIdx(a, p.asInstanceOf[Byte => Boolean], offset, end)
			case a :Array[Float]   => lastIdx(a, p.asInstanceOf[Float => Boolean], offset, end)
			case a :Array[Short]   => lastIdx(a, p.asInstanceOf[Short => Boolean], offset, end)
			case a :Array[Boolean] => lastIdx(a, p.asInstanceOf[Boolean => Boolean], offset, end)
		}
	}

	def indexWhere[E](array :Array[E], offset :Int, length :Int)(p :E => Boolean, from :Int) :Int =
		if (from > length)
			-1
		else {
			val from0 = math.max(from, 0)
			val res = segmentLength(array, p, offset + from0, offset + length, true)
			if (res == length - from0) -1 else res + from0
		}

	@inline def segmentLength[E](array :Array[E], offset :Int, length :Int)(p :E => Boolean, from :Int) :Int =
		if (from > length)
			0
		else
			segmentLength(array, p, offset + math.max(from, 0), offset + length)

	def segmentLength[E](array :Array[E], p :E => Boolean, from :Int, until :Int, flipped :Boolean = false) :Int = {
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
		}
	}

	def updateAll[E](array :Array[E], from :Int, until :Int)(f :Int => E) :Unit = {
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
			}
		}

	}

	def reduceLeft[A >: E, E](array :Array[E])(op :(A, E) => A) :A = {
		if (array.length == 0)
			throw new UnsupportedOperationException("ArrayLike().reduceLeft")
		def reducel[@specialized(Specializable.Everything) X](arr :Array[X], op :(Any, X) => Any) :A = {
			var i = 1; val len = arr.length
			var res :Any = arr(0)
			while (i < len) {
				res = op(res, arr(i))
				i += 1
			}
			res.asInstanceOf[A]
		}
		(array :Array[_]) match {
			case a :Array[AnyRef]  => reducel(a, op.asInstanceOf[(Any, AnyRef) => Any])
			case a :Array[Int]     => reducel(a, op.asInstanceOf[(Any, Int) => Any])
			case a :Array[Long]    => reducel(a, op.asInstanceOf[(Any, Long) => Any])
			case a :Array[Double]  => reducel(a, op.asInstanceOf[(Any, Double) => Any])
			case a :Array[Byte]    => reducel(a, op.asInstanceOf[(Any, Byte) => Any])
			case a :Array[Char]    => reducel(a, op.asInstanceOf[(Any, Char) => Any])
			case a :Array[Float]   => reducel(a, op.asInstanceOf[(Any, Float) => Any])
			case a :Array[Short]   => reducel(a, op.asInstanceOf[(Any, Short) => Any])
			case a :Array[Boolean] => reducel(a, op.asInstanceOf[(Any, Boolean) => Any])
		}
	}

	def reduceRight[E, A >: E](array :Array[E])(op :(E, A) => A) :A = {
		if (array.length == 0)
			throw new UnsupportedOperationException("ArrayLike().reduceRight")
		def reducer[@specialized(Specializable.Everything) X](arr :Array[X], op :(X, Any) => Any) :A = {
			val len = arr.length
			var i = len - 1
			var res :Any = arr(i)
			while (i > 0) {
				i -= 1
				res = op(arr(i), res)
			}
			res.asInstanceOf[A]
		}
		(array :Array[_]) match {
			case a :Array[AnyRef]  => reducer(a, op.asInstanceOf[(AnyRef, Any) => Any])
			case a :Array[Int]     => reducer(a, op.asInstanceOf[(Int, Any) => Any])
			case a :Array[Long]    => reducer(a, op.asInstanceOf[(Long, Any) => Any])
			case a :Array[Double]  => reducer(a, op.asInstanceOf[(Double, Any) => Any])
			case a :Array[Byte]    => reducer(a, op.asInstanceOf[(Byte, Any) => Any])
			case a :Array[Char]    => reducer(a, op.asInstanceOf[(Char, Any) => Any])
			case a :Array[Float]   => reducer(a, op.asInstanceOf[(Float, Any) => Any])
			case a :Array[Short]   => reducer(a, op.asInstanceOf[(Short, Any) => Any])
			case a :Array[Boolean] => reducer(a, op.asInstanceOf[(Boolean, Any) => Any])
		}
	}

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
