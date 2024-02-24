package net.noresttherein.sugar.exceptions

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import net.noresttherein.sugar.extensions.classNameMethods


/** Validators for ubiquitous argument constraints. */
@SerialVersionUID(Ver)
object validate { //consider: macros which use the argument expression as the basis of the error string

	/** Throws a [[NullPointerException]] if `x` is `null`. Otherwise simply returns `x`. */
	@inline def notNull[X](name :String, x :X) :X =
		if (x == null) null_!(name + " is null")
		else x


	/** Throws an [[IllegalArgumentException]] if `x` is negative. Otherwise simply returns `x`. */
	@inline def nonNegative(name :String, x :Int) :Int =
		if (x < 0) illegal_!(name + " is negative: " + x)
		else x

	/** Throws an [[IllegalArgumentException]] if `x` is negative. Otherwise simply returns `x`. */
	@inline def nonNegative(name :String, x :Long) :Long =
		if (x < 0) illegal_!(name + " is negative: " + x)
		else x

	/** Throws an [[IllegalArgumentException]] if `x` is negative. Otherwise simply returns `x`. */
	@inline def nonNegative(name :String, x :Double) :Double =
		if (x < 0) illegal_!(name + " is negative: " + x)
		else x

	/** Throws an [[IllegalArgumentException]] if `x` is negative. Otherwise simply returns `x`. */
	@inline def nonNegative[X](name :String, x :X)(implicit numeric :Numeric[X]) :X =
		if (numeric.lt(x, numeric.zero)) illegal_!(name + " is negative: " + x)
		else x


	/** Throws an [[IllegalArgumentException]] if `x` equals `0`. Otherwise simply returns `x` */
	@inline def nonZero(name :String, x :Int) :Int =
		if (x == 0) illegal_!(name + " is zero")
		else x

	/** Throws an [[IllegalArgumentException]] if `x` equals `0`. Otherwise simply returns `x` */
	@inline def nonZero(name :String, x :Long) :Long =
		if (x == 0) illegal_!(name + " is zero")
		else x

	/** Throws an [[IllegalArgumentException]] if `x` equals `0`. Otherwise simply returns `x` */
	@inline def nonZero(name :String, x :Double) :Double =
		if (x == 0) illegal_!(name + " is zero")
		else x

	/** Throws an [[IllegalArgumentException]] if `x` equals `0`. Otherwise simply returns `x` */
	@inline def nonZero[X](name :String, x :X)(implicit numeric :Numeric[X]) :X =
		if (numeric.equiv(x, numeric.zero)) illegal_!(name + " is zero")
		else x


	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange(name :String, x :Int, min :Int, max :Int) :Int = //consider renaming to withinBounds
		if (x < min | x > max) outOfBounds_!(name + " is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange(x :Int, min :Int, max :Int) :Int =
		if (x < min | x > max) outOfBounds_!("index is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange(name :String, x :Long, min :Long, max :Long) :Long =
		if (x < min | x > max) outOfBounds_!(name + " is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange(x :Long, min :Long, max :Long) :Long =
		if (x < min | x > max) outOfBounds_!("index is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange(name :String, x :Double, min :Double, max :Double) :Double =
		if (x < min | x > max) outOfBounds_!(name + " is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange(x :Double, min :Double, max :Double) :Double =
		if (x < min | x > max) outOfBounds_!("index is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange[X](name :String, x :X, min :X, max :X)(implicit ordering :Ordering[X]) :X =
		if (ordering.lt(x, min)  || ordering.gt(x, max))
			outOfBounds_!(name + " is out of range [" + min + ".." + max + "]: " + x)
		else x

	/** Throws an [[IndexOutOfBoundsException]] if `x` is outside the given range (both bounds are inclusive). */
	@inline def inRange[X](x :X, min :X, max :X)(implicit ordering :Ordering[X]) :X =
		if (ordering.lt(x, min)  || ordering.gt(x, max))
			outOfBounds_!("index is out of range [" + min + ".." + max + "]: " + x)
		else x

//	 /** Throws an [[IllegalArgumentException]] if `length` is negative, or an [[IndexOutOfBoundsException]]
//	   * if either `length > max - min + 1` or `x` is out of range `[min..max-length]` (inclusive). Otherwise returns `x`.
//	   */
//	 def inRange(xName :String, x :Int, lengthName :String, length :Int, min :Int, max :Int) :Int =
//	 	if (length < 0)
//	 		illegal_!(lengthName + " is negative: " + length)
//	 	else if (length > max - min + 1)
//	 		outOfBounds_!(lengthName + " is longer than range [" + min + ".." + max + "]")
//	 	else if (x < min | x > max - length)
//	 		outOfBounds_!(xName + " is out of range [" + min + ".." + max + "-" + length + "]")
//	 	else x
//
//	 /** Throws an [[IllegalArgumentException]] if `length` is negative, or an [[IndexOutOfBoundsException]]
//	   * if either `length > max - min + 1` or `x` is out of range `[min..max-length]`. Otherwise returns `x`.
//	   */
//	 def inRange(xName :String, x :Long, lengthName :String, length :Long, min :Long, max :Long) :Long =
//	 	if (length < 0)
//	 		illegal_!(lengthName + " is negative: " + length)
//	 	else if (length > max - min + 1)
//	 		outOfBounds_!(lengthName + " is longer than range [" + min + ".." + max + "]")
//	 	else if (x < min | x > max - length)
//	 		outOfBounds_!(xName + " is out of range [" + min + ".." + max + "-" + length + "]")
//	 	else x
//
//	 /** Throws an [[IllegalArgumentException]] if `length` is negative, or an [[IndexOutOfBoundsException]]
//	   * if either `length > max - min` or `x` is out of range `[min..max-length]`. Otherwise returns `x`.
//	   */
//	 def inRange(xName :String, x :Double, lengthName :String, length :Double, min :Double, max :Double) :Double =
//	 	if (length < 0)
//	 		illegal_!(lengthName + " is negative: " + length)
//	 	else if (length > max - min)
//	 		outOfBounds_!(lengthName + " is longer than range [" + min + ".." + max + "]")
//	 	else if (x < min | x > max - length)
//	 		outOfBounds_!(xName + " is out of range [" + min + ".." + max + "-" + length + "]")
//	 	else x
//
//	 /** Throws an [[IllegalArgumentException]] if `length` is negative, or a [[IndexOutOfBoundsException]]
//	   * if either `length > max - min` or `x` is out of range `[min..max-length]`. Otherwise returns `x`.
//	   */
//	 def inRange[X](xName :String, x :X, lengthName :String, length :X, min :X, max :X)(implicit numeric :Integral[X]) :X =
//	 	if (numeric.lt(length, numeric.zero))
//	 		illegal_!(lengthName + " is negative: " + length)
//	 	else if (numeric.gt(length, numeric.plus(numeric.one, numeric.minus(max, min))))
//	 		outOfBounds_!(lengthName + " is longer than range [" + min + ".." + max + "]")
//	 	else if (numeric.lt(x, min) || numeric.gt(x, numeric.plus(numeric.one, numeric.minus(max, length))))
//	 		outOfBounds_!(xName + " is out of range [" + min + ".." + max + "-" + length + "]")
//	 	else x

	/** Throws [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]]
	  * if `delta > limit - coll.size`. */
	@inline def sizeLimit(coll :Iterable[_], delta :Int, limit :Int) :Unit =
		if (delta > limit - coll.size)
			maxSize_!(coll, delta, limit)

	/** Throws [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]]
	  * if `delta > Int.MaxValue - coll.size`. */
	@inline def sizeLimit(coll :Iterable[_], delta :Int) :Unit =
		if (delta > Int.MaxValue - coll.size)
			maxSize_!(coll, delta)

	/** Throws [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]]
	  * if `extra.knownSize > limit - coll.size`. */
	@inline def sizeLimit(coll :Iterable[_], extra :IterableOnce[_], limit :Int) :Unit =
		if (extra.knownSize > limit - coll.size)
			maxSize_!(coll, extra, limit)

	/** Throws [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]]
	  * if `extra.knownSize > Int.MaxValue - coll.size`. */
	@inline def sizeLimit(coll :Iterable[_], extra :IterableOnce[_]) :Unit =
		if (extra.knownSize > Int.MaxValue - coll.size)
			maxSize_!(coll, extra)



	@elidable(ASSERTION)
	def assertEqual(x :Any, y :Any) :Unit =
		if (x != y)
			throw new AssertionError(
				x.toString + ": " + x.localClassName + " does not equal " + y + ": " + y.localClassName + "."
			)

	@elidable(ASSERTION)
	def assertEqual(x :Any, y :Any, msg: => Any) :Unit =
		if (x != y)
			throw new AssertionError(
				msg.toString + ": " + x + " (" + x.abbrevClassName + ") does not equal " +
					y + " (" + y.abbrevClassName + ")."
			)
}
