package net.noresttherein.sugar.numeric




/** A facade for a random number generator given as the by-name argument, returning values of other types than `Double`
  * and other ranges.
  * @param generator a by-name expression returning values from range `[0.0, 1.0)`.
  */
class random(generator: => Double) {
	/** A random `Int` from the whole range. */
	def Int :Int = IntInclusive(scala.Int.MinValue, scala.Int.MaxValue)

	/** A random `Int` from range `[0, until)`. */
	def Int(until :Int) :Int = Int(0, until)

	/** A random `Int` from range `[start, end)`. */
	def Int(start :Int, until :Int) :Int =
		if (until == scala.Int.MinValue)
			throw new IllegalArgumentException("Cannot return a random Int from range[" + start + ", " + until + "].")
		else
			IntInclusive(start, until - 1)

	/** A random `Int` from range `[start, to]`. */
	def IntInclusive(start :Int, to :Int) :Int = LongInclusive(start, to).toInt

	/** A random `Long` from the whole range. */
	def Long :Long = LongInclusive(scala.Long.MinValue, scala.Long.MaxValue)

	/** A random `Long` from range `[0, until)`. */
	def Long(until :Long) :Long = Long(0L, until)

	/** A random `Long` from range `[start, until)`. */
	def Long(start :Long, until :Long) :Long =
		if (until == scala.Long.MinValue)
			throw new IllegalArgumentException("Cannot return a random number from range[" + start + ", " + until + "].")
		else
			LongInclusive(start, until - 1)

	/** A random `Long` from range `[start, to]` */
	def LongInclusive(start :Long, to :Long) :Long =
		if (to < start)
			throw new IllegalArgumentException("Cannot return a random number from range[" + start + ", " + to + "].")
		else
			(generator * (to.toDouble - start.toDouble) + start).round
}




/** Generates random numbers from arbitrary ranges. */
@SerialVersionUID(Ver) //consider: maybe it should be in numeric?
object random extends random(scala.math.random())
