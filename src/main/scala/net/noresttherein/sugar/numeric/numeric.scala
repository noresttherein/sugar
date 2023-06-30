package net.noresttherein.sugar

import java.lang.{Math=>math}
import java.util.concurrent.ThreadLocalRandom

import scala.util.Random


//todo: UInt, ULong, UShort, UByte
package object numeric {
	private[numeric] final val Ver = 1L

	@inline def max(a :Int, b :Int) :Int = math.max(a, b)
	@inline def max(a :Long, b :Long) :Long = math.max(a, b)
	@inline def max(a :Double, b :Double) :Double = math.max(a, b)
	@inline def max(a :Float, b :Float) :Float = math.max(a, b)
	@inline def max(a :Short, b :Short) :Short = math.max(a, b).toShort
	@inline def max(a :Byte, b :Byte) :Byte = math.max(a, b).toByte
	@inline def max[T](a :T, b :T)(implicit ordering :Ordering[T]) :T = ordering.max(a, b)

	@inline def max[T](first :T, second :T, rest :T*)(implicit ordering :Ordering[T]) :T =
		if (rest.isEmpty) ordering.max(first, second)
		else ordering.max(first, ordering.max(second, rest.max)) 

	@inline def min(a :Int, b :Int) :Int = math.min(a, b)
	@inline def min(a :Long, b :Long) :Long = math.min(a, b)
	@inline def min(a :Double, b :Double) :Double = math.min(a, b)
	@inline def min(a :Float, b :Float) :Float = math.min(a, b)
	@inline def min(a :Short, b :Short) :Short = math.min(a, b).toShort
	@inline def min(a :Byte, b :Byte) :Byte = math.min(a, b).toByte
	@inline def min[T](a :T, b :T)(implicit ordering :Ordering[T]) :T = ordering.min(a, b)

	@inline def min[T](first :T, second :T, rest :T*)(implicit ordering :Ordering[T]) :T =
		if (rest.isEmpty) ordering.min(first, second)
		else ordering.min(first, ordering.min(second, rest.min)) 
		
	
	implicit val globalRandom      :Random = new Random(new java.util.Random())

	implicit val threadLocalRandom :Random = new Random(
		new java.util.Random() {
			import ThreadLocalRandom.current
			override def isDeprecated = current.isDeprecated
//			override def setSeed(seed :Long) = current.setSeed(seed)

			override def nextBytes(bytes :Array[Byte]) = current.nextBytes(bytes)
			override def nextBoolean = current.nextBoolean
			override def nextInt = current.nextInt
			override def nextInt(bound :Int) = current.nextInt(bound)
			override def nextInt(origin :Int, bound :Int) = current.nextInt(origin, bound)
			override def nextLong = current.nextLong
			override def nextLong(bound :Long) = current.nextLong(bound)
			override def nextLong(origin :Long, bound :Long) = current.nextLong(origin, bound)
			override def nextFloat = current.nextFloat
			override def nextFloat(bound :Float) = current.nextFloat(bound)
			override def nextFloat(origin :Float, bound :Float) = current.nextFloat(origin, bound)
			override def nextDouble = current.nextDouble
			override def nextDouble(bound :Double) = current.nextDouble(bound)
			override def nextDouble(origin :Double, bound :Double) = current.nextDouble(origin, bound)
			override def nextGaussian = current.nextGaussian

			override def ints = current.ints
			override def ints(streamSize :Long) = current.ints(streamSize)
			override def ints(streamSize :Long, randomNumberOrigin :Int, randomNumberBound :Int) =
				current.ints(streamSize, randomNumberOrigin, randomNumberBound)

			override def ints(randomNumberOrigin :Int, randomNumberBound :Int) =
				current.ints(randomNumberOrigin, randomNumberBound)

			override def longs = current.longs
			override def longs(streamSize :Long) = current.longs(streamSize)
			override def longs(streamSize :Long, randomNumberOrigin :Long, randomNumberBound :Long) =
				current.longs(streamSize, randomNumberOrigin, randomNumberBound)

			override def longs(randomNumberOrigin :Long, randomNumberBound :Long) =
				current.longs(randomNumberOrigin, randomNumberBound)

			override def doubles = current.doubles
			override def doubles(streamSize :Long) = current.doubles(streamSize)
			override def doubles(streamSize :Long, randomNumberOrigin :Double, randomNumberBound :Double) =
				current.doubles(streamSize, randomNumberOrigin, randomNumberBound)

			override def doubles(randomNumberOrigin :Double, randomNumberBound :Double) =
				current.doubles(randomNumberOrigin, randomNumberBound)

			override def nextGaussian(mean :Double, stddev :Double) = current.nextGaussian(mean, stddev)
			override def nextExponential() = current.nextExponential()
		}
	)
//	implicit def threadLocalRandom :Random = new Random(ThreadLocalRandom.current())
}
