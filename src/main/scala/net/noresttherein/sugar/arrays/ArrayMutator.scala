





package net.noresttherein.sugar.arrays

import scala.collection.AbstractIterator

import net.noresttherein.sugar.collections.Mutator
import net.noresttherein.sugar.{illegalState_!, noSuch_!, unsupported_!}
import net.noresttherein.sugar.reflect.Specialized.MultiValue

/**
  * @author Marcin Mościcki
  */






//todo: make private, separately compiled.
/** An iterator advancing over a slice of an array. The advantages over built in array
  * [[collection.ArrayOps.iterator iterator]] are fast, in-place `take`, and `copyToArray` delegating to `Array.copy`,
  * making it considerably faster than copying by one.
  * @param first    the index in the array of the first/next element to return.
  * @param `last++` the index in the array delimiting the iterator, that is pointing after the last element
  *                 the iterator should return.
  */
@SerialVersionUID(Ver)
private[sugar] sealed class ArrayMutator[@specialized(MultiValue) T] private[sugar]
                                         (array :Array[T], private[this] var first :Int, private[this] var `last++` :Int)
	extends AbstractIterator[T] with ArrayIteratorOps[T] with Mutator[T] with Serializable
{
	def this(array :Array[T]) = this(array, 0, array.length)

	private[sugar] final override def unsafeArray :Array[_] = array
	final override def index :Int = first
	protected final override def index_=(i :Int) :Unit = first = i
	final override def limit :Int = `last++`
	protected final override def limit_=(i :Int) :Unit = `last++` = i

//	def reverse :ReverseArrayIterator[T] = new ReverseArrayIterator[T](array, first, `last++`)

	final override def hasNext :Boolean = first < `last++`
	override def head :T =
		if (first < `last++`) array(first)
		else noSuch_!("Index " + first + " exceeds the limit of " + `last++` + '.')

	override def head_=(value :T) :Unit =
		if (first < `last++`) array(first) = value
		else illegalState_!("Empty mutator")

	override def next() :T = {
		if (first >= `last++`)
			noSuch_!("Index " + first + " exceeds the limit of " + `last++` + ".")
		val res = array(first)
		first += 1
		res
	}
	override def remove() :Unit = unsupported_!("Cannot remove an element from an array")

	override def clone = new ArrayMutator(array, first, `last++`)
}



@SerialVersionUID(Ver)
case object ArrayMutator extends ArrayLikeIteratorFactory[Array, ArrayMutator] {
	protected override def make[E](array :Array[E], from :Int, until :Int) :ArrayMutator[E] =
		((array :ArrayLike[_]) match {
			case a :Array[AnyRef]    => new ArrayMutator(a, from, until)
			case a :Array[Int]       => new ArrayMutator(a, from, until)
			case a :Array[Long]      => new ArrayMutator(a, from, until)
			case a :Array[Double]    => new ArrayMutator(a, from, until)
			case a :Array[Char]      => new ArrayMutator(a, from, until)
			case a :Array[Byte]      => new ArrayMutator(a, from, until)
			case a :Array[Float]     => new ArrayMutator(a, from, until)
			case a :Array[Short]     => new ArrayMutator(a, from, until)
			case a :Array[Boolean]   => new ArrayMutator(a, from, until)
		}).asInstanceOf[ArrayMutator[E]]

	override def isMutable = true
}