package net.noresttherein.sugar.collections
import scala.collection.Stepper.EfficientSplit
import scala.collection.{Stepper, StepperShape}




/** A mixin traits for mutable and immutable `IndexedSeq` implementations which overrides `IterableOnceOps` methods
  * to use `apply`, rather than `iterator`.
  */
trait ApplyPreferredSeqOps[+E, +CC[_], +C <: collection.IndexedSeq[E]]
	extends collection.IndexedSeqOps[E, CC, C] with collection.StrictOptimizedSeqOps[E, CC, C]
{
	override def iterator :Iterator[E] = IndexedSeqIterator(coll)
	override def reverseIterator :Iterator[E] = ReverseIndexedSeqIterator(coll)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		IndexedSeqStepper(coll)

//	override def head :E =
//		if (length == 0) noSuch_!(toString + ".head")
//		else apply(0)
//
//	override def last :E = {
//		val len = length
//		if (len == 0) noSuch_!(toString + "().last")
//		else apply(len - 1)
//	}

	override def segmentLength(p :E => Boolean, from :Int) :Int = segmentLength(p, from, false)
	private def segmentLength(p :E => Boolean, from :Int, flipped :Boolean) :Int = {
		var i = from; val len = length
		while (i < len && p(apply(i)) != flipped)
			i += 1
		i
	}
	override def indexWhere(p :E => Boolean, from :Int) :Int = {
		val len = segmentLength(p, from, true)
		if (len == length) -1 else len
	}
	override def lastIndexWhere(p :E => Boolean, end :Int) :Int = {
		var i = math.min(length - 1, end)
		while (i >= 0 && !p(apply(i)))
			i -= 1
		if (i < 0) -1 else i
	}

	override def find(p :E => Boolean) :Option[E] = indexWhere(p) match {
		case -1 => None
		case  i => Some(apply(i))
	}
	override def findLast(p :E => Boolean) :Option[E] = lastIndexWhere(p) match {
		case -1 => None
		case  i => Some(apply(i))
	}


	override def filter(pred :E => Boolean) :C = filter(pred, false)
	override def filterNot(pred :E => Boolean) :C = filter(pred, true)
	private def filter(pred :E => Boolean, flipped :Boolean) :C = {
		val res = newSpecificBuilder
		val len = length
		var i   = 0
		while (i < len) {
			val elem = apply(i)
			if (pred(elem) != flipped)
				res += elem
			i += 1
		}
		res.result()
	}
	override def partition(p :E => Boolean) :(C, C) = {
		val yes = newSpecificBuilder
		val no  = newSpecificBuilder
		val len = length
		var i   = 0
		while (i < len) {
			val elem = apply(i)
			if (p(elem)) yes += elem
			else no += elem
			i += 1
		}
		(yes.result(), no.result())
	}
	override def count(p :E => Boolean) :Int = {
		var i, res = 0; val len = length
		while (i < len) {
			if (p(apply(i)))
				res += 1
			i += 1
		}
		res
	}

	override def takeWhile(p :E => Boolean) :C = take(segmentLength(p))
	override def span(p :E => Boolean) :(C, C) = splitAt(segmentLength(p))
	override def dropWhile(p :E => Boolean) :C = drop(segmentLength(p))

	override def forall(p :E => Boolean) :Boolean = segmentLength(p) == length
    override def exists(p :E => Boolean) :Boolean = indexWhere(p, 0) >= 0

}
