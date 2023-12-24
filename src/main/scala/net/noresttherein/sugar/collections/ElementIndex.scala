package net.noresttherein.sugar.collections

import scala.collection.Searching
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}

import net.noresttherein.sugar.vars.IntOpt.{AnInt, NoInt}
import net.noresttherein.sugar.vars.{IntOpt, Opt}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A lighter alternative to [[scala.collection.Searching.SearchResult SearchResult]],
  * which, additionally, allows the possibility of not specifying an insertion point for 'not found' results.
  * This makes it possible to use for unordered, linear search results.
  * @author Marcin Mościcki
  */ //consider: moving to vars
@SerialVersionUID(Ver)
class ElementIndex private[sugar] (private val idx :Int) extends AnyVal with Serializable {
	def insertionPoint :IntOpt =
		if (idx < 0)
			if (idx == Int.MinValue) NoInt else AnInt(-idx - 1)
		else AnInt(idx)

	final def predecessor :IntOpt =
		if (idx < 0)
			if (idx == Int.MinValue) NoInt else AnInt(-idx - 1)
		else AnInt(idx)

	@inline def isFound  :Boolean = idx >= 0
	@inline def notFound :Boolean = idx < 0
	@inline def index    :IntOpt = if (idx < 0) NoInt else AnInt(idx)
	@inline def toOpt    :Opt[Int] = if (idx < 0) Lack else Got(idx)
	@inline def toOption :Option[Int] = if (idx < 0) None else Some(idx)
	def toEither         :Either[Opt[Int], Int] =
		if (idx < 0)
			if (idx == Int.MinValue) Left(Lack)
			else Left(Got(-idx - 1))
		else Right(idx)
//	def toPill :Pill[Opt[Int], Int] =
//		if (idx < 0)
//			if (idx == Int.MinValue) Red(Lack)
//			else Red(Got(-idx - 1))
//		else Blue(idx)

	@throws[UnsupportedOperationException]("if insertion point was not specified (and the element has not been found).")
	def toSearchResult :SearchResult =
		if (idx < 0)
			if (idx == Int.MinValue) throw new UnsupportedOperationException("Unspecified insertion point")
			else InsertionPoint(-idx - 1)
		else Searching.Found(idx)

	override def toString :String =
		if (idx < 0)
			if (idx == Int.MinValue) "NotFound()" else "NotFound(" + (-idx - 1) + ")"
		else
			"Found(" + idx + ")"
}


@SerialVersionUID(Ver)
object ElementIndex {
	def apply(result :SearchResult) :ElementIndex = result match {
		case Found(idx)          => new ElementIndex(idx)
		case InsertionPoint(idx) => new ElementIndex(-idx - 1)
	}

	@inline def unapply(result :ElementIndex) :IntOpt = result.insertionPoint

	@SerialVersionUID(Ver)
	object Present {
		@inline def apply(idx :Int) :ElementIndex =
			if (idx < 0) rejectNegative(idx)
			else new ElementIndex(idx)

		@inline def unapply(result :ElementIndex) :IntOpt = result.index

	}

	@SerialVersionUID(Ver)
	object Absent {
		@inline def apply(insertionPoint :Int) :ElementIndex =
			if (insertionPoint < 0)
				rejectNegative(insertionPoint)
			else if (insertionPoint == Int.MaxValue)
				rejectMaxInt()
			else
				new ElementIndex(-insertionPoint - 1)

		@inline def apply() :ElementIndex = new ElementIndex(Int.MinValue)

		@inline def unapply(result :ElementIndex) :Boolean = !result.isFound

		//private[Absent] for inlining
		private[Absent] def rejectMaxInt() :Nothing =
			throw new IllegalArgumentException("Cannot possibly insert at index Int.MaxValue")
	}

	@SerialVersionUID(Ver)
	object Predecessor {
		@inline def apply(insertionPoint :Int) :ElementIndex = Absent(insertionPoint)

		@inline def unapply(result :ElementIndex) :IntOpt = result.predecessor
	}

	//private[ElementIndex] for inlining
	private[ElementIndex] def rejectNegative(index :Int) :Nothing =
		throw new IllegalArgumentException("Negative index: " + index)



	private[sugar] def indexOfNotFound(self :String, x :Any, from :Int) :Nothing =
		throw new NoSuchElementException(indexOfErrorMessage(self, x, from))

	private[sugar] def lastIndexOfNotFound(self :String, length :Int, x :Any, end :Int) :Nothing =
		throw new NoSuchElementException(lastIndexOfErrorMessage(self, length, x, end))

	private[sugar] def indexWhereNotFound(self :String, from :Int) :Nothing =
		throw new NoSuchElementException(indexWhereErrorMessage(self, from))

	private[sugar] def lastIndexWhereNotFound(self :String, length :Int, end :Int) :Nothing =
		throw new NoSuchElementException(lastIndexWhereErrorMessage(self, length, end))

	private[sugar] def indexOfSliceNotFound(self :String, seq :collection.Seq[Any], from :Int) :Nothing =
		throw new NoSuchElementException(indexOfSliceErrorMessage(self, seq, from))

	private[sugar] def lastIndexOfSliceNotFound(self :String, length :Int, seq :collection.Seq[Any], end :Int) :Nothing =
		throw new NoSuchElementException(lastIndexOfSliceErrorMessage(self, length, seq, end))

	private[sugar] def indexOfErrorMessage(self :String, x :Any, from :Int) :String =
		"No " + x + " in " + self + (if (from == 0) "." else " at or after index " + from + ".")

	private[sugar] def lastIndexOfErrorMessage(self :String, length :Int, x :Any, end :Int) :String =
		"No " + x + " in " + self + (if (end == length - 1) "." else " at or before index " + end + ".")

	private[sugar] def indexWhereErrorMessage(self :String, from :Int) :String =
		"No element satisfying the predicate in " + self +
			(if (from == 0) "." else " at or after index " + from + ".")

	private[sugar] def lastIndexWhereErrorMessage(self :String, length :Int, end :Int) :String =
		"No element satisfying the predicate in " + self +
			(if (end == length - 1) "." else " at or before index " + end + ".")

	private[sugar] def indexOfSliceErrorMessage(self :String, that :collection.Seq[Any], from :Int) :String =
		"No " + that + " in " + self + (if (from == 0) "." else " at or after index " + from + ".")

	private[sugar] def lastIndexOfSliceErrorMessage(self :String, length :Int, that :collection.Seq[Any], end :Int)
			:String =
		"No " + that + " in " + self + (if (end == length - 1) "." else " at or before index " + end + ".")

}



/*
trait HasIndexOf[-E, -C] extends Serializable {
	def indexOf(coll :C, x :E, from :Int) :Int
}

@SerialVersionUID(Ver)
object HasIndexOf {
	implicit val seqHasIndexOf :HasIndexOf[Any, collection.Seq[Any]] = (seq, x, from) => seq.indexOf(x, from)
	implicit val arrayLikeHasIndexOf :HasIndexOf[Any, ArrayLike[Any]] = (array, x, from) => array.indexOf(x, from)
}


trait HasLastIndexOf[-E, -C] extends Serializable {
	def lastIndexOf(coll :C, x :E, end :Int) :Int
}

@SerialVersionUID(Ver)
object HasLastIndexOf {
	implicit val seqHasLastIndexOf :HasLastIndexOf[Any, collection.Seq[Any]] = (seq, x, end) => seq.lastIndexOf(x, end)
	implicit val arrayLikeHasLastIndexOf :HasLastIndexOf[Any, ArrayLike[Any]] =
		(array, x, end) => array.lastIndexOf(x, end)
}


trait HasIndexWhere[+E, -C] extends Serializable {
	def indexWhere(coll :C, p :E => Boolean, from :Int) :Int
}

@SerialVersionUID(Ver)
object HasIndexWhere {
	implicit def seqHasIndexWhere[E] :HasIndexWhere[E, collection.Seq[E]] =
		seqAnyHasIndexWhere.castParams[E, collection.Seq[E]]

	implicit def arrayLikeHasIndexWhere[E] :HasIndexWhere[E, ArrayLike[E]] =
		arrayLikeAnyHasIndexWhere.castParams[E, ArrayLike[E]]

	private[this] val seqAnyHasIndexWhere :HasIndexWhere[Any, collection.Seq[Any]] =
		(seq, p, from) => seq.indexWhere(p, from)
	private[this] val arrayLikeAnyHasIndexWhere :HasIndexWhere[Any, ArrayLike[Any]] =
		(array, p, from) => array.indexWhere(p, from)
}


trait HasLastIndexWhere[+E, -C] extends Serializable {
	def lastIndexWhere(coll :C, p :E => Boolean, end :Int) :Int
}

@SerialVersionUID(Ver)
object HasLastIndexWhere {
	implicit def seqHasLastIndexWhere[E] :HasLastIndexWhere[E, collection.Seq[E]] =
		seqHasLastIndexWhere.castParams[E, collection.Seq[E]]

	implicit def arrayLikeHasLastIndexWhere[E] :HasLastIndexWhere[E, ArrayLike[E]] =
		arrayLikeHasLastIndexWhere.castParams[E, ArrayLike[E]]

	private[this] val seqAnyHasLastIndexWhere :HasLastIndexWhere[Any, collection.Seq[Any]] =
		(seq, p, from) => seq.indexWhere(p, from)
	private[this] val arrayLikeHasLastIndexWhere :HasLastIndexWhere[Any, ArrayLike[Any]] =
		(array, p, from) => array.indexWhere(p, from)
}


trait HasIndexOfSlice[-E, -C, -S] extends Serializable {
	def indexOfSlice(coll :C, xs :S, from :Int) :Int
}

@SerialVersionUID(Ver)
object HasIndexOfSlice {
	implicit val seqHasIndexOfSlice :HasIndexOfSlice[Any, collection.Seq[Any], collection.Seq[Any]] =
		(seq, xs, from) => seq.indexOfSlice(xs, from)
	implicit val arrayLikeHasIndexOf :HasIndexOfSlice[Any, ArrayLike[Any], collection.Seq[Any]] =
		(array, xs, from) => array.indexOfSlice(xs, from)
}


trait HasLastIndexOfSlice[-E, -C, -S] extends Serializable {
	def lastIndexOfSlice(coll :C, xs :S, end :Int) :Int
}

@SerialVersionUID(Ver)
object HasLastIndexOfSlice {
	implicit val seqHasLastIndexOfSlice :HasLastIndexOfSlice[Any, collection.Seq[Any], collection.Seq[Any]] =
		(seq, xs, end) => seq.lastIndexOfSlice(xs, end)

	implicit val arrayLikeHasLastIndexOfSlice :HasLastIndexOfSlice[Any, ArrayLike[Any], collection.Seq[Any]] =
		(array, xs, end) => array.lastIndexOfSlice(xs, end)
}
*/

