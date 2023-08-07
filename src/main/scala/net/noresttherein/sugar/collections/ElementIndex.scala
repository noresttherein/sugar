package net.noresttherein.sugar.collections

import scala.collection.Searching
import scala.collection.Searching.{InsertionPoint, SearchResult}

import net.noresttherein.sugar.vars.IntOpt.{AnInt, NoInt}
import net.noresttherein.sugar.vars.{IntOpt, Opt}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A lighter alternative to [[scala.collection.Searching.SearchResult SearchResult]],
  * which, additionally, allows the possibility of not specifying an insertion point for 'not found' results.
  * This makes it possible to use for unordered, linear search results.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
class ElementIndex private[collections] (private val idx :Int) extends AnyVal with Serializable {
	def insertionPoint :IntOpt =
		if (idx < 0)
			if (idx == Int.MinValue) NoInt else AnInt(-idx - 1)
		else AnInt(idx)

	@inline def isFound  :Boolean = idx >= 0
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
	@inline def unapply(result :ElementIndex) :IntOpt = result.insertionPoint

	@SerialVersionUID(Ver)
	object Present {
		@inline def apply(idx :Int) :ElementIndex =
			if (idx < 0) throw new IllegalArgumentException("Found at a negative index: " + idx)
			else new ElementIndex(idx)

		@inline def unapply(result :ElementIndex) :IntOpt = result.index
	}

	@SerialVersionUID(Ver)
	object Absent {
		def apply(insertionPoint :Int) :ElementIndex =
			if (insertionPoint < 0)
				throw new IllegalArgumentException("Negative insertion point: " + insertionPoint)
			else if (insertionPoint == Int.MaxValue)
				throw new IllegalArgumentException("Cannot possibly insert at index Int.MaxValue")
			else
				new ElementIndex(-insertionPoint - 1)

		@inline def apply() :ElementIndex = new ElementIndex(Int.MinValue)

		@inline def unapply(result :ElementIndex) :Boolean = !result.isFound
	}
}
