package net.noresttherein.slang

import prettyprint.ClassNameOf

package object matching {
	
	
	/** Combines two extractor patterns into a logical conjunction which matches only if both of the patterns
	  * match (and binds values to both patterns). Usage: `{ case P1(x) && P2(y) => ??? }`.
	  */
	object && {
		def unapply[T](x :T) = Some(x, x)
	}

	/** A multi-purpose extractor for values of `Out` from parameter `In`.
	  * Can be used in pattern matching or as a partial function.
	  * @tparam In argument type
	  * @tparam Out type of extracted value
	  */
	abstract class Extractor[-In, +Out] extends PartialFunction[In, Out] {
		def unapply(in :In) :Option[Out]

		override def isDefinedAt(x: In): Boolean = unapply(x).isDefined

		@inline final override def apply(x: In): Out = unapply(x) getOrElse {
			throw new NoSuchElementException(s"$this($x)")
		}

		override def toString :String = this.shortClassName
	}

	/** Companion object for extractors. */
	object Extractor {

		/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
		  * that can be used in pattern matching or as a partial function.
		  * @param f function extracting `Out` values from `In` arguments.
		  * @tparam In argument type
		  * @tparam Out extracted result type
		  * @return a partial function extractor wrapping the given function `f`.
		  */
		def apply[In, Out](f :In=>Option[Out]) :Extractor[In, Out] = new ExtractorOption(f, s"Extractor(${f.localClassName})")


		private class ExtractorOption[-In, +Out](f :In=>Option[Out], override val toString :String) extends Extractor[In, Out] {
			override def unapply(in: In): Option[Out] = f(in)
		}
	}

	abstract class Unapply[-In, +Out] {
		def unapply(arg :In) :Option[Out]

		@inline final def get(arg :In) :Out = unapply(arg) getOrElse {
			throw new NoSuchElementException(s"$this.get($arg)")
		}

		@inline final def apply(arg :In) :Option[Out] = unapply(arg)

		override def toString: String = this.shortClassName
	}

	object Unapply {
		def apply[In, Out](f :In => Option[Out]) :Unapply[In, Out] = new Unapply[In, Out] {
			override def unapply(in :In) = f(in)

			override def toString = s"Unapply(${f.localClassName})"
		}
	}
	
}