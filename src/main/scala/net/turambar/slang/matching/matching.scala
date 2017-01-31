package net.turambar.slang



package object matching {
	
	
	/** Combines two extractor patterns into a logical conjunction which matches only if both of the patterns
	  * match (and binds values to both patterns). Usage: `{ case P1(x) && P2(y) => ??? }`.
	  */
	object && {
		def unapply[T](x :T) = Some(x, x)
	}
	
	
	trait Unapply[-In, +Out] extends PartialFunction[In, Out] {
		def unapply(in :In) :Option[Out]
		
		override def isDefinedAt(x: In): Boolean = unapply(x).isDefined
		
		override def apply(x: In): Out = unapply(x) getOrElse {
			throw new IllegalArgumentException(s"Can't unapply $x with $this")
		}
	}
	
	object Unapply {
		def apply[In, Out](f :In=>Option[Out]) :Unapply[In, Out] = new UnapplyOption(f, s"Unapply($f)")
		
		
		private class UnapplyOption[-In, +Out](f :In=>Option[Out], override val toString :String) extends Unapply[In, Out] {
			override def unapply(in: In): Option[Out] = f(in)
		}
	}
	
	
}