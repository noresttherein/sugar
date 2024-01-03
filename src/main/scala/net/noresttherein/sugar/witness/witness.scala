package net.noresttherein.sugar

import scala.reflect.ClassTag


package object witness {
	private[witness] final val Ver = 1L

	type MaybeClassTag[T] = Optionally[ClassTag[T]]

	/** An ordering by `System.`[[java.lang.System.identityHashCode identityHashCode]],
	  * falling back to `getClass.hashCode`, and then to `getClass.getName.hashCode`.
	  * Useful to impose a unique (within a single JVM run) ordering on any collection of objects.
	  * This allows to acquire object monitors always in the same order, preventing deadlocks.
	  */
	object ReferentialOrdering extends Ordering[AnyRef] {
		//Extracted to reduce the size of compare for a higher likelihood of JVM optimization.
		private def hashConflict(x :AnyRef, y :AnyRef) = {
			var hash1 = x.getClass.hashCode
			var hash2 = y.getClass.hashCode
			java.lang.Integer.compare(hash1, hash2) match {
				case 0    =>
					hash1 = x.getClass.getName.hashCode
					hash2 = y.getClass.getName.hashCode
					java.lang.Integer.compare(hash1, hash2)
				case diff => diff
			}
		}
		override def compare(x :AnyRef, y :AnyRef) :Int = {
			if (x eq y)
				0
			else
				java.lang.Integer.compare(System.identityHashCode(x), System.identityHashCode(y)) match {
					case 0    => hashConflict(x, y)
					case diff => diff
				}
		}
	}
}
