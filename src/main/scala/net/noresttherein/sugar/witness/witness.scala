package net.noresttherein.sugar

import scala.reflect.ClassTag




package object witness {
	private[witness] final val Ver = 1L

	type MaybeClassTag[T] = Optionally[ClassTag[T]]


	implicit val IgnoredArgumentInstance  :Ignored  = new Ignored
	implicit val Ignored1ArgumentInstance :Ignored1 = new Ignored1
	implicit val Ignored2ArgumentInstance :Ignored2 = new Ignored2
	implicit val Ignored3ArgumentInstance :Ignored3 = new Ignored3
	implicit val Ignored4ArgumentInstance :Ignored4 = new Ignored4
	implicit val Ignored5ArgumentInstance :Ignored5 = new Ignored5
	implicit val Ignored6ArgumentInstance :Ignored6 = new Ignored6
	implicit val Ignored7ArgumentInstance :Ignored7 = new Ignored7
	implicit val Ignored8ArgumentInstance :Ignored8 = new Ignored8
	implicit val Ignored9ArgumentInstance :Ignored9 = new Ignored9

	implicit val `ignore Overload`  :Overload[Nothing]  = new Overload
	implicit val `ignore Overload1` :Overload1[Nothing] = new Overload1
	implicit val `ignore Overload2` :Overload2[Nothing] = new Overload2
	implicit val `ignore Overload3` :Overload3[Nothing] = new Overload3
	implicit val `ignore Overload4` :Overload4[Nothing] = new Overload4
	implicit val `ignore Overload5` :Overload5[Nothing] = new Overload5
	implicit val `ignore Overload6` :Overload6[Nothing] = new Overload6
	implicit val `ignore Overload7` :Overload7[Nothing] = new Overload7
	implicit val `ignore Overload8` :Overload8[Nothing] = new Overload8
	implicit val `ignore Overload9` :Overload9[Nothing] = new Overload9
}




package witness {

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
