package net.noresttherein.sugar.util

import net.noresttherein.sugar.util.CachesHashCode.CacheNotComputed




/** A mixin trait for immutable classes which stores the object's `hashCode` in a field, once computed,
  * for quick repeated calls. Additionally, it first compares the hash codes of the two compared objects
  * in `equals`, betting on the fact that at least one of them was or will be compared again with other instances.
  *
  * The class to which it is mixed in should ''not'' override `equals` and `hashCode` (but may `canEqual`);
  * rather these methods ''must'' be implemented in a trait or class preceding `CachesHashCode`
  * in the linearization order for the class, as they relegate to `super`. They may, however, override `canEqual`.
  * @author Marcin Mościcki
  */
trait CachesHashCode {
	/** Returns `false` if the hash codes of objects differ; otherwise, returns `super.equals(that)`. */
	override def equals(that :Any) :Boolean = that match {
		//We don't check if that is CachesHashCode or canEqual, because
		// 1) super should do it, and 2) the subclass may wish to equal instances of other classes (like collections do).
		case self :AnyRef if this eq self   => true
		case _ if that.hashCode != hashCode => false
		case _                              => super.equals(that)
	}

	//Yes, using -1 as a marker effectively disables the cache if the real hash code is, in fact, -1,
	// but it's a small risk, and we don't need a separate field.
	@volatile private var hash = CacheNotComputed
	override def hashCode :Int = {
		var ## = hash
		if (## == CacheNotComputed) {
			##   = super.hashCode
			hash = ##
		}
		##
	}
}


private object CachesHashCode {
	final val CacheNotComputed = -1
}
