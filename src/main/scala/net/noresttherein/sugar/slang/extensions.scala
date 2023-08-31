package net.noresttherein.sugar.slang

import net.noresttherein.sugar.slang.extensions.AnyRefExtension





trait extensions extends Any {
	/** Adds `identityHashCode` method to all reference types. */
	@inline implicit final def AnyRefExtension(self :AnyRef) :AnyRefExtension = new AnyRefExtension(self)
}



@SerialVersionUID(Ver)
object extensions extends extensions {
	class AnyRefExtension(private val self :AnyRef) extends AnyVal {
		/** The value of `hashCode` as it would be inherited for this object from the default implementation in `AnyRef`. */
		@inline def identityHashCode :Int = System.identityHashCode(self)
	}
}
