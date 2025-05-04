package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.extensions.ifNullMethods


trait extensions extends Any {
	/** Extension methods for any type for graceful handling of `null` values. */
	@inline final def ifNullMethods[A](self :A) :ifNullMethods[A] = new ifNullMethods(self)
}



object extensions extends extensions {

	/** Extension methods for any type for graceful handling of `null` values. */
	class ifNullMethods[A](private val self :A) extends AnyVal {
		/** Applies the given function to `this` value if the latter is not `null`,
		  * or returns `null` in the opposite case.
		  * Same as [[net.noresttherein.sugar.vars.extensions.ifNullMethods.ifNotNull ifNotNull]].
		  */
		@inline def ?[B >: Null](ifNotNull :A => B) :B = if (self == null) null else ifNotNull(self)

		/** Applies the given function to `this` value if the latter is not `null`,
		  * or returns `null` in the opposite case.
		  */
		@inline def ifNotNull[B >: Null](ifNotNull :A => B) :B = if (self == null) null else ifNotNull(self)

		/** Returns `this` unless it is `null`, in which case returns the argument. */
		@inline def ifNull[U >: A](ifNull: => U) :U = if (self == null) ifNull else self
	}

}
