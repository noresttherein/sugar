package net.noresttherein.sugar.reflect

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JLong, JShort}
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




trait extensions extends Any {
	implicit def classExtension(self :Class[_]) = new ClassExtension(self)
}




@SerialVersionUID(ver)
object extensions extends extensions {
	class ClassExtension private[reflect] (private val self :Class[_]) extends AnyVal {
		/** True for Java classes which serve as wrappers for Java primitive types (`Integer`, `Character`, etc.). */
		def isBox :Boolean = extensions.Unwrapped.contains(self)

		/** True if the argument is a class for a built in value type represented by a Java primitive,
		  * and this class is the Java class used to box it when lifting the argument to a reference type. */
		def isBoxOf(valueClass :Class[_]) :Boolean = extensions.Unwrapped.getOrElse(self, null) == valueClass

		/** If this class represents a built in value type (a Java primitive type), return the Java class to which
		  * it is auto boxed when a reference type is needed. */
		@throws[UnsupportedOperationException]("if this class is not a built in value type.")
		def boxed   :Class[_] = extensions.Wrappers(self)

		/** If this is a Java class to which a Java primitive type is auto boxed, return the class for the primitive type. */
		@throws[UnsupportedOperationException]("if this class is not a box for a value type.")
		def unboxed :Class[_] = extensions.Unwrapped(self)

		/** Returns the appropriate box class for built in value types, or `this` if it is a reference type
		  * (or a custom value class).
		  */
		def refClass :Class[_] = if (!self.isPrimitive) self else extensions.Wrappers(self)

		/** Unboxes this class to its primitive type.
		  * Similar to [[net.noresttherein.sugar.reflect.extensions.ClassExtension.unboxed unboxed]],
		  * but primitive types simply return themselves.
		  */
		@throws[UnsupportedOperationException]("if this class is neither a primitive type nor a box of a primitive type.")
		def valueClass :Class[_] = if (self.isPrimitive) self else extensions.Unwrapped(self)

		/** Same as [[Class.isAssignableFrom isAssignableFrom]], but perhaps less confusing about the direction
		  * of subtyping.
		  */
		@inline def isSuperclassOf(other :Class[_]) :Boolean = self.isAssignableFrom(other)
	}


	/** Matching pattern unboxing Java primitive box classes and a boxing class conversion. */
	final class Boxer private[reflect] {
		/** If `clss` is a Java primitive type, return its specific box class. Otherwise return the argument itself. */
		def apply(clss :Class[_]) :Class[_] =
			if (clss.isPrimitive) clss.boxed else clss

		/** If the argument is a box class for a Java primitive type, return the appropriate primitive type.
		  * Otherwise return the argument itself.
		  */
		def unbox(clss :Class[_]) :Class[_] =
			Unwrapped.getOrElse(clss, clss)

		def unapply(clss :Class[_]) :Opt[Class[_]] =
			Opt(Unwrapped.getOrElse(clss, null))
	}

	private[reflect] val Wrappers :Map[Class[_], Class[_]] = Map[Class[_], Class[_]](
		classOf[Char]    -> classOf[JChar],
		classOf[Byte]    -> classOf[JByte],
		classOf[Short]   -> classOf[JShort],
		classOf[Int]     -> classOf[JInt],
		classOf[Long]    -> classOf[JLong],
		classOf[Float]   -> classOf[JFloat],
		classOf[Double]  -> classOf[JDouble],
		classOf[Boolean] -> classOf[JBoolean]
	).withDefault(c => throw new UnsupportedOperationException("Class " + c.getName + " is not a built in value class."))

	private[reflect] val Unwrapped :Map[Class[_], Class[_]] =
		Wrappers.map { case (k, v) => (v, k) }.withDefault(
			c => throw new UnsupportedOperationException(
				"Class " + c.getName + " is not a wrapper for a Java primitive type."
			)
		)
}
