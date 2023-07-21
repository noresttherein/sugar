package net.noresttherein.sugar.reflect

import net.noresttherein.sugar.reflect.extensions.{AnyRefExtension, ClassExtension}
import net.noresttherein.sugar.reflect.prettyprint.{abbrevNameOf, fullNameOf, innerNameOf, localNameOf}




trait extensions extends Any with prettyprint.extensions {
	/** Adds `innerName`, `localName` and `abbrevName` methods to `Class`, providing a shorter alternative to `getName`. */
	@inline implicit final def ClassExtension(self :Class[_]) :ClassExtension = new ClassExtension(self)

	/** Adds `identityHashCode` method to all reference types. */
	@inline implicit final def AnyRefExtension(self :AnyRef) :AnyRefExtension = new AnyRefExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {
	class AnyRefExtension(private val self :AnyRef) extends AnyVal {
		/** The value of `hashCode` as it would be inherited for this object from the default implementation in `AnyRef`. */
		@inline def identityHashCode :Int = System.identityHashCode(self)
	}



	/** Extension methods dealing with boxing and unboxing, as well as formatting the name of a class in several ways,
	  * demangling the runtime class name to an approximation of the class symbol as it appears in code.
	  */
	class ClassExtension private[reflect] (private val self :Class[_]) extends AnyVal {
		/** Returns `other isAssignableFrom this`. */
		@inline def <:<(other :Class[_]) :Boolean = other isAssignableFrom self

		/** True if a value of a variable of this type can be assigned in source code to a variable of type `other`,
		  * possibly involving boxing or unboxing.
		  * @return `true` ''iff'' one of the conditions hold:
		  *            1. `this` is a subclass of `other`, or implements `other` if `other` is a trait;
		  *            1. `this` is a built in value type, and `other` is a superclass of its reference box type;
		  *            1. `other` is a built in value type, and `this` is its reference box type.
		  */
		@inline def <%<(other :Class[_]) :Boolean =
			(other isAssignableFrom self) || other.isAssignableFrom(refClass) || isBoxOf(other)

		/** Tue if either the argument is the same class as this one, or one class is a built in value type,
		  * and the other is its standard box class.
		  */
		@inline def =%=(other :Class[_]) :Boolean = (self == other) || isBoxOf(other) || other.isBoxOf(self)

		/** True for Java classes which serve as wrappers for Java primitive types (`Integer`, `Character`, etc.). */
		def isBox :Boolean = PrimitiveClass.contains(self)

		/** True if the argument is a class for a built in value type represented by a Java primitive,
		  * and this class is the Java class used to box it when lifting the argument to a reference type. */
		def isBoxOf(valueClass :Class[_]) :Boolean = PrimitiveClass.getOrElse(self, null) == valueClass

		/** If this class represents a built in value type (a Java primitive type), return the Java class to which
		  * it is auto boxed when a reference type is needed. */
		@throws[UnsupportedOperationException]("if this class is not a built in value type.")
		def boxed   :Class[_] = BoxClass.getOrElse(self, null) match {
			case null => throw new UnsupportedOperationException(
				"Class " + self.getName + " is not a built in value type."
			)
			case box => box
		}

		/** If this is a Java class to which a Java primitive type is auto boxed, return the class for the primitive type. */
		@throws[UnsupportedOperationException]("if this class is not a box for a value type.")
		def unboxed :Class[_] = PrimitiveClass.getOrElse(self, null) match {
			case null => throw new UnsupportedOperationException(
				"Class " + self.getName + " is not a wrapper for a Java primitive type."
			)
			case primitive => primitive
		}

		/** True if the Java/Scala runtime allows this type where `other` class is expected.
		  * @return `(other isAssignableFrom this) || (this isBoxOf other) || (other isBoxOf this)`.
		  */ //consider: renaming to isAcceptableFor
		def isConvertibleTo(other :Class[_]) :Boolean =
			other.isAssignableFrom(self) || isBoxOf(other) || other.isBoxOf(self)

		/** Returns the appropriate box class for built in value types, or `this` if it is a reference type
		  * (or a custom value class).
		  */
		def refClass :Class[_] = if (!self.isPrimitive) self else BoxClass(self)

		/** Unboxes this class to its primitive type.
		  * Similar to [[net.noresttherein.sugar.reflect.extensions.ClassExtension.unboxed unboxed]],
		  * but primitive types simply return themselves.
		  */
		@throws[UnsupportedOperationException]("if this class is neither a primitive type nor a box of a primitive type.")
		def valueClass :Class[_] = if (self.isPrimitive) self else PrimitiveClass(self)

		/** Same as [[Class.isAssignableFrom isAssignableFrom]], but perhaps less confusing about the direction
		  * of subtyping.
		  */
		@inline def isSuperclassOf(other :Class[_]) :Boolean = self.isAssignableFrom(other)

		/** An approximation of the imported type symbol of this class, as it would be referenced
		  * in code. First, the whole package prefix and all trailing '$' characters are dropped.
		  * Then, all escape sequences for special characters which are legal for use in identifiers are unescaped
		  * to their original forms. Then, dropped is the prefix up until and including to the last '$'. If the class
		  * is specialized, its mangled type parameters are resolved and composed in a type parameter list
		  * in Scala's syntax. If the class is anonymous, a simple '.anon' replaces its whole anonymous name section,
		  * and prepended to it is the directly preceding/enclosing class name, that is the inner-most class name
		  * from the non-anonymous prefix. Primitive types are capitalized to their Scala names and arrays are formatted
		  * recursively as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used
		  * for informational, debugging purposes, and not for identifiers or in any sort of reflection operations,
		  * as it can fail to produce the correct and unique type representation for a number of reasons. Most notably,
		  * any kind of generic, non-specialized classes will not have any type arguments listed,
		  * and only `@specialized` type parameters of specialized classes will be shown. Use of '$' in a demangled name
		  * will throw it off, as will identifiers quoted in backticks. Finally, for the obvious reason, the name
		  * of the anonymous class is synthetic and the same for all anonymous inner classes of the same enclosing
		  * class/object.
		  */
		@inline def innerName: String = innerNameOf(self)

		/** An approximation of the type name of this class, as it would appear in code.
		  * It doesn't include the package prefix, but includes demangled names of all enclosing classes/objects.
		  * The demangling proceeds as follows: first, all trailing '$' characters are dropped.
		  * Then, all escape sequences for special characters which are legal for use in identifiers are unescaped
		  * to their original forms. All individual '$' signs (used in name mangling of inner classes as the separators)
		  * are replaced with a '.', and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class
		  * is specialized, its mangled type parameters are resolved and composed in a type parameter list
		  * in Scala's syntax. Primitive types are capitalized to their Scala names and arrays are formatted recursively
		  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used
		  * for informational, debugging purposes, and not for identifiers or in any sort of reflection operations,
		  * as it can fail to produce the correct and unique type representation for a number of reasons. Most notably,
		  * any kind of generic, non-specialized classes will not have any type arguments listed, and only `@specialized`
		  * type parameters of specialized classes will be shown. Use of '$' in a demangled name will throw it off,
		  * as will identifiers quoted in backticks. Finally, for the obvious reason, the name of the anonymous class
		  * is synthetic.
		  */
		@inline def localName :String = localNameOf(self)

		/** An abbreviated qualified name of this class, with abbreviated package prefix and demangled
		  * to an approximation of how it would appear in code. All package names are replaced with their first letters,
		  * while the class name is demangled as follows: first, all trailing '$' are dropped and escape sequences
		  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
		  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
		  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
		  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
		  * to their Scala names and arrays are formatted recursively as 'Array['`classNameOf(element)`']'.
		  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
		  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique
		  * type representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
		  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
		  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
		  * Finally, anonymous classes receive synthetic names for the obvious reason.
		  */
		@inline def abbrevName :String = abbrevNameOf(self)

		/** An approximation of the full, qualified and demangled name of this, as it would
		  * appear in code.
		  * @return `this.`[[net.noresttherein.sugar.reflect.extensions.ClassExtension.demangledName demangledName]].
		  */
		@inline def name :String = fullNameOf(self)

		/** An approximation of the full, qualified and demangled name of this, as it would
		  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
		  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
		  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
		  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
		  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
		  * to their Scala names and arrays are formatted recursively as 'Array['`classNameOf(element)`']'.
		  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
		  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique
		  * type representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
		  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
		  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
		  * Finally, anonymous classes receive synthetic names for the obvious reason.
		  */
		@inline def demangledName :String = fullNameOf(self)
	}

}
