package net.noresttherein.sugar.reflect

import java.lang.reflect.Type

import net.noresttherein.sugar.reflect
import net.noresttherein.sugar.reflect.extensions.{ClassExtension, ReflectAnyExtension}
import net.noresttherein.sugar.reflect.prettyprint.{abbrevNameOf, fullNameOf, innerNameOf, localNameOf}




trait extensions extends Any with prettyprint.extensions {
	/** Adds `innerName`, `localName` and `abbrevName` methods to `Class`, providing a shorter alternative to `getName`. */
	@inline implicit final def ClassExtension(self :Class[_]) :ClassExtension = new ClassExtension(self)

	/** Adds methods for reflected querying of an object for its type parameters. */
	@inline implicit final def ReflectAnyExtension(self :Any) :ReflectAnyExtension = new ReflectAnyExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {
	class ReflectAnyExtension(private val self :Any) extends AnyVal {
		/** Returns the Java type given as the `n`-th type parameter to a superclass of this object.
		  * If the type is known, that is there is a chain of classes `C1, ..., CN, C`, such that;
		  *   1. `C1 == supertype`,
		  *   1. `this :C`,
		  *   1. each class directly extends (or implements, for traits) the previous one,
		  *   1. `I1,.., IN` is a sequence of integers such that `I1 = n`, and `I`_k+1_
		  *      is the index of the type parameter of class `C`_k+1_
		  *      given as the type argument for the `I`_k_-th type parameter of class `C`_k_,
		  *   1. `C` gives a concrete class `T` as the `IN`-th type parameter to `CN`,
		  * then `T` is the returned. Otherwise, the exact `n`-th type argument does not resolve to a concrete class,
		  * and a [[java.lang.reflect.TypeVariable TypeVariable]] of this object's class,
		  * to which the `n`-th type argument of `supertype` resolves.
		  */
		@inline def typeArgumentOf(n :Int, supertype :Class[_]) :Type =
			reflect.typeArgumentOf(self.getClass, n, supertype)
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
		  * and this class is the Java class used to box it when lifting the argument to a reference type.
		  */
		def isBoxOf(valueClass :Class[_]) :Boolean = PrimitiveClass.getOrElse(self, null) == valueClass

		/** If this class represents a built in value type (a Java primitive type), return the Java class to which
		  * it is auto boxed when a reference type is needed.
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.refClass refClass]]
		  */ //consider: there is an inconsistency in naming with Boxed and Unboxed, which never throw.
		@throws[UnsupportedOperationException]("if this class is not a built in value type.")
		def boxed   :Class[_] = BoxClass.getOrElse(self, null) match {
			case null => throw new UnsupportedOperationException(
				"Class " + self.getName + " is not a built in value type."
			)
			case box => box
		}

		/** If this is a Java class to which a Java primitive type is auto boxed, return the class for the primitive type.
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.unwrapped unwrapped]]
		  */
		@throws[UnsupportedOperationException]("if this class is not a box for a value type.")
		def unboxed :Class[_] = PrimitiveClass.getOrElse(self, null) match {
			case null => throw new UnsupportedOperationException(
				"Class " + self.getName + " is not a wrapper for a built-in value type."
			)
			case primitive => primitive
		}

		/** If this is a Java class to which a Java primitive type is auto boxed,
		  * return the class for the primitive type; otherwise, return this class.
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.unboxed unboxed]]
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.valueClass valueClass]]
		  */
		def unwrapped :Class[_] = PrimitiveClass.getOrElse(self, null) match {
			case null => self
			case primitive => primitive
		}

		/** True if the Java/Scala runtime allows this type where `other` class is expected.
		  * @return `(other isAssignableFrom this) || (this isBoxOf other) || (other isBoxOf this)`.
		  */ //consider: renaming to isAcceptableFor
		def isConvertibleTo(other :Class[_]) :Boolean =
			other.isAssignableFrom(self) || isBoxOf(other) || other.isBoxOf(self)

		/** Returns the appropriate box class for built in value types, or `this` if it is a reference type
		  * (or a custom value class).
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.boxed boxed]]
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

		/** The actual type argument given by this class, possibly indirectly, to the specified superclass
		  * or extended trait.
		  * @param n the index, counting from
		  */
		def typeArgument(n :Int, givenTo :Class[_]) :Type = reflect.typeArgumentOf(self, n, givenTo)
	}

}
