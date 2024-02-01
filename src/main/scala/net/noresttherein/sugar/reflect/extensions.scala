package net.noresttherein.sugar.reflect

import java.lang.reflect.Type

import scala.annotation.tailrec

import net.noresttherein.sugar
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.reflect.prettyprint.{abbrevNameOf, fullNameOf, innerNameOf, localNameOf}
import net.noresttherein.sugar.exceptions.{illegal_!, unsupported_!}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




trait extensions extends Any with prettyprint.extensions {
	/** Adds `innerName`, `localName` and `abbrevName` methods to `Class`, providing a shorter alternative to `getName`. */
	@inline implicit final def ClassExtension(self :Class[_]) :ClassExtension = new ClassExtension(self)
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	/** Extension methods dealing with boxing and unboxing, as well as formatting the name of a class in several ways,
	  * demangling the runtime class name to an approximation of the class symbol as it appears in code.
	  */
	class ClassExtension private[reflect] (private val self :Class[_]) extends AnyVal {

		/** Same as [[java.lang.Class Class]]`.`[[java.lang.Class.isInstance isInstance]], but returns `true`
		  * also if this class is of a built in (primitive) value type to which the argument can be automatically
		  * unboxed.
		  */
		@inline def isBoxInstance(value :Any) :Boolean =
			self.isInstance(value) || self.isPrimitive && BoxClass(self).isInstance(value)

		/** Returns `other isAssignableFrom this`. */
		@inline def <:<(other :Class[_]) :Boolean = other isAssignableFrom self

		/** Returns `other isAssignableFrom this`. */
		@inline def isSubclassOf(other :Class[_]) :Boolean = other isAssignableFrom self

		/** Same as `this `[[java.lang.Class.isAssignableFrom isAssignableFrom]]` other`. */
		@inline def >:>(other :Class[_]) :Boolean = self isAssignableFrom other

		/** Same as [[Class.isAssignableFrom isAssignableFrom]],
		  * but perhaps less confusing about the direction of subtyping.
		  */
		@inline def isSuperclassOf(other :Class[_]) :Boolean = self.isAssignableFrom(other)

		/** True if a value of a variable of this type can be assigned in source code to a variable of type `other`,
		  * possibly involving boxing or unboxing.
		  * @return `true` ''iff'' one of the conditions hold:
		  *            1. `this` is a subclass of `other`, or implements `other` if `other` is a trait;
		  *            1. `this` is a built in value type, and `other` is a superclass of its reference box type;
		  *            1. `other` is a built in value type, and `this` is its reference box type.
		  */
		@inline def <%<(other :Class[_]) :Boolean =
			(other isAssignableFrom self) || other.isAssignableFrom(Boxed(self)) || isBoxOf(other)

		/** True if the Java/Scala runtime allows this type where `other` class is expected.
		  * @return `(other isAssignableFrom this) || (other isAssignableFrom Boxed(this)) || (this isBoxOf other)`.
		  */
		@inline def isConvertibleTo(other :Class[_]) :Boolean =
			other.isAssignableFrom(self) || other.isAssignableFrom(Boxed(self)) || isBoxOf(other)

		/** True if a value of a variable of this type can be assigned in source code to a variable of type `other`,
		  * possibly involving boxing or unboxing.
		  * @return `other `[[net.noresttherein.sugar.reflect.extensions.ClassExtension.<%< <%<]]` this`.
		  */
		@inline def >%>(other :Class[_]) :Boolean =
			(self isAssignableFrom other) || self.isAssignableFrom(Boxed(other)) || other.isBoxOf(self)

		/** True if a value of a variable of this type can be assigned in source code to a variable of type `other`,
		  * possibly involving boxing or unboxing.
		  * @return `other `[[net.noresttherein.sugar.reflect.extensions.ClassExtension.<%< <%<]]` this`.
		  */
		@inline def accepts(other :Class[_]) :Boolean =
			(self isAssignableFrom other) || self.isAssignableFrom(Boxed(other)) || other.isBoxOf(self)

		/** Tue if either the argument is the same class as this one, or one class is a built in value type,
		  * and the other is its standard box class.
		  */
		@inline def =%=(other :Class[_]) :Boolean = Boxed(self) == Boxed(other)

		/** Tue if either the argument is the same class as this one, or one class is a built in value type,
		  * and the other is its standard box class.
		  */
		@inline def boxedEquals(other :Class[_]) :Boolean = Boxed(self) == Boxed(other)

		/** Finds a common superclass with another class. For the purpose of this method, `classOf[Any]`
		  * is understood to be a superclass of all other classes, despite not satisfying
		  * [[java.lang.Class.isAssignableFrom isAssignableFrom]] for value (primmitive) classes.
		  * This method however does not abstract over boxing: a superclass of a primitive class and its box class
		  * is always `classOf[Any]`.
		  *   1. If the classes are equal, than this class is returned;
		  *   1. Otherwise, if either of the classes is a primitive type, `classOf[Any]` is returned.
		  *   1. Otherwise, if there is a superclass `S` of the two classes, such that all other common super classes
		  *      are a superclass of `S`, then `classOf[S]` is returned.
		  *   1. Otherwise, `classOf[Any]` is returned.
		  */
		@inline def ||(other :Class[_]) :Class[_] = commonSuperclass(other) match {
			case Yes(superClass) => superClass
			case _ => classOf[Any]
		}

		/** Same as [[net.noresttherein.sugar.reflect.extensions.ClassExtension.|| ||]]. */
		@inline def or(other :Class[_]) :Class[_] = this || other

		/** Finds a superclass, or extended trait, `S` of this class and the argument, such that,
		  * for every other class `C: C.isAssignableFrom(this) && C.isAssignableFrom(other)`, `C.isAssignableFrom(S)`.
		  * In other words, this method traverses the inheritance graph of both classes,
		  * and finds their least upper bound with regard to inheritance partial order.
		  *
		  * The search may return `No` in the following situations:
		  *   1. `this` and `other` are different value types;
		  *   1. one of the classes is a value type, and the other a reference type;
		  *   1. both `this` and `other` extend, directly or indirectly, the same two unrelated classes/traits.
		  */
		def commonSuperclass(other :Class[_]) :Maybe[Class[_]] =
			if (self eq other) Yes(self)
			else if (self isAssignableFrom other) Yes(self)
			else if (other isAssignableFrom self) Yes(other)
			else if (self.isPrimitive || other.isPrimitive) No
			else {
				def findSuperclass(superclass :Class[_], candidate :Class[_]) :Class[_] =
					if (superclass isAssignableFrom other)
						if (superclass isAssignableFrom candidate) candidate
						else if (candidate isAssignableFrom superclass) superclass
						else null
					else {
						val sup = superclass.getSuperclass
						var best =
							if (sup == null) candidate
							else findSuperclass(sup, candidate)
						val traits = superclass.getInterfaces
						var i = traits.length
						while (best != null & i > 0) {
							i -= 1
							best = findSuperclass(traits(i), best)
						}
						best
					}
				Maybe(findSuperclass(self, classOf[Any]))
			}

		/** True for Java classes which serve as wrappers for Java primitive types (`Integer`, `Character`, etc.),
		  * as well as `BoxedUnit`. */
		def isBox :Boolean = PrimitiveClass.contains(self)

		/** True if the argument is a class for a built in value type represented by a Java primitive,
		  * and this class is the Java class used to box it when lifting the argument to a reference type.
		  * `classOf[BoxedUnit] isBoxOf classOf[Unit]` returns `true`.
		  */
		def isBoxOf(valueClass :Class[_]) :Boolean = PrimitiveClass.getOrElse(self, null) == valueClass

		/** If this class represents a built in value type (a Java primitive type), return the Java class to which
		  * it is auto boxed when a reference type is needed.
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.refClass refClass]]
		  */ //consider: there is an inconsistency in naming with Boxed and Unboxed, which never throw.
		@throws[UnsupportedOperationException]("if this class is not a built in value type.")
		def boxed   :Class[_] = BoxClass.getOrElse(self, null) match {
			case null => unsupported_!(
				"Class " + self.getName + " is not a built in value type."
			)
			case box => box
		}

		/** If this is a Java class to which a Java primitive type is auto boxed, return the class for the primitive type.
		  * @see [[net.noresttherein.sugar.reflect.extensions.ClassExtension.unwrapped unwrapped]]
		  */
		@throws[UnsupportedOperationException]("if this class is not a box for a value type.")
		def unboxed :Class[_] = PrimitiveClass.getOrElse(self, null) match {
			case null => unsupported_!(
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

		/** True if this class is either a Java primitive type, or the runtime representation of `Unit`
		  * (which is a reference class). Returns `false` for custom value classes.
		  */
		def isAnyVal :Boolean = self.isPrimitive || self == classOf[Unit]

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

		/** Class of an array with component type equal to this class. */
		@inline def arrayClass :Class[_] = ArrayClass(self)

		/** A `dim`-dimensional array of this element type:
		  * array with `componentType` equal to `dim-1`-dimensional array of this element type.
		  */
		@throws[IllegalArgumentException]("if dim <= 0")
		def arrayClass(dim :Int) :Class[_] =
			if (dim <= 0)
				illegal_!("Non positive array dimension: " + dim)
			else {
				def arrayOf(n :Int) :Class[_] =
					if (n == 0) self else ArrayClass(arrayOf(n - 1))
				arrayOf(dim)
			}


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
		  * Finally, anonymous classes receive synthetic names for obvious reasons.
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
		  * Finally, anonymous classes receive synthetic names for obvious reasons.
		  */
		@inline def demangledName :String = fullNameOf(self)

		/** The actual type argument given by this class, possibly indirectly, to the specified superclass
		  * or extended trait. If the type is known, that is there is a chain of classes `C1, ..., CN, C`, such that;
		  *   1. `C1 == supertype`,
		  *   1. `this =:= C`,
		  *   1. each class directly extends (or implements, for traits) the previous one,
		  *   1. `I1,.., IN` is a sequence of integers such that `I1 = n`, and `I`_k+1_
		  *      is the index of the type parameter of class `C`_k+1_
		  *      given as the type argument for the `I`_k_-th type parameter of class `C`_k_,
		  *   1. `C` gives a concrete class `T` as the `IN`-th type parameter to `CN`,
		  *
		  * then `T` is the returned. Otherwise, the exact `n`-th type argument does not resolve to a concrete class,
		  * and a [[java.lang.reflect.TypeVariable TypeVariable]] of this object's class,
		  * to which the `n`-th type argument of `supertype` resolves.
		  * @param n       the index, counting from zero, of one of type parameters of `givenTo`.
		  * @param givenTo a superclass (or trait) of this class.
		  */
		def typeArgument(n :Int, givenTo :Class[_]) :Type = sugar.reflect.typeArgumentOf(self, n, givenTo)
	}

}
