package net.noresttherein.sugar.prettyprint

import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{MethodSymbol, TermSymbol, TypeTag}
import scala.util.Try

import net.noresttherein.sugar.prettyprint.extensions.{ClassNameExtension, ClassNameMethods, FieldsStringMethods}




trait extensions extends Any {

	/** Adds `innerClassName`, `localClassName` and `abbrevClassName` methods to any object providing a shorter alternative to `getClass.getName`. */
	@inline implicit final def classNameMethods[T](any :T) = new ClassNameMethods[T](any)

	/** Adds `innerName`, `localName` and `abbrevName` methods to `Class`, providing a shorter alternative to `getName`. */
	@inline implicit final def classNameExtension(clazz :Class[_]) = new ClassNameExtension(clazz)

	/** Implicit extension of any object of statically known type `T` (or for which type tag and class tag are available),
	  * providing methods for formatting it as a string
	  */
	@inline implicit final def fieldsStringMethods[T](obj :T) = new FieldsStringMethods(obj)

	/** Adds a `yesNo` and `yn` methods to `Boolean` values for shorter `String` representations. */
	@inline implicit final def yesNoMethod(boolean :Boolean) :YesNo = new YesNo(boolean)

}




object extensions {

	/** Implicit conversion patching any object with methods providing prettified/shortened class names. */
	class ClassNameMethods[T](private val self :T) extends AnyVal {

		/** An approximation of the imported type symbol of the class of this object, as it would be referenced
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
		@inline def innerClassName: String = innerNameOf(self.getClass)

		/** An approximation of the type name of the class of the given object, as it would appear in code.
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
		@inline def localClassName :String = localNameOf(self.getClass)

		/** An abbreviated qualified name of the class of this object, demangled to an approximation of how it would
		  * appear in code. All package names are replaced with their first letters, while the class name is demangled
		  * as follows: first, all trailing '$' are dropped and escape sequences
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
		@inline def abbrevClassName :String = abbrevNameOf(self.getClass)

		/** An approximation of the full, qualified and demangled name of the class of this object, as it would
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
		@inline def className :String = fullNameOf(self.getClass)

		/** Same as the default `Object.toString`, but uses
		  * `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.innerClassName innerClassName]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toInnerClassString :String =
			if (self == null) "null" else innerClassName + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses
		  * `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.localClassName localClassName]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toLocalClassString :String =
			if (self == null) "null" else localClassName + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses
		  * `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.abbrevClassName abbrevClassName]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toAbbrevClassString :String =
			if (self == null) "null" else abbrevClassName + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses
		  * (printable) `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.className className]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toFullNameString :String =
			if (self == null) "null" else className + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses
		  * `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.innerClassName innerClassName]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortInnerClassString :String =
			if (self == null) "null" else innerClassName + "@" + shortHashString

		/** Same as the default `Object.toString`, but uses
		  * `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.localClassName localClassName]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortLocalClassString :String =
			if (self == null) "null" else localClassName + "@" + shortHashString

		/** Same as the default `Object.toString`, but uses
		  * `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.abbrevClassName abbrevClassName]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortAbbrevClassString :String =
			if (self == null) "null" else abbrevClassName + "@" + shortHashString

		/** Same as the default `Object.toString`, but uses
		  * (printable) `this.`[[net.noresttherein.sugar.prettyprint.extensions.ClassNameMethods.className className]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortFullNameString :String =
			if (self == null) "null" else className + "@" + shortHashString

		/** Formats `this.hashCode` compacted to two bytes as a hexadecimal string. */
		@inline def shortHashString :String = {
			val hash = self.hashCode
			(hash ^ (hash >> 16) & 0xffff).toHexString
		}

		/** Formats `this.hashCode` as a hexadecimal string. */
		@inline def hashCodeString :String = self.hashCode.toHexString

		/** Formats the default [[java.lang.Object]] implementation of `hashCode` as a hexadecimal `String`. */
		@inline def identityHashCodeString :String = System.identityHashCode(self).toHexString
	}




	/** Extension methods formatting the name of a class in several ways, demangling the runtime class name
	  * to an approximation of the class symbol as it appears in code.
	  */
	class ClassNameExtension(private val self :Class[_]) extends AnyVal {
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
		@inline def name :String = fullNameOf(self)
	}




	/** Extension of any type T which generates string representation of its members. Implementor may choose
	  * to reflect over getter methods, case class fields, or all `val`s and `var`s, depending on the method chosen.
	  * As scala type reflection is used to obtain information about members of this object, it requires
	  * full static type information in order to be complete. For this reason it should be used exclusively
	  * to reflect objects referenced as 'this' (that is, from the inside of the reflected class). Attempt to
	  * reflect other objects will only provide information which is statically known about them. In other words,
	  * if the runtime type of the referenced object is more specialized than the static type of the reference,
	  * none of this additional information will be visible. For this reason, when using to reflect objects of
	  * classes defined in other projects (which source code cannot be modified), it is important to dynamically
	  * narrow down the type as much as possible.
	  *
	  * @author Marcin Mościcki
	  */
	class FieldsStringMethods[T, +R](private val subject :T) extends AnyVal {

		/** Local type name of this object. */
		def typeName(implicit typeTag :TypeTag[T]) :String = typeTag.tpe.typeSymbol.name.toString

		/** Lists all declared symbols designating getter methods for case fields. */
		def caseFields(implicit typeTag :TypeTag[T]) :Iterable[universe.MethodSymbol] =
			typeTag.tpe.decls.sorted.collect { case m if m.isMethod && m.asMethod.isCaseAccessor => m.asMethod }

		/** Lists all inherited and declared getter members. A getter is a synthetic method accessing an underlying
		  * `var` or `val` field; this list does not include user-defined `def` members.
		  */
		def getters(implicit typeTag :TypeTag[T]) :Iterable[universe.MethodSymbol] =
			typeTag.tpe.members.sorted.collect { case m if m.isMethod && m.asMethod.isGetter => m.asMethod }

		/** Lists all inherited and declared member fields (`val`s and `var`s). */
		def fields(implicit typeTag :TypeTag[T]):Iterable[universe.TermSymbol] =
			typeTag.tpe.members.sorted.collect {
				case m if m.isTerm && (m.asTerm.isVal || m.asTerm.isVar)  => typeTag.tpe.member(m.name.decodedName).asTerm
			}

		/** Given a list of parameterless methods of this object, generate a string in the format of ''name=value(, name=value)*''. */
		def getterStrings(fields :Iterable[MethodSymbol])(implicit typeTag :TypeTag[T]) :Iterable[String] = {
			val mirror = universe.runtimeMirror(getClass.getClassLoader)
			val reflection = mirror.reflect(subject)(ClassTag(mirror.runtimeClass(typeTag.tpe)).asInstanceOf[ClassTag[T]])
			fields.view.map { field =>
				reflection.reflectMethod(field)() match {
					case b :Boolean => field.name.toString + "=" + b.yesNo
					case value => field.name.toString + "=" + value
				}
			}
		}

		/** Given a list of fields of this object, generate a string in the format of ''name=value(, name=value)*''. */
		def fieldStrings(fields :Iterable[TermSymbol])(implicit typeTag :TypeTag[T]) :Iterable[String] = {
			val mirror = universe.runtimeMirror(getClass.getClassLoader)
			val reflection = mirror.reflect(subject)(ClassTag(mirror.runtimeClass(typeTag.tpe)).asInstanceOf[ClassTag[T]])
			fields.view.flatMap { field =>
				Try( //exclude constructor parameters which don't translate to member fields, as there is no good other way
					   reflection.reflectField(field).get match {
						   case b :Boolean => field.name.decodedName.toString.trim + "=" + b.yesNo
						   case value => field.name.decodedName.toString.trim + "=" + value
					   }
				   ).toOption
			}
		}


		/** Lists all declared and inherited getter methods of this object with their values, prefixed with the given string. */
		def gettersString(prefix :String)(implicit typeTag :TypeTag[T]) :String =
			getterStrings(getters).mkString(prefix + "(", ", ", ")")

		/** Formats this object by following its type name with the list of all member getter methods paired with their values. */
		def gettersString(implicit typeTag :TypeTag[T]) :String =
			getterStrings(getters).mkString(typeName + "(", ", ", ")")

		/** Lists all declared and inherited field members of this object with their values, prefixed with the given string. */
		def fieldsString(prefix :String)(implicit typeTag :TypeTag[T]) :String =
			fieldStrings(fields).mkString(prefix + "(", ", ", ")")

		/** Formats this object by following its type name with the list of all member fields paired with their values. */
		def fieldsString(implicit typeTag :TypeTag[T]) :String =
			fieldStrings(fields).mkString(typeName + "(", ", ", ")")

		/** Lists all case fields of this object with their values, prefixed with the given string. */
		def caseFieldsString(prefix :String)(implicit typeTag :TypeTag[T]) :String =
			getterStrings(caseFields).mkString(prefix + "(", ", ", ")")

		/** Formats this object by following its type name with the list of all case fields paired with their values. */
		def caseFieldsString(implicit typeTag :TypeTag[T]) :String =
			getterStrings(caseFields).mkString(typeName + "(", ", ", ")")

	}

}
