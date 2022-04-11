package net.noresttherein.slang.prettyprint




trait extensions extends Any {

	/** Adds `innerClassName`, `localClassName` and `abbrevClassName` methods to any object providing a shorter alternative to `getClass.getName`. */
	@inline implicit final def classNameMethods[T](any :T) = new ClassNameMethods[T](any)

	/** Adds `innerName`, `localName` and `abbrevName` methods to `Class`, providing a shorter alternative to `getName`. */
	@inline implicit final def classExtension(clazz :Class[_]) = new ClassExtension(clazz)

	/** Implicit extension of any object of statically known type `T` (or for which type tag and class tag are available),
	  * providing methods for formatting it as a string
	  */
	@inline implicit final def fieldsStringMethods[T](obj :T) = new FieldsStringMethods(obj)

	/** Adds a `yesNo` and `yn` methods to `Boolean` values for shorter `String` representations. */
	@inline implicit final def yesNoMethod(boolean :Boolean) :YesNo = new YesNo(boolean)

}




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
	  * `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.innerClassName innerClassName]]
	  * instead of `this.getClass.getName`.
	  */
	@inline def toInnerClassString :String =
		if (self == null) "null" else innerClassName + "@" + self.hashCode.toHexString

	/** Same as the default `Object.toString`, but uses
	  * `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.localClassName localClassName]]
	  * instead of `this.getClass.getName`.
	  */
	@inline def toLocalClassString :String =
		if (self == null) "null" else localClassName + "@" + self.hashCode.toHexString

	/** Same as the default `Object.toString`, but uses
	  * `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.abbrevClassName abbrevClassName]]
	  * instead of `this.getClass.getName`.
	  */
	@inline def toAbbrevClassString :String =
		if (self == null) "null" else abbrevClassName + "@" + self.hashCode.toHexString

	/** Same as the default `Object.toString`, but uses
	  * (printable) `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.className className]]
	  * instead of `this.getClass.getName`.
	  */
	@inline def toFullNameString :String =
		if (self == null) "null" else className + "@" + self.hashCode.toHexString

	/** Same as the default `Object.toString`, but uses
	  * `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.innerClassName innerClassName]]
	  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
	  */
	@inline def toShortInnerClassString :String =
		if (self == null) "null" else innerClassName + "@" + shortHashString

	/** Same as the default `Object.toString`, but uses
	  * `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.localClassName localClassName]]
	  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
	  */
	@inline def toShortLocalClassString :String =
		if (self == null) "null" else localClassName + "@" + shortHashString

	/** Same as the default `Object.toString`, but uses
	  * `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.abbrevClassName abbrevClassName]]
	  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
	  */
	@inline def toShortAbbrevClassString :String =
		if (self == null) "null" else abbrevClassName + "@" + shortHashString

	/** Same as the default `Object.toString`, but uses
	  * (printable) `this.`[[net.noresttherein.slang.prettyprint.ClassNameMethods.className className]]
	  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
	  */
	@inline def toShortFullNameString :String =
		if (self == null) "null" else className + "@" + shortHashString

	/** Formats `this.hashCode` compacted to two bytes as a hexadecimal string. */
	@inline def shortHashString :String = {
		val hash = self.hashCode
		(hash ^ (hash >> 16) & 0xffff).toHexString
	}
}




/** Extension methods formatting the name of a class in several ways, demangling the runtime class name
  * to an approximation of the class symbol as it appears in code.
  */
class ClassExtension(private val self :Class[_]) extends AnyVal {
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

