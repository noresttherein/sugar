package net.noresttherein.sugar.reflect

import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.illegal_!


/**
  * @define innerClassNameDocs  First, the whole package prefix and all trailing '$' characters are dropped.
  *                             Then, all escape sequences for special characters which are legal for use in identifiers
  *                             are unescaped to their original forms. Then, dropped is the prefix up until and including
  *                             to the last '$'. If the class is specialized, its mangled type parameters are resolved
  *                             and composed in a type parameter list in Scala's syntax. If the class is anonymous,
  *                             a simple '.anon' replaces its whole anonymous name section, and prepended to it
  *                             is the directly preceding/enclosing class name, that is the inner-most class name
  *                             from the non-anonymous prefix. Primitive types are capitalized to their Scala names,
  *                             while their Java boxes receive "J" as a prefix. Arrays are formatted recursively
  *                             as 'Array['`innerClassName(element)`']'.
  *
  *                             This algorithm is a heuristic and can only be used for informational, debugging purposes,
  *                             and not for identifiers or in any sort of reflection operations, as it can fail to produce
  *                             the correct and unique type representation for a number of reasons. Most notably,
  *                             any kind of generic, non-specialized classes will not have any type arguments listed,
  *                             and only `@specialized` type parameters of specialized classes will be shown.
  *                             Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
  *                             Finally, for obvious reasons, names of anonymous classes are synthetic and the same
  *                             for all anonymous inner classes of the same enclosing class/object.
  *
  * @define localClassNameDocs  It doesn't include the package prefix, but includes demangled names of all enclosing
  *                             classes/objects. The demangling proceeds as follows: first, all trailing '$' characters
  *                             are dropped. Then, all escape sequences for special characters which are legal for use
  *                             in identifiers are unescaped to their original forms. All individual '$' signs
  *                             (used in name mangling of inner classes as the separators) are replaced with a '.',
  *                             and so is the double '$$' in '$$anon' sequence for anonymous classes.
  *                             If the class is specialized, its mangled type parameters are resolved and composed
  *                             in a type parameter list in Scala's syntax. Primitive types are capitalized
  *                             to their Scala names, while their Java box classes receive "J" as a prefix.
  *                             Arrays are formatted recursively as 'Array['`localClassName(element)`']'.
  *
  *                             This algorithm is a heuristic and can only be used for informational, debugging purposes,
  *                             and not for identifiers or in any sort of reflection operations, as it can fail to produce
  *                             the correct and unique type representation for a number of reasons. Most notably,
  *                             any kind of generic, non-specialized classes will not have any type arguments listed,
  *                             and only `@specialized` type parameters of specialized classes will be shown.
  *                             Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
  *                             Finally, for obvious reasons, names of anonymous classes are synthetic.
  *
  * @define abbrevClassNameDocs All package names are replaced with their first letters, while the class name
  *                             is demangled as follows: first, all trailing '$' are dropped and escape sequences
  *                             for characters which are legal for use in identifiers are unescaped.
  *                             Encoding of type arguments for `@specialized` classes is resolved and replaced
  *                             with a parameter list, as it would occur in the code. Finally, all individual '$'
  *                             (used in particular for separating names of nested classes) are replaced with '.',
  *                             as is the double '$$' in '$$anon' marking an anonymous class. Primitive types
  *                             are capitalized to their Scala names and arrays are formatted recursively as
  *                             'Array['`abbrevName(element)`']'.
  *
  *                             This algorithm is a heuristic and can only be used for informational, debugging purposes,
  *                             and not for identifiers or in any sort of reflection operations, as it can fail to produce
  *                             the correct and unique type representation for a number of reasons. Most notably,
  *                             any kind of generic, non-specialized classes will not have any type arguments listed,
  *                             and only `@specialized` type parameters of specialized classes will be shown.
  *                             Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
  *                             Finally, anonymous classes receive synthetic names for obvious reasons.
  *
  * @define fullClassNameDocs   Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
  *                             for characters which are legal for use in identifiers are unescaped.
  *                             Encoding of type arguments for `@specialized` classes is resolved and replaced
  *                             with a parameter list, as it would occur in the code. Finally, all individual '$'
  *                             (used in particular for separating names of nested classes) are replaced with '.',
  *                             as is the double '$$' in '$$anon' marking an anonymous class. Primitive types
  *                             are capitalized to their Scala names and arrays are formatted recursively
  *                             as 'Array['`fullNameOf(element)`']'.
  *
  *                             This algorithm is a heuristic and can only be used for informational, debugging purposes,
  *                             and not for identifiers or in any sort of reflection operations,
  *                             as it can fail to produce the correct and unique type representation
  *                             for a number of reasons. Most notably, any kind of generic, non-specialized classes
  *                             will not have any type arguments listed, and only `@specialized` type parameters
  *                             of specialized classes will be shown. Use of '$' in a demangled name will throw it off,
  *                             as will identifiers quoted in backticks. Finally, anonymous classes
  *                             receive synthetic names for the obvious reason.
  */
package object prettyprint {
	private[prettyprint] final val Ver = 1L
	//todo: a fifth 'pretty'/'import' scheme which automatically imports everything in 'java.lang' and 'scala' package object


	/** An approximation of the imported type symbol of the class of the given object, as it would be referenced 
	  * in code.
	  *
	  * $innerClassNameDocs
	  */
	@inline def innerClassNameOf(o :Any) :String = innerNameOf(o.getClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced 
	  * in code.
	  *
	  * $innerClassNameDocs
	  */
	@inline def innerNameOf[C :ClassTag] :String = innerNameOf(classTag[C].runtimeClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced 
	  * in code.
	  *
	  * $innerClassNameDocs
	  */
	def innerNameOf(clazz :Class[_]) :String = innerClassName(clazz.getName)


	/** Attempts to decode the binary, JVM class name to its unqualified Scala source symbol.
	  *
	  * $innerClassNameDocs
	  */
	def innerClassName(className :String) :String = {
		def buildInnerName(offset :Int, buffer :JStringBuilder) :JStringBuilder =
			if (offset == className.length)
				illegal_!("Not a valid class name '" + className + "'")
			else if (className.charAt(offset) == '[')
				buildInnerName(offset + 1, buffer append "Array[") append ']'
			else if (offset > 0 && className.charAt(offset - 1) == '[' && className.charAt(offset) == 'L')
				buildInnerName(offset + 1, buffer)
			else if (javaToScala(className, offset, buffer, boxTypeAliases))
				buffer
			else {
				val len   = className.length
				val local = className.lastIndexOf('.') + 1
				val anon  = className.indexOf("$$anon", local)
				val end   =
					if (anon >= 0) anon
					else if (local == len) len
					else typeNameEnd(className, offset)
				val rollbackIndex = buffer.length
				var i = local
				while (i < end) className.charAt(i) match {
					case '$' =>
						i += 1
						if (className.startsWith(specializationPrefix, i))
							i = demangleSpecialization(className, i, end, buffer)
						else {
							val jump = unescape(className, i, buffer)
							if (jump == i) //no escape sequence, individual '$' treated as a class name separator
								buffer.delete(rollbackIndex, buffer.length)
							i = jump
						}
					case c => buffer append c; i += 1
				}
				if (anon >= 0)
					buffer append ".anon"
				buffer
			}

		val primitive = primitiveNames.indexOf(className)
		if (primitive >= 0)
			typeParamNames(primitive)
		else
			buildInnerName(0, new JStringBuilder(className.length)).toString
	}




	/** An approximation of the type name of the class of the given object, as it would appear in code.
	  *
	  * $localClassNameDocs
	  */
	@inline def localClassNameOf(obj :Any): String = localNameOf(obj.getClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  *
	  * $localClassNameDocs
	  */
	@inline def localNameOf[C :ClassTag] :String = localNameOf(classTag[C].runtimeClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  *
	  * $localClassNameDocs
	  */
	def localNameOf(clazz :Class[_]) :String = localClassName(clazz.getName)

	/** An approximation of the type name of the given class, as it would appear in code.
	  *
	  * $localClassNameDocs
	  */
	def localClassName(className :String) :String = {
		def buildLocalName(offset :Int, buffer :JStringBuilder) :JStringBuilder =
			if (offset == className.length)
				illegal_!("Not a valid class name '" + className + "'")
			else if (className.charAt(offset) == '[')
				buildLocalName(offset + 1, buffer append "Array[") append ']'
			else if (offset > 0 && className.charAt(offset - 1) == '[' && className.charAt(offset) == 'L')
				buildLocalName(offset + 1, buffer)
			else if (javaToScala(className, offset, buffer, boxTypeAliases))
				buffer
			else {
				val local = className.lastIndexOf('.') + 1
				val end   = typeNameEnd(className, offset)
				demangleClassName(className, local, end, buffer)
				buffer
			}
		val primitive = primitiveNames.indexOf(className)
		if (primitive >= 0)
			typeParamNames(primitive)
		else
			buildLocalName(0, new JStringBuilder(className.length)).toString
	}


	/** An abbreviated qualified name of the class of the given object, demangled to an approximation of how it would 
	  * appear in code.
	  *
	  * $abbrevClassNameDocs
	  */
	@inline def abbrevClassNameOf(obj :Any) :String = abbrevNameOf(obj.getClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would 
	  * appear in code.
	  *
	  * $abbrevClassNameDocs
	  */
	@inline def abbrevNameOf[C :ClassTag] :String = abbrevNameOf(classTag[C].runtimeClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would 
	  * appear in code.
	  *
	  * $abbrevClassNameDocs
	  */
	def abbrevNameOf(clazz :Class[_]) :String = abbrevClassName(clazz.getName)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would
	  * appear in code.
	  *
	  * $abbrevClassNameDocs
	  */
	def abbrevClassName(className :String) :String = {
		def buildAbbrevName(offset :Int, buffer :JStringBuilder) :JStringBuilder =
			if (offset == className.length)
				illegal_!("Not a valid class name '" + className + "'")
			else if (className.charAt(offset) == '[')
				buildAbbrevName(offset + 1, buffer append "Array[") append ']'
			else if (offset > 0 && className.charAt(offset - 1) == '[' && className.charAt(offset) == 'L')
				buildAbbrevName(offset + 1, buffer)
			else if (javaToScala(className, offset, buffer, boxTypeAbbrevs))
				buffer
			else {
				val local  = className.lastIndexOf('.') + 1
				val end    = typeNameEnd(className, offset)
				var i      = offset
				var wasDot = true
				while (i < local) {
					if (className.charAt(i) == '.') {
						buffer append '.'
						wasDot = true
					} else if (wasDot) {
						buffer append className.charAt(i)
						wasDot = false
					}
					i += 1
				}
				demangleClassName(className, local, end, buffer)
				buffer
			}

		val primitive = primitiveNames.indexOf(className)
		if (primitive >= 0)
			typeParamNames(primitive)
		else
			buildAbbrevName(0, new JStringBuilder(className.length)).toString
	}


	/** An approximation of the full, qualified and demangled name of the class of the given object, as it would appear
	  * in code.
	  *
	  * $fullClassNameDocs
	  */
	@inline def classNameOf(obj :Any) :String = fullNameOf(obj.getClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would appear in code.
	  *
	  * $fullClassNameDocs
	  */
	@inline def fullNameOf[T :ClassTag] :String = fullNameOf(classTag[T].runtimeClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would appear in code.
	  *
	  * $fullClassNameDocs
	  */
	def fullNameOf(clazz :Class[_]) :String = demangledName(clazz.getName)
/*
	clazz match {
		case Void.TYPE        => "Unit"
		case AnyRef           => "AnyRef"
//		case BoxedUnit        => clazz.getName
		case Primitive()      => clazz.getName.capitalize
		case ArrayClass(elem) => "Array[" + fullNameOf(elem) + "]"
		case _                =>
			val qname = clazz.getName
			val start = qname.lastIndexOf('.') + 1
			val end = trimTrailingDollars(qname)
			val res = new JStringBuilder(qname.length)
			var i = 0
			while (i < start) {
				res append qname.charAt(i); i += 1
			}
			demangleClassName(qname, start, end, res)
			res.toString
	}
*/

	/** An approximation of the full, qualified and demangled name of the given class, as it would appear in code.
	  *
	  * $fullClassNameDocs
	  */
	def demangledName(className :String) :String = {
		def buildClassName(offset :Int, buffer :JStringBuilder) :JStringBuilder =
			if (offset == className.length)
				illegal_!("Not a valid class name '" + className + "'")
			else if (className.charAt(offset) == '[')
				buildClassName(offset + 1, buffer append "Array[") append ']'
			else if (offset > 0 && className.charAt(offset - 1) == '[' && className.charAt(offset) == 'L')
				buildClassName(offset + 1, buffer)
			else if (javaToScala(className, offset, buffer, boxTypeNames))
				buffer
			else {
				val local = className.lastIndexOf('.') + 1
				val end   = typeNameEnd(className, offset)
				var i     = 0
				while (i < local) {
					buffer append className.charAt(i);
					i += 1
				}
				demangleClassName(className, local, end, buffer)
				buffer
			}

		val primitive = primitiveNames.indexOf(className)
		if (primitive >= 0)
			typeParamNames(primitive)
		else
			buildClassName(0, new JStringBuilder(className.length)).toString
	}



	private def javaToScala(className :String, offset :Int, res :JStringBuilder, boxNames :Array[String]) :Boolean = {
		def illegal() =
			illegal_!("Not a valid class name: '" + className + "'")
		if (offset > 0 && offset == className.length - 1 && className.charAt(offset - 1) == '[') {
			val i = typeParamCodes.indexOf(className.charAt(offset))
			if (i < 0)
				illegal()
			res append typeParamNames(i)
			true
		} else if (offset > 0 && className.charAt(offset - 1) != 'L')
			illegal()
		else if ({
			val boxedUnitName   = "scala.runtime.BoxedUnit"
			val boxedUnitLength = boxedUnitName.length
			if (offset == 0)
				className == boxedUnitName
			else
				offset + boxedUnitLength + 1 == className.length && className.charAt(offset + boxedUnitLength) == ';'
		}) {
			res append "Unit"
			true
		} else if (className.startsWith("java.lang.", offset)) {
			val suffixLen = if (offset > 0) 1 else 0 //array names end with ';'
			if (className.startsWith("Object", offset + 10) && className.length == offset + 16 + suffixLen) {
				res append "AnyRef"
				true
			} else {
				var i = NumberOfPrimitives - 1
				while (i >= 0 && {
					val box = boxTypeNames(i)
					className.length - offset - suffixLen != box.length || !className.startsWith(box, offset)
				})
					i -= 1
				i >= 0 && { res append boxNames(i); true }
			}
		} else
			false
	}

	private def typeNameEnd(input :String, offset :Int) :Int =
		if (offset == 0)
			trimTrailingDollars(input)
		else {
			if (input.charAt(input.length - 1) != ';')
				illegal_!("Not a valid class name '" + input + "' (missing suffix ';')")
			trimTrailingDollars(input, input.length - 2)
		}

	private def trimTrailingDollars(input :String, before :Int = -1) :Int = {
		var i = if (before < 0 ) input.length - 1 else before
		while (i >= 0 && input.charAt(i) == '$')
			i -= 1
		i + 1
	}


	private def demangleClassName(input :String, offset :Int, end :Int, result :JStringBuilder) :Unit = {
		var i  = offset
		while (i < end) input.charAt(i) match {
			case '$' =>
				i += 1
				if (input.startsWith(specializationPrefix, i))
					i = demangleSpecialization(input, i, end, result)
				else if (input.startsWith("anon", i)) {
					result append "anon"; i += 4
				} else {
					val jump = unescape(input, i, result)
					if (jump == i)
						result append '.'
				}

			case c => 
				result append c; i += 1
		}
	}


	private def unescape(input :String, offset :Int, result :JStringBuilder) :Int = {
		var s = escapes.length
		var symbol = ""
		while (s > 0) {
			s -= 1
			symbol = escapes(s)
			if (input.startsWith(symbol, offset)) {
				result append symbols(s)
				return offset + symbol.length
			}
		}
		offset
	}


	private def demangleSpecialization(input :String, offset :Int, end :Int, result :JStringBuilder) :Int = {
		//inputStartsWith("mc", offset)
		val rollbackPoint = result.length
		result append '['

		def rollback() = {
			result.delete(rollbackPoint, result.length)
			result append '.' //for the '$' starting '$mc'.
			offset //no infinite loop as input doesn't start with a '$' and "mc" will get normal char treatment
		}
		@tailrec def demangle(pos :Int, params :List[String] = Nil) :Int =
			if (pos == end)  //end of input encountered before the closing sequence of specialized parameters encoding
				rollback()
			else {
				val param = input.charAt(pos)
				var t = typeParamCodes.length - 1
				while (t >= 0 && typeParamCodes(t) != param)
					t -= 1
				if (t >= 0)
					demangle(pos + 1, typeParamNames(t)::params)
				else if (input.startsWith("$sp", pos) && params.nonEmpty) {
					params foreach { p => result append p append ',' }
					result.delete(result.length - 1, result.length) //last ','
					result append ']'
					pos + 3
				} else  //illegal specialization mangling, revert to standard behaviour
					rollback()
			}
		demangle(offset + 2)
	}


//	private[this] val AnyRef    = classOf[AnyRef]
//	private[this] val BoxedUnit = classOf[BoxedUnit]
	private[this] val escapes = Array(//empty string at start as an old fashioned guard which maps to '.' at the same time
		/* "",*/ "tilde", "bang", "at", "hash", "percent", "up", "amp", "times", "minus", "plus", "eq", "less", "greater",
		"qmark", "div", "bar", "bslash", "colon"
	)
	private[this] val symbols = Array(
		/*'.',*/ '~', '!', '@', '#', '%', '^', '&', '*', '-', '+', '=', '<', '>', '?', '/', '|', '\\', ':'
	)

	private[this] final val NumberOfPrimitives = 9
	private[this] val typeParamCodes = Array('I', 'J', 'D', 'C', 'B', 'F', 'Z', 'S', 'V')
	private[this] val primitiveNames = Array("int", "long", "double", "char", "byte", "float", "boolean", "short", "void")
	private[this] val typeParamNames = Array("Int", "Long", "Double", "Char", "Byte", "Float", "Boolean", "Short", "Unit")
	private[this] val boxTypeAliases = Array("JInt", "JLong", "JDouble", "JChar", "JByte", "JFloat", "JBoolean", "JShort", "")
	private[this] val boxTypeAbbrevs = Array(
		"j.l.Integer", "j.l.Long", "j.l.Double", "j.l.Character", "j.l.Byte", "j.l.Float", "j.l.Boolean", "j.l.Short", ""
	)
	private[this] val boxTypeNames   = Array(
		"java.lang.Integer", "java.lang.Long", "java.lang.Double", "java.lang.Character", "java.lang.Byte",
		"java.lang.Float", "java.lang.Boolean", "java.lang.Short", ""
	)

	private[this] final val specializationPrefix = "mc"
	
}

