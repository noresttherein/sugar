package net.noresttherein.sugar

import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.reflect.{classTag, ClassTag}





package object prettyprint {
	final val ver = 1L

	/** An approximation of the imported type symbol of the class of the given object, as it would be referenced 
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix. 
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively 
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational, 
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce 
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic, 
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters 
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers 
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	@inline def innerClassNameOf(o :Any) :String = innerNameOf(o.getClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced 
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix. 
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively 
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational, 
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce 
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic, 
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters 
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers 
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	@inline def innerNameOf[C :ClassTag] :String = innerNameOf(classTag[C].runtimeClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced 
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix. 
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively 
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational, 
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce 
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic, 
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters 
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers 
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	def innerNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qualified = clazz.getName
			val len = qualified.length
			val start = qualified.lastIndexOf('.') + 1
			val anon = qualified.indexOf("$$anon", start)
			val end =
				if (anon >= 0) anon
				else if (start == len) len
				     else trimTrailingDollars(qualified)
			var i = start
			val res = new JStringBuilder(end - i + 5) //5 for anon
			while (i < end) qualified.charAt(i) match {
				case '$' =>
					i += 1
					if (qualified.startsWith(specializationPrefix, i))
						i = demangleSpecialization(qualified, i, end, res)
					else {
						val jump = unescape(qualified, i, res)
						if (jump == i) //no escape sequence, individual '$' treated as a class name separator
							res.delete(0, res.length)
						i = jump
					}
				case c => res append c; i += 1
			}
			if (anon >= 0)
				res append ".anon"
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + innerNameOf(elem) + "]"
	}



	/** An approximation of the type name of the class of the given object, as it would appear in code.
	  * It doesn't include the package prefix, but includes demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively 
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational, 
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce 
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic, 
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters 
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	@inline def localClassNameOf(obj :Any): String = localNameOf(obj.getClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  * It doesn't include the package prefix, but includes demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	@inline def localNameOf[C :ClassTag] :String = localNameOf(classTag[C].runtimeClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  * It doesn't include the package prefix, but includes demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	def localNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qualified = clazz.getName
			val end = trimTrailingDollars(qualified)
			val start = qualified.lastIndexOf('.') + 1
			val res = new JStringBuilder(end - start)
			demangleClassName(qualified, start, end, res)
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + localNameOf(elem) + "]"
	}


	
	/** An abbreviated qualified name of the class of the given object, demangled to an approximation of how it would 
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences 
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments 
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced 
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized 
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'. 
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not 
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type 
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes 
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes 
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks. 
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline def abbrevClassNameOf(obj :Any) :String = abbrevNameOf(obj.getClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would 
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences 
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments 
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced 
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized 
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'. 
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not 
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type 
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes 
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes 
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks. 
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */	
	@inline def abbrevNameOf[C :ClassTag] :String = abbrevNameOf(classTag[C].runtimeClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would 
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences 
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments 
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced 
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized 
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'. 
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not 
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type 
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes 
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes 
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks. 
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	def abbrevNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qname = clazz.getName
			val end = trimTrailingDollars(qname)
			val sb = new JStringBuilder(end)
			val start = clazz.getPackage match {
				case null => 0
				case p =>
					val pname = p.getName; val plen = pname.length
					var i = 0
					if (plen > 0) {
						sb append pname.charAt(0)
						i += 1
					}
					while (i < plen) pname.charAt(i) match {
						case '.' => sb append '.' append pname.charAt(i + 1); i += 2
						case _ => i += 1
					}
					if (i > 0) {
						sb append '.'
						i += 1 //skip the '.' separating the package and class name
					}
					i
			}
			demangleClassName(qname, start, end, sb)
			sb.toString

		case c if c.isPrimitive => "Array[" + abbrevNameOf(c).capitalize + "]"
		case c => "Array[" + abbrevNameOf(c) + "]"
	}



	/** An approximation of the full, qualified and demangled name of the class of the given object, as it would appear
	  * in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences for characters
	  * which are legal for use in identifiers are unescaped. Encoding of type arguments for `@specialized` classes
	  * is resolved and replaced with a parameter list, as it would occur in the code. Finally, all individual '$'
	  * (used in particular for separating names of nested classes) are replaced with '.', as is the double '$$' 
	  * of '$$anon' marking an anonymous class. Primitive types are capitalized to their Scala names and arrays
	  * are formatted recursively as 'Array['`fullNameOf(element)`']'. This algorithm is a heuristic and can
	  * only be used for informational, debugging purposes, and not for identifiers or in any sort of reflection 
	  * operations, as it can fail to produce the correct and unique type representation for a number of reasons.
	  * Most notably, any kind of generic, non-specialized classes will not have any type arguments listed,
	  * and only `@specialized` type parameters of specialized classes will be shown. Use of '$' in a demangled name
	  * will throw it off, as will identifiers quoted in backticks. Finally, anonymous classes receive synthetic names
	  * for the obvious reason.
	  */
	@inline def classNameOf(obj :Any) :String = fullNameOf(obj.getClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would 
	  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences 
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments 
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced 
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized 
	  * to their Scala names and arrays are formatted recursively as 'Array['`fullNameOf(element)`']'. 
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not 
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type 
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes 
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes 
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks. 
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline def fullNameOf[T :ClassTag] :String = fullNameOf(classTag[T].runtimeClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would 
	  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences 
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments 
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced 
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized 
	  * to their Scala names and arrays are formatted recursively as 'Array['`fullNameOf(element)`']'. 
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not 
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type 
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes 
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes 
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks. 
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	def fullNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
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

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + fullNameOf(elem) + "]"
	}




	private def trimTrailingDollars(input :String) :Int = {
		var i = input.length - 1
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
				} else
					i = unescape(input, i, result)

			case c => result append c; i += 1
		}
	}


	private def unescape(input :String, offset :Int, result :JStringBuilder) :Int = {
		var s = escapes.length - 1
		var symbol = ""
		while ({ symbol = escapes(s); !input.startsWith(symbol, offset)}) //escapes has "" at 0 as a guard
			s -= 1
		val name = symbols(s)
		result append name
		offset + symbol.length
	}


	private def demangleSpecialization(input :String, offset :Int, end :Int, result :JStringBuilder) :Int = {
		//inputStartsWith("mc", offset)
		val rollbackPoint = result.length
		result append '['

		def rollback() = {
			result.delete(rollbackPoint, result.length)
			result append '.' //for the '$' starting '$mc'.
			offset //no infinite loop as input doesn't starti with a '$' and "mc" will get normal char treatment
		}
		@tailrec def demangle(pos :Int, params :List[String] = Nil) :Int =
			if (pos == end)  //end of input encoutered before the closing sequence of specialized parameters encoding
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


	
	private[this] val escapes = Array(//empty string at start as an old fashioned guard which maps to '.' at the same time
		"", "tilde", "bang", "at", "hash", "percent", "up", "amp", "times", "minus", "plus", "eq", "less", "greater",
		"qmark", "div", "bar", "bslash", "colon"
	)
	private[this] val symbols = Array(
		".", "~", "!", "@", "#", "%", "^", "&", "*", "-", "+", "=", "<", ">", "?", "/", "|", "\\", ":"
	)

	private[this] val typeParamCodes = Array('S', 'V', 'Z', 'C', 'F', 'B', 'D', 'J', 'I')
	private[this] val typeParamNames = Array("Short", "Unit", "Bool", "Char", "Float", "Byte", "Double", "Long", "Int")

	private[this] final val specializationPrefix = "mc"
	
}

