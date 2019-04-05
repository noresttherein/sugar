package net.noresttherein.slang

import scala.reflect.{classTag, ClassTag}


package object prettyprint {
	//implicits


	/** Unqualified class name of the given object, that is its class name with all outer packages stripped.
	  * Note that this won't contain information about type arguments for type constructors.
	  * @return suffix of `clss.getName` starting after last '.' (or whole class name for top-level classes)
	  */
	@inline def unqualifiedClassName(obj :Any): String = unqualifiedNameOf(obj.getClass)


	/** The name of the class given as type parameter with all outer packages stripped. As an additional feature,
	  * for arrays the unqualified component name of the component is recursively printed surrounded by '[' ']'.
	  * Note that this won't contain information about type arguments for type constructors.
	  * @return suffix of `clss.getName` starting after last '.' (or whole class name for top-level classes)
	  */
	@inline def unqualifiedNameOf[C :ClassTag] :String = unqualifiedNameOf(classTag[C].runtimeClass)


	/** The name of the given class with all outer packages stripped. As an additional feature, if `clss` is
	  * an array type, the unqualified component name of the component is recursively printed surrounded by '[' ']'.
	  * Note that this won't contain information about type arguments for type constructors.
	  * @return suffix of `clss.getName` starting after last '.' (or whole class name for top-level classes)
	  */
	def unqualifiedNameOf(clss :Class[_]) :String = clss.getComponentType match {
		case elem if elem != null => //clss is an array
			'[' + unqualifiedClassName(elem) + ']'
		case _ =>
			val name = clss.getName
			name.substring(name.lastIndexOf('.')+1)
	}



	/** Returns the class name of the given object aa seen from the most inner containing scope (class or object).
	  * Anonymous classes will instead retain the name of the first non-synthetic (programmer named) scope.
	  * @return `localNameOf(o.getClass)`
	  * @see [[localNameOf]]
	  */
	@inline def localClassName(o :Any) :String = localNameOf(o.getClass)

	/** Attempts to strip the name of the class given as type parameter of all outer packages and containing classes or objects.
	  * First, it strips package names separated by dots as [[unqualifiedClassName()]] does, and then outer classes
	  * assuming the class name uses '$' as nesting separator as by compiler default. If the most inner identifier
	  * thus obtained is recognized as anonymous (in particular for lambdas), returned string will start with the
	  * most inner class/object identifier recognized as non-synthetic (that is, existing as in code with an optional '$').
	  * Note that this function is largely a heuristic and may not always return desired results.
	  */
	def localNameOf[C :ClassTag] :String = localNameOf(classTag[C].runtimeClass)


	/** Attempts to strip the name of the given class of all outer packages and containing classes or objects.
	  * First, it strips package names separated by dots as [[unqualifiedClassName()]] does, and then outer classes
	  * assuming the class name uses '$' as nesting separator as by compiler default. If the most inner identifier
	  * thus obtained is recognized as anonymous (in particular for lambdas), returned string will start with the
	  * most inner class/object identifier recognized as non-synthetic (that is, existing as in code with an optional '$').
	  * Note that this function is largely a heuristic and may not always return desired results.
	  */
	def localNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case elem if elem != null =>
			'[' + localNameOf(clazz) + ']'
		case _ =>
			val name = clazz.getName
			val uq = name.substring(name.lastIndexOf('.')+1)
			var i = uq.lastIndexOf('$', uq.length-2)
			while (i>0 && uq.startsWith("anon", i+1))
				i = uq.lastIndexOf('$', i-1)
			uq.substring(i+1)
	}




	/** Returns the name of the given class with all outer package names (as separated by dots) replaced with) their
	  * first letters. For nested classes, the names of all container classes or objects are preserved.
	  */
	@inline def shortNameOf[C :ClassTag] :String = shortNameOf(classTag[C].runtimeClass)


	/** Returns the name of the given class with all outer package names (as separated by dots) replaced with) their
	  * first letters. For nested classes, the names of all container classes or objects are preserved.
	  */
	def shortNameOf(clazz :Class[_]) :String = {
		val qname = clazz.getName; val len = qname.length
		val sb = new StringBuilder(len)
		var i = 0
		var lastDot = -1
		while (i > 0) qname(i) match {
			case '.' =>
				sb.delete(lastDot+2, sb.length)
				lastDot = sb.length
				sb += '.';  i += 1
			case char =>
				sb += char; i += 1
		}
		sb.toString
	}


	/** Implicit conversion patching any object with methods providing prittified/shortened class names. */
	implicit class ClassNameOf(private val self :Any) extends AnyVal {
		/** Class name of this object trimmed of all containing packages and non-anonymous surrounding classes but one. */
		@inline def localClassName: String = localNameOf(self.getClass)

		/** Fully qualified class name of this object with all containing package names replaced with their first letter. */
		@inline def shortClassName :String = shortNameOf(self.getClass)

		/** Fully qualified class name of this object, shorthand for `this.getClass.getName`. */
		@inline def className :String = self.getClass.getName
	}









}