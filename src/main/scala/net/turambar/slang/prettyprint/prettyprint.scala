package net.turambar.slang


import scala.reflect.{classTag, ClassTag}
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{typeTag, MethodSymbol, Mirror, TermSymbol, TypeTag}


package object prettyprint {
	//implicits
	import  YesNo.shorthandBoolean
	import ToString.typePrinter

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
		def localClassName: String = localNameOf(self.getClass)

		/** Fully qualified class name of this object with all containing package names replaced with their first letter. */
		def shortClassName :String = shortNameOf(self.getClass)
	}
	
	
	
	
	
	
	
	/** Extension of any type T which generates string representation of its fields
	  * (defined as getter methods for underlying fields or case class fields depending on method chosen).
	  * @author Marcin Mościcki
	  */
	class ToString[T](val subject :T) extends AnyVal {
		//todo: include inherited fields.

		def typeName(implicit typeTag :TypeTag[T]) :String = typeTag.tpe.typeSymbol.name.toString
		
		def caseFields(implicit typeTag :TypeTag[T]) :Iterable[universe.MethodSymbol] =
			typeTag.tpe.decls.collect { case m if m.isMethod && m.asMethod.isCaseAccessor => m.asMethod }
		
		def fields(implicit typeTag :TypeTag[T]) :Iterable[universe.MethodSymbol] =
			typeTag.tpe.decls.collect { case m if m.isMethod && m.asMethod.isGetter => m.asMethod }
		
		def fieldStrings(fields :Iterable[MethodSymbol])(implicit classTag :ClassTag[T]) :Iterable[String] = {
			val mirror = universe.runtimeMirror(getClass.getClassLoader)
			val reflection = mirror.reflect(subject)
			fields.view.map { field =>
				reflection.reflectMethod(field)() match {
					case b :Boolean => field.name + "="+ b.yesno
					case value => field.name + "=" + value
				}
			}
		}
		
		def fieldsString(prefix :String)(implicit typeTag :TypeTag[T], classTag :ClassTag[T]) :String =
			fieldStrings(fields).mkString(prefix+"(", ", ", ")")
		
		def fieldsString(implicit typeTag :TypeTag[T], classTag :ClassTag[T]) :String =
			fieldStrings(fields).mkString(typeName+"(", ", ", ")")
		
		def caseFieldsString(prefix :String)(implicit typeTag :TypeTag[T], classTag :ClassTag[T]) :String =
			fieldStrings(caseFields).mkString(prefix+"(", ", ", ")")
		
		def caseFieldsString(implicit typeTag :TypeTag[T], classTag :ClassTag[T]) :String =
			fieldStrings(caseFields).mkString(typeName+"(", ", ", ")")
		
		
	}
	
	


	object ToString {
		
		implicit def typePrinter[T :ClassTag :TypeTag](obj :T) :ToString[T] = new ToString(obj)

		/** Base class providing a `toString` implementation listing the values of all fields
		  * of extending class `Self` with their names.
		  */
		class DefToString[Self <: DefToString[Self] : TypeTag : ClassTag] { this: Self =>
			override def toString :String = (this: Self).fieldsString
		}

		/** Base class providing a `toString` implementation as a `lazy val` listing the values of all fields
		  * of extending class `Self` with their names.
		  */
		class LazyToString[Self <: LazyToString[Self] : TypeTag : ClassTag] {
			this: Self =>
			@transient
			override lazy val toString :String = (this: Self).fieldsString
		}

		/** Base class providing a `toString` as eagerly computed `val` implementation listing the values of all fields
		  * of extending class `Self` with their names.
		  */
		class EagerToString[Self <: EagerToString[Self] : TypeTag : ClassTag] {
			this: Self =>
			@transient
			override val toString :String = (this: Self).fieldsString
		}

	}


	
	/** Base class providing a `toString` implementation listing the values of all case class fields
	  * of extending case class `Self` with their names.
	  */
	class CaseClass[Self <:CaseClass[Self] :TypeTag :ClassTag] extends Serializable { this :Self =>
		override def toString :String = (this :Self).caseFieldsString
	}

	object CaseClass {

		/** Base class providing a `toString` implementation as a `lazy val` listing the values of all case class fields
		  * of extending case class `Self` with their names.
		  */
		class LazyCaseClass[Self <: LazyCaseClass[Self] : TypeTag : ClassTag] extends Serializable { this: Self =>
			@transient override lazy val toString :String = (this: Self).caseFieldsString
		}

		/** Base class providing a `toString` implementation as eagerly computed `val` listing the values of all case class fields
		  * of extending case class `Self` with their names.
		  */
		class EagerCaseClass[Self <: EagerCaseClass[Self] : TypeTag : ClassTag] extends Serializable { this: Self =>
			//todo: transient and recomputing it on deserialization
			override val toString :String = (this: Self).caseFieldsString
		}

	}


	/** Provides alternative string representations of `Boolean` values in the form of ''yes/no'' or ''y/n'' (default).
	  * Makes for shorter `toString` results in classes containing several `Boolean` fields.
	  */
	class YesNo(val toBoolean :Boolean) extends AnyVal {
		def bit :Int = if (toBoolean) 1 else 0
		def yesno :String = if (toBoolean) "yes" else "no"
		def yn :String = if (toBoolean) "y" else "n"
		override def toString :String = yn
	}


	/** Patches `Boolean` values to print aa ''yes'' or ''no''. */
	object YesNo {
		def apply(is :Boolean) = new YesNo(is)

		@inline final val Yes = new YesNo(true)
		@inline final val No = new YesNo(false)

		implicit def shorthandBoolean(boolean :Boolean) :YesNo = new YesNo(boolean)
		implicit def isYes(yn :YesNo) :Boolean = yn.toBoolean
	}
}