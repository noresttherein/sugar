package net.turambar.slang


import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{TypeTag, typeTag, TermSymbol, MethodSymbol, Mirror}


package object prettyprint {
	
	def unqualifiedClassName(obj :Any): String = unqualifiedNameOf(obj.getClass)
	
	def unqualifiedNameOf[C :ClassTag] :String = unqualifiedNameOf(classTag[C].runtimeClass)
	
	def unqualifiedNameOf(clss :Class[_]) :String = {
		val name = clss.getName
		name.substring(name.lastIndexOf('.')+1)
	}
	
	def shortNameOf[C :ClassTag] :String = shortNameOf(classTag[C].runtimeClass)
	
	def shortNameOf(clazz :Class[_]) :String = {
		val name = clazz.getName
		val uq = name.substring(name.lastIndexOf('.')+1)
		var i = uq.lastIndexOf('$', uq.length-2)
		while (i>0 && uq.startsWith("anon", i+1))
			i = uq.lastIndexOf('$', i-1)
		uq.substring(i+1)
	}
	
	def shortClassName(o :Any) :String = shortNameOf(o.getClass)

	implicit class Prettyfier(private val self :Any) extends AnyVal {
		def shortClassName = shortNameOf(self.getClass)
	}
	
	
	
	
	
	
	
	/** Extension of any type T which generates string representation of its fields
	  * (defined as getter methods for underlying fields or case class fields depending on method chosen).
	  * @author Marcin Mościcki
	  */
	class ToString[T](val subject :T) extends AnyVal {
		
		def typeName(implicit typeTag :TypeTag[T]) = typeTag.tpe.typeSymbol.name.toString
		
		def caseFields(implicit typeTag :TypeTag[T]) =
			typeTag.tpe.decls.collect { case m if m.isMethod && m.asMethod.isCaseAccessor => m.asMethod }
		
		def fields(implicit typeTag :TypeTag[T]) =
			typeTag.tpe.decls.collect { case m if m.isMethod && m.asMethod.isGetter => m.asMethod }
		
		def fieldStrings(fields :Iterable[MethodSymbol])(implicit classTag :ClassTag[T]) :Iterable[String] = {
			val mirror = universe.runtimeMirror(getClass.getClassLoader)
			val reflection = mirror.reflect(subject)
			fields.view.map { field =>
				field.name +"="+reflection.reflectMethod(field)()
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
		class DefToString[Self <: DefToString[Self] : TypeTag : ClassTag] {
			this: Self =>
			override def toString = (this: Self).fieldsString
		}

		/** Base class providing a `toString` implementation as a `lazy val` listing the values of all fields
		  * of extending class `Self` with their names.
		  */
		class LazyToString[Self <: LazyToString[Self] : TypeTag : ClassTag] {
			this: Self =>
			@transient
			override lazy val toString = (this: Self).fieldsString
		}

		/** Base class providing a `toString` as eagerly computed `val` implementation listing the values of all fields
		  * of extending class `Self` with their names.
		  */
		class EagerToString[Self <: EagerToString[Self] : TypeTag : ClassTag] {
			this: Self =>
			@transient
			override val toString = (this: Self).fieldsString
		}

	}

	import ToString.typePrinter
	
	/** Base class providing a `toString` implementation listing the values of all case class fields
	  * of extending case class `Self` with their names.
	  */
	class CaseClass[Self <:CaseClass[Self] :TypeTag :ClassTag] extends Serializable { this :Self =>
		override def toString = (this :Self).caseFieldsString
	}

	object CaseClass {

		/** Base class providing a `toString` implementation as a `lazy val` listing the values of all case class fields
		  * of extending case class `Self` with their names.
		  */
		class LazyCaseClass[Self <: LazyCaseClass[Self] : TypeTag : ClassTag] extends Serializable {
			this: Self =>
			@transient
			override lazy val toString = (this: Self).caseFieldsString
		}

		/** Base class providing a `toString` implementation as eagerly computed `val` listing the values of all case class fields
		  * of extending case class `Self` with their names.
		  */
		class EagerCaseClass[Self <: EagerCaseClass[Self] : TypeTag : ClassTag] extends Serializable {
			this: Self =>
			@transient
			override val toString = (this: Self).caseFieldsString
		}

	}

	class YesNo(val toBoolean :Boolean) extends AnyVal {
		def bit = if (toBoolean) 1 else 0
		def yesno = if (toBoolean) "yes" else "no"
		def yn = if (toBoolean) "y" else "n"
		override def toString = yn
	}

	object YesNo {
		def apply(is :Boolean) = new YesNo(is)

		@inline final val Yes = new YesNo(true)
		@inline final val No = new YesNo(false)

		implicit def shorthandBoolean(boolean :Boolean) :YesNo = new YesNo(boolean)
		implicit def isYesOrNo(yn :YesNo) :Boolean = yn.toBoolean
	}
}