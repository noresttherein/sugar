package net.noresttherein.slang.prettyprint

import net.noresttherein.slang.prettyprint.YesNo.shorthandBoolean

import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{MethodSymbol, TermSymbol, TypeTag}
import scala.util.Try


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
class ObjectFormatter[T](private val subject :T) extends AnyVal {

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
		typeTag.tpe.members.sorted.collect { case m if m.isTerm && (m.asTerm.isVal || m.asTerm.isVar)  => typeTag.tpe.member(m.name.decodedName).asTerm }

	/** Given a list of parameterless methods of this object, generate a string in the format of ''name=value(, name=value)*''. */
	def getterStrings(fields :Iterable[MethodSymbol])(implicit typeTag :TypeTag[T]) :Iterable[String] = {
		val mirror = universe.runtimeMirror(getClass.getClassLoader)
		val reflection = mirror.reflect(subject)(ClassTag(mirror.runtimeClass(typeTag.tpe)).asInstanceOf[ClassTag[T]])
		fields.view.map { field =>
			reflection.reflectMethod(field)() match {
				case b :Boolean => field.name + "="+ b.yesno
				case value => field.name + "=" + value
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
					   case b :Boolean => field.name.decodedName.toString.trim + "=" + b.yesno
					   case value => field.name.decodedName.toString.trim + "=" + value
				   }
			   ).toOption
		}
	}


	/** Lists all declared and inherited getter methods of this object with their values, prefixed with the given string. */
	def gettersString(prefix :String)(implicit typeTag :TypeTag[T]) :String =
		getterStrings(getters).mkString(prefix+"(", ", ", ")")

	/** Formats this object by following its type name with the list of all member getter methods paired with their values. */
	def gettersString(implicit typeTag :TypeTag[T]) :String =
		getterStrings(getters).mkString(typeName+"(", ", ", ")")

	/** Lists all declared and inherited field members of this object with their values, prefixed with the given string. */
	def fieldsString(prefix :String)(implicit typeTag :TypeTag[T]) :String =
		fieldStrings(fields).mkString(prefix + "(", ", ", ")")

	/** Formats this object by following its type name with the list of all member fields paired with their values. */
	def fieldsString(implicit typeTag :TypeTag[T]) :String =
		fieldStrings(fields).mkString(typeName+"(", ", ", ")")

	/** Lists all case fields of this object with their values, prefixed with the given string. */
	def caseFieldsString(prefix :String)(implicit typeTag :TypeTag[T]) :String =
		getterStrings(caseFields).mkString(prefix+"(", ", ", ")")

	/** Formats this object by following its type name with the list of all case fields paired with their values. */
	def caseFieldsString(implicit typeTag :TypeTag[T]) :String =
		getterStrings(caseFields).mkString(typeName+"(", ", ", ")")


}


//	/** Implicit extension of any object of statically known type `T` (or for which type tag and class tag are available),
//	  * providing methods for formatting it as a string
//	  */
//	@inline implicit def ObjectFormatter[T :ClassTag :TypeTag](obj :T) :ObjectFormatter[T] = new ObjectFormatter[T](obj)


object ObjectFormatter {

	/** Implicit extension of any object of statically known type `T` (or for which type tag and class tag are available),
	  * providing methods for formatting it as a string
	  */
	@inline implicit def objectFormatter[T :TypeTag](obj :T) :ObjectFormatter[T] = new ObjectFormatter[T](obj)


	/** Base class providing a `toString` implementation listing the values of all fields
	  * of extending class `Self` with their names.
	  */
	class DefToString[Self <: DefToString[Self] : TypeTag] { this: Self =>
		override def toString :String = (this: Self).gettersString
	}




	/** Base class providing a `toString` implementation listing the values of all case class fields
	  * of extending case class `Self` with their names.
	  */
	class CaseClass[Self <:CaseClass[Self] :TypeTag] extends Serializable { this :Self =>
		override def toString :String = (this :Self).caseFieldsString
	}

}


