package net.noresttherein.sugar.format

import scala.reflect.runtime.universe.{runtimeMirror, TypeTag}

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.reflect.PropertyPath
import net.noresttherein.sugar.reflect.prettyprint.innerNameOf
import net.noresttherein.sugar.vars.Potential

//implicits
import net.noresttherein.sugar.extensions.classNameMethods




/** A utility base trait for formats which introduces new [[net.noresttherein.sugar.format.Format.Parts Parts]]
  * and [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]], which use reflection to derive property and model
  * names from their getter functions and type.
  * It can be safely mixed in to other traits, including full `Format` implementations.
  * @author Marcin Mościcki
  */
trait ReflectingFormat extends Format {

	trait ReflectingParts[M] extends Parts[M] {
		/** A part representing a property of type `P` of molded type `M`.
		  * It is the same `Part` as returned
		  * by [[net.noresttherein.sugar.format.Format.Parts.apply apply]]`(name :String)(part)`,
		  * but its name is set to the reflected name of the function.
		  * @param part A property getter. Must consist of any number of chained calls to parameterless methods.
		  *             Must not throw exceptions -
		  *             use instead [[net.noresttherein.sugar.vars.Potential Potential]]
		  *             or [[net.noresttherein.sugar.vars.Fallible Fallible]] as the property type.
		  */
		def apply[P](part :M => P)(implicit tag :TypeTag[M], mold :Mold[P]) :Part[M, P] =
			apply(propertyName(PropertyPath.nameOf(part)))(part)

		/** A part representing a property of type `P` of molded type `M`.
		  * Parsing of the property is allowed to fail without automatically propagating the error.
		  * Instead the application receives a `Potential[P]` which can be inspected before continuing the parsing.
		  * If [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] is passed as the argument to `part`,
		  * nothing is consumed from the input or appended to the output.
		  *
		  * It is the same `Part` as returned
		  * by [[net.noresttherein.sugar.format.Format.Parts.opt opt]]`(name :String)(part)`,
		  * but its name is set to the reflected name of the function.
		  * @param part A property getter. Must consist of any number of chained calls to parameterless methods.
		  *             Must not throw exceptions -
		  *             use instead [[net.noresttherein.sugar.vars.Potential Potential]]
		  *             or [[net.noresttherein.sugar.vars.Fallible Fallible]] as the property type.
		  */
		def opt[P](part :M => P)(implicit tag :TypeTag[M], mold :Mold[P]) :Part[M, Potential[P]] =
			opt(propertyName(PropertyPath.nameOf(part)))(part)
	}

	type ReflectingMoldmaker[M] = MoldmakerTemplate[M, ReflectingParts]

	private class NamedReflectingMoldmaker[M](name :String)
		extends NamedParts[M](name) with ReflectingParts[M] with ReflectingMoldmaker[M]
	{
		override def flatMap(construct :ReflectingParts[M] => Mold[M]) = wrap(name)(construct(this))
	}

	override def apply[M](name :String) :ReflectingMoldmaker[M] = new NamedReflectingMoldmaker(name)
	override def apply[M](model :Class[M]) :ReflectingMoldmaker[M] = apply(model.innerClassName)
	def apply[M :TypeTag] :ReflectingMoldmaker[M] = apply(modelName[M])


	protected def typeName[M](implicit tag :TypeTag[M]) :String = tag.tpe.typeSymbol.name.decodedName.toString

	protected def modelName[M](implicit tag :TypeTag[M]) :String = {
		val runtime = runtimeMirror(getClass.getClassLoader)
		val cls = runtime.runtimeClass(tag.tpe.dealias.erasure.typeSymbol.asClass)
		innerNameOf(cls)
	}


	protected def propertyName(name :String) :String

	protected def underscored(name :String) :String = name.replace('.', '_')

	protected def camelCase(name :String) :String = {
		val len = name.length
		val res = new JStringBuilder(name.length)
		var copied = 0
		var dot = name.indexOf('.'.toInt)
		while (dot >= 0) {
			res append name.substring(copied, dot)
			dot += 1
			copied = dot
			if (dot < len) {
				val char = name.charAt(dot)
				if (Character.isLetter(char)) {
					res append char.toUpper
					copied += 1
					dot = name.indexOf('.'.toInt, copied)
				}
			} else
				dot = - 1
		}
		res append name.substring(copied, len)
		res.toString
	}
}




//todo: for this to work, everything from Format object would have to be duplicated - for ReflectingParts
//  we must have a ReflectingFormat, and MoldLayout accepts any Format. At the same time,
//  MoldLayout should be a subtype of ReflectingMoldLayout (and anything else we'll come up with).
//@SerialVersionUID(ver)
//object ReflectingFormat {
//
//
//	/** A factory of [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s,
//	  * themselves factories of [[net.noresttherein.sugar.format.Format.Mold! Mold]]s
//	  * for any [[net.noresttherein.sugar.format.Format Format]] given as the argument.
//	  * This allows reusing a once defined the structure of formatted model object,
//	  * and create actual parser/formatters for multiple formats, such as XML and JSON, for example.
//	  *
//	  * It allows to define layouts in two ways:
//	  *   1. Monadic-like composition using its [[net.noresttherein.sugar.format.Format.MoldLayoutMaker.flatMap flatMap]],
//	  *      In the exact same way as [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] -
//	  *      see that type's documentation for more details. Example:
//	  * {{{
//	  *          for {
//	  *             dragon <- Format[Dragon]
//	  *             name   <- dragon("name")(_.name)
//	  *             color  <- dragon("color")(_.color)
//	  *             level  <- dragon("level")(_.level)
//	  *          } yield Dragon(name, color, level)
//	  *       }}}
//	  *   1. Factory methods creating adapter `Mold`s based on functions passed as arguments,
//	  *      the same way that individual `Mold`s can be created by analogical factory methods
//	  *      of [[net.noresttherein.sugar.format.Format.Mold$ Mold]] object from any `Format` instance.
//	  *
//	  * A `MoldLayoutMaker[S]` can be created by factory `apply` methods defined
//	  * in singleton object [[net.noresttherein.sugar.format.Format$ Format]]; their signatures are exactly the same
//	  * as those of factory methods defined in ''class'' [[net.noresttherein.sugar.format.Format Format]]
//	  * (available on any `Format` instance), except they return
//	  * [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s instead of `Mold`s.
//	  */
//	trait ReflectingMoldLayoutMaker[M] extends MoldLayoutFactory[M] {
//		override val fmt :ReflectingFormat
//		import fmt.{Mold, ReflectingParts}
//
//		/** Takes a `Mold` constructor for internals of type `M` and wraps it up as a universal mold layout of that type.
//		  * This works the same way as in [[net.noresttherein.sugar.format.Format.MoldLayoutMaker MoldLayoutMaker]],
//		  * but in place of [[net.noresttherein.sugar.format.Format.Parts Parts]], their subtype
//		  * [[net.noresttherein.sugar.format.ReflectingFormat.ReflectingParts ReflectingParts]] are made available
//		  * to the constructor function.
//		  * @see [[net.noresttherein.sugar.format.Format.wrap]]
//		  */
//		def flatMap(mold :ReflectingParts[M] => Mold[M]) :MoldLayout[M]
//	}
//
//
//
//	private class ReflectingNamedMoldLayoutMaker[M](name :String)
//		extends NamedMoldLayoutFactory[M](name) with ReflectingMoldLayoutMaker[M]
//	{
//		import fmt.{Mold, ReflectingParts}
//		override val fmt :ReflectingFormat = null
//
//		override def flatMap(mold :ReflectingParts[M] => Mold[M]) :MoldLayout[M] =
//			new MoldLayout[M] {
//				override def apply(format :Format) :format.Mold[M] =
//					format[M](name).flatMap(mold.asInstanceOf[format.Parts[M] => format.NamedMold[M]])
//			}
//		override def toString = "ReflectingMoldLayoutMaker[" + name + "]"
//	}
//}