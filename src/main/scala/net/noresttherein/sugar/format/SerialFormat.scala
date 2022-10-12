package net.noresttherein.sugar.format

import scala.reflect.ClassTag

import net.noresttherein.sugar.vars.Opt.Got
import net.noresttherein.sugar.vars.Potential
import net.noresttherein.sugar.witness.Maybe

//implicits
import net.noresttherein.sugar.extensions.classNameExtension




/** A class of formats in which object's [[net.noresttherein.sugar.format.Format.Part parts]] are written consecutively,
  * without additional information about the part itself aside of its value. This allows it to not require property
  * names in the factory methods of [[net.noresttherein.sugar.format.Format.Parts Parts]].
  */
trait SerialFormat extends Format {
/*
	protected override def wrap[S](name :String)(parts :Mold[S]) :Mold[S] =
		if (name.length == 0 || name == "_")

		else if (open == Mold.empty && close == Mold.empty)
			parts
		else
			super.wrap(name)(parts)
*/

	/** Returns the value `Mold` itself. */
	protected override def propertyMold[M](propertyName :String)(implicit valueMold :Mold[M]) :Mold[M] = valueMold

	protected override val open  :Mold[String] = Mold.emptyString
	protected override val close :Mold[String] = Mold.emptyString

	override def apply[M](name :String) :SerialMoldmaker[M] = new NamedSerialMoldmaker[M](name)
	override def apply[M](subject :Class[M]) :SerialMoldmaker[M] = apply(subject.innerName)
	def apply[M](implicit subject :Maybe[ClassTag[M]]) :SerialMoldmaker[M] = subject.opt match {
		case Got(tag) => apply[M](tag.runtimeClass.innerName)
		case _ => apply("_")
	}


	trait SerialParts[M] extends Parts[M] {
		def apply[P :Mold](part :M => P) :Part[M, P]
		def opt[P :Mold](part :M => P) :Part[M, Potential[P]]
		def mirror[P :Mold](part :M => P) :Part[M, (Liquid, P)]
	}

	type SerialMoldmaker[S] = MoldmakerTemplate[S, SerialParts]

	private class NamedSerialParts[M](name :String) extends NamedParts[M](name) with SerialParts[M] {
		override val format = SerialFormat.this
		override def apply[P :Mold](part :M => P)  :Part[M, P] = apply("_")(part)
		override def opt[P :Mold](part :M => P)    :Part[M, Potential[P]] = opt("_")(part)
		override def mirror[P :Mold](part :M => P) :Part[M, (Liquid, P)] = mirror("_")(part)
	}
	private class NamedSerialMoldmaker[M](name :String) extends NamedSerialParts[M](name) with SerialMoldmaker[M] {
		override val format = SerialFormat.this
		override def flatMap(construct :SerialParts[M] => Mold[M]) :Mold[M] = construct(this)
	}
}

