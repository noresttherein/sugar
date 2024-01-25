package net.noresttherein.sugar.format

import net.noresttherein.sugar.vars.{Opt, Outcome, Potential}




//todo: MoldLayoutTemplate[M, -F <: Format]; type MoldLayout[M] = MoldLayoutTemplate[M, Format]
/** A `MoldLayout` contains all information needed to create a [[net.noresttherein.sugar.format.Format.Mold Mold]]
  * for any [[net.noresttherein.sugar.format.Format Format]]. It is typically created using the exact same process
  * of monadic composition, but starting with
  * a [[net.noresttherein.sugar.format.Format.MoldLayoutMaker MoldLayoutMaker]] created by
  * [[net.noresttherein.sugar.format.Format$.apply Format]]`[M](name)`
  * rather than a [[net.noresttherein.sugar.format.Format!.apply format]]`[M](name)` for a `format :Format`.
  * See the documentation for [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]]
  * for more detailed information. Alternatively, the interface may be also implemented directly by a custom class.
  * If an implicit `MoldLayout[M]` is defined, it will be a source of implicit `Mold`[M] for all formats.
  */ //consider: we can move this out to the package level. This would cut
trait MoldLayout[M] extends Any with Serializable {
	/** Create a mold of the described model type for the specified format.
	  * The mold will use the format's parsing and formatting methods, by recursively casting/melting
	  * each property of this mold's model class.
	  * This typically involved starting with `format.`[[net.noresttherein.sugar.format.Format.open open]],
	  * followed by `format.`[[net.noresttherein.sugar.format.Format.propertyMold propertyMold]]
	  * for every property of `M`, and wrapping up with `format.`[[net.noresttherein.sugar.format.Format.close close]].
	  */
	def apply(format :Format) :format.Mold[M]

	/** A `MoldLayout` composing the created `Mold`s with the specified functions. */
	def map[O](read :M => O, write :O => M) :MoldLayout[O] = new MappedMoldLayout(this, read, write)

	/** A `MoldLayout` composing the created `Mold`s with the specified functions. */
	def map[O](name :String, read :M => O, write :O => M) :MoldLayout[O] =
		new NamedMappedMoldLayout(name, this, read, write)

	/** Provides a layout for this model type taking into account the possibility that the value may be missing
	  * - both in the parsed format and a property of a larger molded model.
	  * If parsing fails, regardless of the used [[net.noresttherein.sugar.format.Format.Mold Mold]] method,
	  * nothing is consumed and [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] is returned.
	  * Conversely, if `Inexistent` is melted by a created mold, nothing is appended to the output.
	  */
	def potential :MoldLayout[Potential[M]] =
		new MoldLayout[Potential[M]] {
			override def apply(format :Format) :format.Mold[Potential[M]] = MoldLayout.this(format).potential
			override def toString :String = MoldLayout.this.toString + ".potential"
		}
	/** Provides a layout for this model type taking into account the possibility that the value may be missing
	  * - both in the parsed format and a property of a larger molded model.
	  * If parsing fails, regardless of the used [[net.noresttherein.sugar.format.Format.Mold Mold]] method,
	  * nothing is consumed and [[net.noresttherein.sugar.vars.Opt.Lack Lack]] is returned.
	  * Conversely, if `Lack` is melted by a created mold, nothing is appended to the output.
	  */
	def opt :MoldLayout[Opt[M]] =
		new MoldLayout[Opt[M]] {
			override def apply(format :Format) :format.Mold[Opt[M]] = MoldLayout.this(format).opt
			override def toString :String = MoldLayout.this.toString + ".opt"
		}
	/** Provides a layout for this model type taking into account the possibility that the value may be missing
	  * - both in the parsed format and a property of a larger molded model.
	  * If parsing fails, regardless of the used [[net.noresttherein.sugar.format.Format.Mold Mold]] method,
	  * nothing is consumed and [[None]] is returned.
	  * Conversely, if `None` is melted by a created mold, nothing is appended to the output.
	  */
	def option :MoldLayout[Option[M]] =
		new MoldLayout[Option[M]] {
			override def apply(format :Format) :format.Mold[Option[M]] = MoldLayout.this(format).option
			override def toString :String = MoldLayout.this.toString + ".option"
		}
	/** Provides a layout for this model type reporting a missing or malformed model
	  * - both in the parsed format and a property of a larger molded model - to the application.
	  * A `Outcome[M]` instance is available in the layout, rather than being implicitly unwrapped.
	  * If parsing fails, regardless of the used [[net.noresttherein.sugar.format.Format.Mold Mold]] method,
	  * nothing is consumed and [[net.noresttherein.sugar.vars.Opt.Lack Lack]] is returned.
	  * Conversely, if `Lack` is melted by a created mold, nothing is appended to the output.
	  */
	def guard :MoldLayout[Outcome[M]] =
		new MoldLayout[Outcome[M]] {
			override def apply(format :Format) :format.Mold[Outcome[M]] = MoldLayout.this(format).guard
			override def toString :String = MoldLayout.this.toString + ".guard"
		}

	/** A mold layout which will first try parsing/formatting using this layout,
	  * and fallback to the argument in case it fails.
	  */
	def |(other :MoldLayout[M]) :MoldLayout[M] =
		new MoldLayout[M] {
			override def apply(format :Format) :format.Mold[M] = MoldLayout.this(format) | other(format)
			override def toString :String = MoldLayout.this.toString + "|" + other
		}
}

/** A `MoldLayout` which builds [[net.noresttherein.sugar.format.Format.NamedMold NamedMold]]s.
  * The [[net.noresttherein.sugar.format.Format.NamedMoldLayout.name name]] is used to better identify
  * a `Mold` in its `toString` implementation. While the property is not typically used directly by this layout
  * or created molds, [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]]s
  * and [[net.noresttherein.sugar.format.Format.MoldLayoutMaker MoldLayoutMaker]]s are most often initialized
  * with a name of the molded model type, which may be used in created molds' formats, and they return
  * instances of `NamedMoldLayout`/`NamedMold` initialized with the same name.
  */
trait NamedMoldLayout[M] extends MoldLayout[M] {
	/** A name used in `toString` implementation of both this layout and molds it creates,
	  * typically the name of the molded type. The mold name should omit information about the owning `Format`,
	  * and composed or derived mold should construct their names based on other molds' names,
	  * rather than implement `toString`, to avoid printing the format with every mold.
	  * It also allows to use the name inside `[]` brackets, again avoiding composing those brackets.
	  */
	def name :String

	override def apply(format :Format) :format.NamedMold[M]

	override def map[O](read :M => O, write :O => M) :NamedMoldLayout[O] =
		new NamedMappedMoldLayout(this, read, write)

	override def potential :NamedMoldLayout[Potential[M]] =
		new AbstractNamedMoldLayout[Potential[M]](name) {
			override def apply(format :Format) :format.NamedMold[Potential[M]] =
				NamedMoldLayout.this(format).potential
		}
	override def opt :NamedMoldLayout[Opt[M]] =
		new AbstractNamedMoldLayout[Opt[M]](name) {
			override def apply(format :Format) :format.NamedMold[Opt[M]] =
				NamedMoldLayout.this(format).opt
		}
	override def option :NamedMoldLayout[Option[M]] =
		new AbstractNamedMoldLayout[Option[M]](name) {
			override def apply(format :Format) :format.NamedMold[Option[M]] =
				NamedMoldLayout.this(format).option
		}
	override def guard :NamedMoldLayout[Outcome[M]] =
		new AbstractNamedMoldLayout[Outcome[M]](name) {
			override def apply(format :Format) :format.NamedMold[Outcome[M]] =
				NamedMoldLayout.this(format).guard
		}
	override def |(other :MoldLayout[M]) :MoldLayout[M] = other match {
		case named :NamedMoldLayout[M] => this | named
		case _ => super.|(other)
	}
	def |(other :NamedMoldLayout[M]) :NamedMoldLayout[M] =
		new NamedMoldLayout[M] {
			override def apply(format :Format) :format.NamedMold[M] = NamedMoldLayout.this(format) | other(format)
			override def name = NamedMoldLayout.this.name + "|" + other.name
		}

	override def toString :String = "MoldLayout[" + name + "]"
}


@SerialVersionUID(Ver)
object MoldLayout {
	/** Summons an implicit `MoldLayout[M]`. */
	@inline def apply[M](implicit mold :MoldLayout[M]) :MoldLayout[M] = mold

	/** Maps an implicit `MoldLayout[X]` to represent it as a `MoldLayout[Y]`. */
	def map[X, Y](map :X => Y, imap :Y => X)(implicit layout :MoldLayout[X]) :MoldLayout[Y] =
		layout.map(map, imap)

	/** A `Mold` mapping the given constant to an empty `liquid`. Always returns `value` when parsing
	  * without consuming any of the input, and always returns
	  * [[net.noresttherein.sugar.format.Format.Liquid.empty empty]] when formatting, ignoring its input.
	  */
	def empty[S](value :S) :MoldLayout[S] = new EmptyMoldLayout(value)

	@SerialVersionUID(Ver)
	private class EmptyMoldLayout[S](value :S) extends MoldLayout[S] {
		override def apply(format :Format) = format.Mold.empty(value)
		override def toString = "MoldLayout.empty(" + value + ")"
	}

	implicit val stringMoldLayout :MoldLayout[String] =
		new MoldLayout[String] {
			override def apply(format :Format) = format.stringMold
		}
	implicit val charMoldLayout   :MoldLayout[Char] =
		new MoldLayout[Char] {
			override def apply(format :Format) = format.charMold
		}
	implicit val longMoldLayout   :MoldLayout[Long]   =
		new MoldLayout[Long] {
			override def apply(format :Format) = format.longMold
		}

	implicit val intMoldLayout    :MoldLayout[Int] =
		new MoldLayout[Int] {
			override def apply(format :Format) = format.intMold
		}

	implicit val shortMoldLayout  :MoldLayout[Short] =
		new MoldLayout[Short] {
			override def apply(format :Format) = format.shortMold
		}

	implicit val byteMoldLayout   :MoldLayout[Byte] =
		new MoldLayout[Byte] {
			override def apply(format :Format) = format.byteMold
		}
	implicit val doubleMoldLayout :MoldLayout[Double] =
		new MoldLayout[Double] {
			override def apply(format :Format) = format.doubleMold
		}

	implicit val floatMoldLayout  :MoldLayout[Float] =
		new MoldLayout[Float] {
			override def apply(format :Format) = format.floatMold
		}

}




//These need to be outside MoldLayout to be seen by NamedMoldLayout
@SerialVersionUID(Ver)
private class MappedMoldLayout[X, Y](layout :MoldLayout[X], read :X => Y, write :Y => X) extends MoldLayout[Y] {
	override def apply(format :Format) = layout(format).map(read, write)
	override def toString = layout.toString + ".map"
}

@SerialVersionUID(Ver)
private class NamedMappedMoldLayout[X, Y](override val name :String, layout :MoldLayout[X],
                                          read :X => Y, write :Y => X)
	extends MappedMoldLayout[X, Y](layout, read, write) with NamedMoldLayout[Y]
{
	def this(layout :NamedMoldLayout[X], read :X => Y, write :Y => X) =
		this("map(" + layout.name + ")", layout, read, write)
	override def apply(format :Format) = layout(format).map(name, read, write)
}






/** Factory methods for creating [[net.noresttherein.sugar.format.MoldLayout MoldLayout]]s from scratch. */
trait MoldLayoutFactory[M] extends Serializable {
	//in Scala 3 we will get rid of it and instead have flatMap(mold :(parts :Parts[M]) => parts.format.Mold[M]))
	/** A mock `Format` instance standing for 'any `Format`'.
	  * It allows to define [[net.noresttherein.sugar.format.MoldLayout MoldLayout]]s
	  * using functions expressed in types defined in this (or any other) `Format`.
	  * It should ''never'' be used itself in any form other as a path element to its types;
	  * methods defined here accept functions which take as arguments those types,
	  * such as `fmt.`[[net.noresttherein.sugar.format.Format.Parts Parts]].
	  */
	val fmt :Format
	import fmt.Liquid

	/** The most generic `MoldLayout` factory method, using the given functions to implement
	  * [[net.noresttherein.sugar.format.Format.Mold Mold]]s parsing and formatting values
	  * of this maker's model type. The given functions are used directly to implement their eponymous methods.
	  * This method is fully useful only for molds performing such functions
	  * as checksum calculation and verification - most molds do not need to inspect already parsed/formatted data.
	  * @param advance A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.advance advance]] method.
	  *                It accepts the previously parsed data (back to the beginning of the entity
	  *                owning the molded model), the remaining input and parses a value of `M`, at the same time
	  *                moving the split past/future forward. The returned tuple contains the first argument
	  *                with a fragment parsed by the created mold appended, the cast model and a suffix
	  *                of the second argument with the parsed fragment representing the model dropped.
	  * @param append  A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.append append]] method.
	  *                It accepts the data already formatted (back to the beginning of the entity
	  *                owning the molded model), the value to format and returns the first argument
	  *                [[net.noresttherein.sugar.format.Format.concat concat]] concatenated
	  *                with the melted model `M`.
	  */
	def apply(advance :(Liquid, Liquid) => (Liquid, M, Liquid), append :(Liquid, M) => Liquid) :MoldLayout[M]

	/** The simplest factory method for a `MoldLayout`, accepting functions necessary to implement
	  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] parsing and formatting values of this maker's
	  * model type.
	  * @param split   A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.advance advance]] method.
	  *                It accepts the input data, assumed to start with a formatted value of `M`,
	  *                and casts the model object, splitting the input into past and future fragments.
	  *                The returned tuple contains a prefix of the argument representing the created mold,
	  *                the cast model and the argument with the parsed fragment representing the model dropped.
	  * @param melt    A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.melt melt]] method.
	  *                It accepts the model to melt and returns it in a formatted form.
	  */
	def apply(split :Liquid => (Liquid, M, Liquid), melt :M => Liquid) :MoldLayout[M]

	/** The most generic `MoldLayout` factory method, using the given functions for parsing and formatting.
	  * The functions are used directly to implement [[net.noresttherein.sugar.format.Format.Mold Mold]]'s
	  * eponymous methods. They must not throw any exceptions, and instead return
	  * an empty [[net.noresttherein.sugar.vars.Potential Potential]].
	  * This method is fully useful only for molds performing such functions
	  * as checksum calculation and verification - most molds do not need to inspect already parsed/formatted data.
	  * @param advance A function used in the implementation of the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.advanceOpt advanceOpt]] method.
	  *                It accepts the previously parsed data (back to the beginning of the entity
	  *                owning the molded model), the remaining input and parses a value of `M`, at the same time
	  *                moving the split past/future forward. The returned tuple contains the first argument
	  *                with a fragment parsed by the created mold appended, the cast model and a suffix
	  *                of the second argument with the parsed fragment representing the model dropped.
	  * @param append  A function used in the implementation of the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.appendOpt appendOpt]] method.
	  *                It accepts the data already formatted (back to the beginning of the entity
	  *                owning the molded model), the value to format and returns the first argument
	  *                [[net.noresttherein.sugar.format.Format.concat concat]] concatenated
	  *                with the melted model `M`.
	  */
	def opt(advance :(Liquid, Liquid) => Potential[(Liquid, M, Liquid)], append :(Liquid, M) => Potential[Liquid])
			:MoldLayout[M]

	/** The simplest factory method for a `MoldLayout`, accepting functions necessary to implement
	  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] parsing and formatting values of this maker's
	  * model type. They must not throw any exceptions, and instead return
	  * an empty [[net.noresttherein.sugar.vars.Potential Potential]].
	  * @param split   A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.advanceOpt advanceOpt]] method.
	  *                It accepts the input data, assumed to start with a formatted value of `M`,
	  *                and casts the model object, splitting the input into past and future fragments.
	  *                The returned tuple contains a prefix of the argument representing the created mold,
	  *                the cast model and the argument with the parsed fragment representing the model dropped.
	  * @param melt    A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.meltOpt meltOpt]] method.
	  *                It accepts the model to melt and returns it in a formatted form.
	  */
	def opt(split :Liquid => Potential[(Liquid, M, Liquid)], melt :M => Potential[Liquid]) :MoldLayout[M]

	/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
	  * The functions are used directly to implement [[net.noresttherein.sugar.format.Format.Mold Mold]]'s
	  * eponymous methods. They must not throw any exceptions, and instead return
	  * a [[net.noresttherein.sugar.vars.Outcome.Failed Failed]] instance.
	  * This method is fully useful only for molds performing such functions
	  * as checksum calculation and verification - most molds do not need to inspect already parsed/formatted data.
	  * @param advance A function used in the implementation of the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.guardAdvance guardAdvance]] method.
	  *                It accepts the previously parsed data (back to the beginning of the entity
	  *                owning the molded model), the remaining input and parses a value of `M`, at the same time
	  *                moving the split past/future forward. The returned tuple contains the first argument
	  *                with a fragment parsed by the created mold appended, the cast model and a suffix
	  *                of the second argument with the parsed fragment representing the model dropped.
	  * @param append  A function used in the implementation of the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.guardAppend guardAppend]] method.
	  *                It accepts the data already formatted (back to the beginning of the entity
	  *                owning the molded model), the value to format and returns the first argument
	  *                [[net.noresttherein.sugar.format.Format.concat concat]] concatenated
	  *                with the melted model `M`.
	  */
	def guard(advance :(Liquid, Liquid) => Outcome[(Liquid, M, Liquid)], append :(Liquid, M) => Outcome[Liquid])
			:MoldLayout[M]

	/** The simplest factory method for a `Mold`,accepting functions necessary to implement
	  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] parsing and formatting values of this maker's
	  * model type. They must not throw any exceptions, and instead return
	  * a [[net.noresttherein.sugar.vars.Outcome.Failed Failed]] instance.
	  * @param split   A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.guardAdvance guardAdvance]] method.
	  *                It accepts the input data, assumed to start with a formatted value of `M`,
	  *                and casts the model object, splitting the input into past and future fragments.
	  *                The returned tuple contains a prefix of the argument representing the created mold,
	  *                the cast model and the argument with the parsed fragment representing the model dropped.
	  * @param melt    A function used to implement the mold's
	  *                [[net.noresttherein.sugar.format.Format.Mold.guardMelt guardMelt]] method.
	  *                It accepts the model to melt and returns it in a formatted form.
	  */
	def guard(split :Liquid => Outcome[(Liquid, M, Liquid)], melt :M => Outcome[Liquid]) :MoldLayout[M]
}




private abstract class AbstractNamedMoldLayout[S](override val name :String) extends NamedMoldLayout[S]

private abstract class NamedMoldLayoutFactory[M](name :String) extends MoldLayoutFactory[M] {
	import fmt.Liquid

	override def apply(advance :(Liquid, Liquid) => (Liquid, M, Liquid), append :(Liquid, M) => Liquid) =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) :format.NamedMold[M] =
				format.asInstanceOf[fmt.type].Mold(this.name, advance, append).asInstanceOf[format.NamedMold[M]]
		}
	override def apply(split :fmt.Liquid => (fmt.Liquid, M, fmt.Liquid), melt :M => fmt.Liquid) :MoldLayout[M] =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) :format.NamedMold[M] =
				format.asInstanceOf[fmt.type].Mold(this.name, split, melt).asInstanceOf[format.NamedMold[M]]
		}

	override def opt(advance :(Liquid, Liquid) => Potential[(Liquid, M, Liquid)],
	                 append :(Liquid, M) => Potential[Liquid]) =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) =
				format.asInstanceOf[fmt.type].Mold.opt(this.name, advance, append).asInstanceOf[format.NamedMold[M]]
		}
	override def opt(split :fmt.Liquid => Potential[(fmt.Liquid, M, fmt.Liquid)], melt :M => Potential[fmt.Liquid])
			:MoldLayout[M] =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) =
				format.asInstanceOf[fmt.type].Mold.opt(this.name, split, melt).asInstanceOf[format.NamedMold[M]]
		}

	override def guard(advance :(Liquid, Liquid) => Outcome[(Liquid, M, Liquid)],
	                   append :(Liquid, M) => Outcome[Liquid]) =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) =
				format.asInstanceOf[fmt.type].Mold.guard(this.name, advance, append).asInstanceOf[format.NamedMold[M]]
		}
	override def guard(split :fmt.Liquid => Outcome[(fmt.Liquid, M, fmt.Liquid)], melt :M => Outcome[fmt.Liquid])
			:MoldLayout[M] =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) =
				format.asInstanceOf[fmt.type].Mold.guard(this.name, split, melt).asInstanceOf[format.NamedMold[M]]
		}

	override def toString = "MoldLayoutFactory[" + name + "]"
}
