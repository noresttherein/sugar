package net.noresttherein.sugar.format

import net.noresttherein.sugar.format.extensions.{ParsingFormattingExtensions, LiquidExtension}




private[format] trait extensionsLowPriority extends Any {
	/** Extension method for [[net.noresttherein.sugar.format.Format.Liquid Liquid]] data
	  * in the implicit [[net.noresttherein.sugar.format.Format Format]].
	  */ //lower priority in case Liquid and Raw are the same type.
	implicit def liquidFormatExtension[L](liquid :L)(implicit format :Format { type Liquid = L })
			:LiquidExtension[L, format.Mold] =
		new LiquidExtension(liquid)
}


trait extensions extends Any with extensionsLowPriority {
	/** Extension methods allowing to either parse this value as a specified format
	  * if it is its [[net.noresttherein.sugar.format.Format.Raw Raw]] type or, conversely,
	  * format it in the specified format if an implicit [[net.noresttherein.sugar.format.Format.Mold Mold]]
	  * exists for that format.
	  */
	implicit def parsingFormattingExtensions[T](self :T) = new ParsingFormattingExtensions(self)

	/** Extension method for [[net.noresttherein.sugar.format.Format.Raw Raw]] data
	  *  in the implicit [[net.noresttherein.sugar.format.Format Format]].
	  */
	implicit def rawFormatExtension[R](raw :R)(implicit format :Format { type Raw = R })
			:LiquidExtension[format.Liquid, format.Mold] =
		new LiquidExtension(format.melt(raw))
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	/** Extension parsing and formatting methods for parsable types serving as some
	  * [[net.noresttherein.sugar.format.Format format]]'s [[net.noresttherein.sugar.format.Format.Raw Raw]] type
	  * or for formattable types for which an implicit [[net.noresttherein.sugar.format.Format.Mold Mold]]
	  * of a specified format exists.
	  */
	class ParsingFormattingExtensions[A] private[extensions] (private val self :A) extends AnyVal {
		/** Formats this object in the given format, returning its raw representation. */
		@throws[FormattingException]("if this value is not a model matching the implicit mold.")
		@inline def as(format :Format)(implicit mold :format.Mold[A]) :format.Raw = format.write(self)

		/** Formats this object in the given format, returning its raw representation.
		  * Same as [[net.noresttherein.sugar.format.extensions.ParsingFormattingExtensions.as as]],
		  * in case the latter conflicts with other methods/extension methods.
		  */
		@throws[FormattingException]("if this value is not a model matching the implicit mold.")
		@inline def melt(format :Format)(implicit mold :format.Mold[A]) :format.Raw = format.write(self)

		/** Parses the [[net.noresttherein.sugar.format.Format.Raw raw]] data in the format specified as the argument
		  * as the argument's type parameter. The argument `maker` itself is irrelevant: it serves only
		  * to establish both the [[net.noresttherein.sugar.format.Format Format]] and the target model type.
		  * Given a `Format` instance `format`, passing a `format[S]` as the argument will fix the format
		  * and the return type.
		  * @param maker an unused `Mold` builder serving to specify both the underlying format and the format
		  *              of models `S`, for example `XML[Dragon]`
		  */
		@throws[ParsingException]("if this value is not in a valid specified format for the given mold.")
		@inline def as[S](maker :(Format { type Raw = A })#Moldmaker[S])(implicit mold :maker.format.Mold[S]) :S =
			mold.cast(maker.format.melt(self))

		/** Parses the [[net.noresttherein.sugar.format.Format.Raw raw]] data in the format specified as the argument
		  * as the argument's type parameter. The argument `maker` itself is irrelevant: it serves only
		  * to establish both the [[net.noresttherein.sugar.format.Format Format]] and the target model type.
		  * Given a `Format` instance `format`, passing a `format[S]` as the argument will fix the format
		  * and the return type.
		  * The name of the method comes from casting an object with a mold, not forcing conformance to another type.
		  * It is equivalent to [[net.noresttherein.sugar.format.extensions.ParsingFormattingExtensions.as as]],
		  * but less likely to cause name conflicts with existing methods or extension methods for this type.
		  * @param maker an unused `Mold` builder serving to specify both the underlying format and the format
		  *              of models `S`, for example `XML[Dragon]`
		  */
		@throws[ParsingException]("if this value is not in a valid specified format for the given mold.")
		@inline def cast[S](maker :(Format { type Raw = A })#Moldmaker[S])(implicit mold :maker.format.Mold[S]) :S =
			mold.cast(maker.format.melt(self))

		/** Reads a value of type specified by the type parameter from raw data `A` of the given format. */
		@throws[ParsingException]("if this value is not in a valid specified format for the given mold.")
		@inline def castAs[S](format :Format { type Raw = A })(implicit mold :format.Mold[S]) :S =
			mold.cast(format.melt(self))
	}

	/** Extension methods for [[net.noresttherein.sugar.format.Format.Liquid Liquid]] data allowing to parse it
	  * as any value for which a [[net.noresttherein.sugar.format.Format.Mold Mold]] exists.
	  * Both types require existence of a single implicit [[net.noresttherein.sugar.format.Format Format]].
	  */ //todo: it should be possible to include it with the others
	class LiquidExtension[L, M[S] <: (Format { type Liquid = L })#Mold[S]] private[format]
	                              (private val liquid :L)
		extends AnyVal
	{
		/** Parses a value of type `S` specified as the type parameter,
		  * providing a [[net.noresttherein.sugar.format.Format.Mold Mold]] exists for it.
		  * This method is only available if an implicit [[net.noresttherein.sugar.format.Format Format]]
		  * for that mold exists, and this value is of its [[net.noresttherein.sugar.format.Format.Raw Raw]] type.
		  * The name of the method comes from casting an object with a mold, not forcing conformance to another type.
		  */
		@throws[ParsingException]("if this value is not in a valid specified format for the given mold.")
		@inline def read[S](implicit mold :M[S]) :S = mold.cast(liquid)

		/** Parses a value of type `S` specified as the type parameter,
		  * providing a [[net.noresttherein.sugar.format.Format.Mold Mold]] exists for it.
		  * This method is only available if an implicit [[net.noresttherein.sugar.format.Format Format]]
		  * for that mold exists, and this value is of its [[net.noresttherein.sugar.format.Format.Raw Raw]] type.
		  * The name of the method comes from casting an object with a mold, not forcing conformance to another type.
		  * @return a pair consisting of the parsed element and unparsed suffix.
		  */
		@throws[ParsingException]("if this value is not in a valid specified format for the given mold.")
		@inline def next[S](implicit mold :M[S]) :(S, L) = mold.next(liquid)
	}
}
