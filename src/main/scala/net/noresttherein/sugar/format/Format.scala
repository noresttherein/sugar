package net.noresttherein.sugar.format


import scala.annotation.{implicitAmbiguous, implicitNotFound}
import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.format.Format.{MoldingProcess, MoldLayout, MoldLayoutFactory, NamedMoldLayout}
import net.noresttherein.sugar.format.Format.MoldingProcess.{Casting, Melting}
import net.noresttherein.sugar.vars.{Fallible, Opt, Potential}
import net.noresttherein.sugar.vars.Fallible.{Failed, Passed}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Potential.{Existent, Inexistent}
import net.noresttherein.sugar.witness.Maybe

//implicits
import net.noresttherein.sugar.extensions.{classNameExtension, classNameMethods}




/** Provides second-choice implicit [[net.noresttherein.sugar.format.Format.Mold Mold]]s created
  * based on format agnostic [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s.
  */
private[format] sealed trait FormatLayoutMolds { this :Format =>
	@inline implicit def moldOf[S](implicit factory :MoldLayout[S]) :Mold[S] = factory(this)
}

/** Format's [[net.noresttherein.sugar.format.Format.Raw Raw]] and [[net.noresttherein.sugar.format.Format.Liquid Liquid]]
  * types may be equal, or equal to `String`, hence the need to introduce priority of implicits between
  * `Mold[Raw]`, `Mold[Liquid]` and any other concrete type that a `Format` may define itself.
  * `Mold[Raw]` is less desirable than `Mold[Liquid]` because it involves conversions to `Liquid`.
  */
private[format] sealed trait FormatRawMoldImplicit extends FormatLayoutMolds { this :Format =>
	/** A `Mold` embedding directly given `Raw` values in the formatted result.
	  * When parsing, all of the input is automatically consumed, regardless of its contents.
	  */
	implicit val rawMold :Mold[Raw] = rawMoldPrototype
}

/** Declaration of an implicit
  * [[net.noresttherein.sugar.format.Format.Mold Mold]]`[`[[net.noresttherein.sugar.format.Format.Liquid Liquid]]`]`,
  * extracted to have a lower priority then `Mold[String]` and any other molds declared by `Format` subclasses
  * which may be the same type as `Liquid` in some implementations.
  */
private[format] sealed trait FormatLiquidMoldImplicit extends FormatRawMoldImplicit { this :Format =>
	/** A `Mold` embedding directly given `Liquid` values in the formatted result.
	  * When parsing, all of the input is automatically consumed, regardless of its contents.
	  */
	implicit val liquidMold :Mold[Liquid] = liquidMoldPrototype
}


/** A representation of a data format, or a language used to define such formats.
  * The formats of actual concrete Scala objects in this language/format are defined by this instance's
  * [[net.noresttherein.sugar.format.Format.Mold Mold]] type classes.
  * Each format instance recognizes three types:
  *   1. [[net.noresttherein.sugar.format.Format.Raw Raw]] - the formatted raw data serving as input and output,
  *      such as `String` or `Array[Byte]`;
  *   1. [[net.noresttherein.sugar.format.Format.Liquid Liquid]] - an intermediate buffer type used when formatting
  *      and parsing, such as [[net.noresttherein.sugar.collections.StringLike StringLike]] or [[StringBuilder]];
  *   1. 'Solid' types - Scala types of formatted entities from the application domain model.
  *
  * The conversion process goes `Raw => Liquid => Solid` and `Solid => Liquid => Raw`.
  *
  *
  * '''Converting between Scala objects and formatted values'''
  *
  * There are several ways in which input can be parsed or formatted in a given `Format`;
  * the following examples assume `val XML :Format` and an implicit `XML.Mold[Dragon]`:
  *   1. Methods defined directly in `Format`:
  *      {{{
  *          case class Dragon(name :String, color :String, level :Int)
  *
  *          val xml = "<Dragon><name>Firkraag</name><color>Red</color><level>23</level></Dragon>"
  *
  *          val dragon :Dragon = XML.read(xml) //type argument Dragon inferred from the expected type
  *          val fmt = XML.write(dragon)
  *      }}}
  *   1. Extension methods available in [[net.noresttherein.sugar.extensions sugar.extensions]]
  *      and [[net.noresttherein.sugar.format.extensions format.extension]] (equivalent, `sugar` just includes
  *      also extensions for classes from other packages):
  *      {{{
  *          //Available also under name `cast`, in case `as` causes conflicts.
  *          xml as XML[Dragon]     //:Dragon
  *
  *          xml castAs[Dragon] XML //:Dragon
  *
  *          //Available also under name `melt`, in  case `as` causes conflicts.
  *          Dragon("Firkraag", "Red", 23) as XML //:String
  *
  *          //Assumes a single implicit Format and an implicit Mold[Dragon] for that format.
  *          xml.read[Dragon]       //:Dragon
  *      }}}
  *   1. Readers and writers defined in every `Format` instance can be used for ad-hoc parsing of values
  *      for which `Mold`s are unavailable:
  *      {{{
  *          val result = for { //result :Fallible[Dragon]
  *              reader <- XML.reader(xml)
  *              _      <- reader.expect("Dragon")(XML.open) //fails if the input doesn't start with "<Dragon>".
  *              name   <- reader.property[String]("name")   //reads <name>$name</name>
  *              color  <- reader.property[String]("color")
  *              level  <- reader.property[Int]("level")
  *              _      <- reader.expect("Dragon")(XML.close)
  *          } yield Dragon(name, color, level)
  *      }}}
  *      The API is common for all formats, so the above example would also work for JSON, for example.
  *      {{{
  *          import XML.{writer, tagMold}
  *          val dragonXML = writer + "<Dragon"> +
  *             tagMold[String]("name").melt("Firkraag") +
  *             tagMold[String]("color").melt("Red") +
  *             tagMold[Int]("level").melt(23) +
  *          "</Dragon>
  *
  *          val firkraag = Dragon("Firkraag", "Red", 23)
  *          val saladrex = Dragon("Saladrex", "Red", 25)
  *          val thaxll   = Dragon("Thaxll'sillyia", "Black", 20)
  *          JSON.writer + "[" + firkraag + ", " + saladrex + ", " + thaxll + "]" //:String
  *          JSON.make("[", ",", "]") + firkraag + saladrex + thaxll              //same as above
  *
  *          //same result as above, with a mutable builder
  *          JSON.newBuilder += "[" += firkraag += ", " += saladrex += ", " + thaxll += "]" to JSON.Raw
  *          JSON.newBuilder("[", ",", "]") += firkraag += saladrex += thaxll to JSON.Raw
  *      }}}
  *      While in example above many of the written objects were strings for simplicity, every one of them
  *      could be replaced with another type `T` for which an implicit `Mold[T]` exists.
  *
  *
  * '''Defining class formats'''
  *
  * The `Format` trait itself contains declarations of molds for common built in types which are available
  * even in a generic context where the concrete type of a `Format` is unknown.
  * Some subclasses add to those definitions more specific instances. Nevertheless, the application needs
  * to define a `Mold` for every type it wishes to parse/format in the above ways.
  * This can be done in a variety of ways:
  *
  *   1. By implementing one of `Mold` subtraits designed explicitly as base classes, such as
  *      [[net.noresttherein.sugar.format.Format.SimpleThrowingMold SimpleThrowingMold]],
  *      [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold SimpleOptBasedMold]],
  *      [[net.noresttherein.sugar.format.Format.SimpleGuardingMold SimpleGuardingMold]]
  *      (good for molds which do not use other molds) and
  *      [[net.noresttherein.sugar.format.Format.SpecialThrowingMold SpecialThrowingMold]],
  *      [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold SpecialOptBasedMold]],
  *      [[net.noresttherein.sugar.format.Format.SimpleGuardingMold SpecialGuardingMold]].
  *      which are based on more generic methods needed to properly use unknown `Mold` implementations.
  *      The difference lies in the primary way of error reporting and more generic/simpler methods left abstract.
  *      See [[net.noresttherein.sugar.format.Format.Mold Mold]] documentation for more details.
  *   1. By factory methods in the companion [[net.noresttherein.sugar.format.Format.Mold$ Mold]] object,
  *      accepting parsing and formatting functions.
  *   1. By mapping an existing mold, either with its [[net.noresttherein.sugar.format.Format.Mold!.map map]] method,
  *      or [[net.noresttherein.sugar.format.Format.Mold$.map the one in its companion object]].
  *   1. Through a monadic-style composition in a ''for comprehension'' (code assumes existence of implicit molds
  *      `JSON.Mold[Int]` and `JSON.Mold[String]`):
  *      {{{
  *          object Dragon {
  *              implicit val DragonAsJSON = for {
  *                  dragon <- JSON[Dragon]
  *                  name   <- dragon("name")(_.name)
  *                  color  <- dragon("color")(_.color)
  *                  level  <- dragon("level")(_.level)
  *              } yield Dragon(name, color, level)
  *          }
  *      }}}
  *      The above code creates specifically an instance of `JSON.Mold[Dragon]`.
  *   1. By providing a generic [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]`[M]`, which serves
  *      as a `Mold[M]` factory. These can, similarly to molds themselves, be either implemented directly,
  *      by mapping another `MoldLayout`, or by a close equivalent of the monadic composition defined above.
  *      The difference is only that instead of starting with `apply[Dragon]` method of a `Format` instance
  *      as above, but [[net.noresttherein.sugar.format.Format$.apply one]] defined in
  *      [[net.noresttherein.sugar.format.Format$ Format]] object itself.
  *      {{{
  *          implicit val DragonFormat = for {
  *              dragon <- JSON[Dragon]
  *              name   <- dragon("name")(_.name)
  *              color  <- dragon("color")(_.color)
  *              level  <- dragon("level")(_.level)
  *          } yield Dragon(name, color, level)
  *      }}}
  *      The above definition can be made once and will work for all formats: existence of an implicit `MoldLayout[M]`
  *      implies the existence of an implicit `format.Mold[M]` for all `format :Format`.
  *
  * @author Marcin Mościcki
  */
trait Format extends FormatLiquidMoldImplicit with Serializable {
	/** The type to which Scala objects are formatted. This is usually the exact form and type in which they are stored
	  * or sent/received.
	  * @see [[net.noresttherein.sugar.format.Format.Liquid]]
	  */
	type Raw

	/** The base class of the companion object value to type [[net.noresttherein.sugar.format.Format.Raw! Raw]]. */
	@SerialVersionUID(ver)
	class RawFactory extends Serializable {
		@inline final def apply(liquid :Liquid) :Raw = cool(liquid)
		@inline final def empty :Raw = emptyRaw
	}
	/** Factory of [[net.noresttherein.sugar.format.Format.Raw! Raw]] format instances. */
	val Raw = new RawFactory

	/** An intermediate type used during formatting and parsing, not specific to any particular Scala class.
	  * It is the type that this format's [[net.noresttherein.sugar.format.Format.Mold Mold]]s work on;
	  * easily convertible to and from the [[net.noresttherein.sugar.format.Format.Raw raw]] format type,
	  * it is intended to provide faster 'concatenation' and 'prefix'/'suffix' operations (in the meaning specific
	  * to `this.`[[net.noresttherein.sugar.format.Format.Raw Raw]]).
	  * For example, creating a JSON `String` may require creation of many strings for its children elements,
	  * and their concatenation would severely impact performance. Similarly, parsing a `String` requires
	  * tracking of the current position, and [[String.substring]] copies all contents, again leading
	  * to `O(n^2)` complexity instead of `O(n)`. This can be solved by using a type such as
	  * [[net.noresttherein.sugar.collections.StringLike StringLike]], which acts as a buffer,
	  * offering `O(1)` concatenation. On the other hand, its [[net.noresttherein.sugar.collections.Substring Substring]]
	  * subtype offers `O(1)` [[net.noresttherein.sugar.collections.Substring.substring substring]] operation,
	  * greatly benefiting parsing. If a format allows for references to other elements within formatted data
	  * (which should be resolved to the same object), then it is the responsibility of this type to provide
	  * methods allowing such access. This conversion is expected to be fast and only prepare the necessary structures,
	  * rather then validate the input data.
	  * @see [[net.noresttherein.sugar.format.Format.Raw]]
	  * @see [[net.noresttherein.sugar.format.Format.melt]]
	  * @see [[net.noresttherein.sugar.format.Format.cool]]
	  */
	type Liquid

	/** The base class of the companion object value to type [[net.noresttherein.sugar.format.Format.Liquid! Liquid]]. */
	@SerialVersionUID(ver)
	class LiquidFactory extends Serializable {
		@inline final def apply(raw :Raw) :Liquid = melt(raw)
		@inline final def empty :Liquid = Format.this.emptyLiquid
	}
	/** Factory of [[net.noresttherein.sugar.format.Format.Liquid! Liquid]] format instances. */
	val Liquid = new LiquidFactory

//	@SerialVersionUID(ver)
//	class SolidFactory extends Serializable { //we could maybe
//		@inline final def apply[S :Mold](raw :Raw) :S = Mold[S].apply(melt(raw))
//	}
//	/** An object performing casting with an implicit `Mold`. */
//	val Solid = new SolidFactory



	/** Starts a process of ad-hoc reading of arbitrary values from sources
	  * in the [[net.noresttherein.sugar.format.Format.Raw raw]] format using monadic composition.
	  * The returned object is a factory of [[net.noresttherein.sugar.format.Format.ReadNext reader monads]]
	  * based on implicit [[net.noresttherein.sugar.format.Format.Mold molds]], but also provides a
	  * [[net.noresttherein.sugar.format.Format.RawReader.flatMap flatMap]]`(f :RawReader => `[[net.noresttherein.sugar.format.Format.ReadNext ReadNext]]`)`,
	  * so it can be used as the first generator in a ''for comprehension'':
	  * {{{
	  *     val adventurer = for {
	  *         read   <- format.reader(raw)
	  *         name   <- read[String]
	  *         race   <- read[String]
	  *         gender <- read[Char]
	  *         level  <- read[Int]
	  *         weapon <- read[String]
	  *     } yield Adventurer(name, race, gender, level, weapon)
	  * }}}
	  */
	def reader(raw :Raw) :RawReader = guardMelt(raw) match {
		case Passed(liquid) => new DefaultRawReader(liquid)
		case fail :Failed   => new FailingReader(fail)
	}

	/** Starts a process of ad-hoc reading of arbitrary values from sources
	  * in the [[net.noresttherein.sugar.format.Format.Raw raw]] format using monadic composition.
	  * The returned object is a factory of [[net.noresttherein.sugar.format.Format.ReadNext reader monads]]
	  * based on implicit [[net.noresttherein.sugar.format.Format.Mold molds]], but also provides a
	  * [[net.noresttherein.sugar.format.Format.RawReader.flatMap flatMap]]`(f :RawReader => `[[net.noresttherein.sugar.format.Format.ReadNext ReadNext]]`)`,
	  * so it can be used as the first generator in a ''for comprehension''.
	  * The arguments `prefix`, `separator` and `suffix` are constants expected at the beginning of the input,
	  * between every read object pair, and after all values have been read, before returning the result
	  * from the reader's [[net.noresttherein.sugar.format.Format.RawReader.flatMap flatMap]].
	  * They are compared as Scala objects rather than pre-melted liquids to allow for possibly multiple
	  * representations (for example if their [[net.noresttherein.sugar.format.Format.Mold Mold]] ignores whitespace
	  * in the input). If any of these three values is [[net.noresttherein.sugar.format.Format.isEmpty empty]]
	  * after melting to a [[net.noresttherein.sugar.format.Format.Liquid! Liquid]], it is completely ignored.
	  * On the other hand, if melting fails, the whole subsequent process will fail with this error.
	  */
	def reader[Pre :Mold, Sep :Mold, Suf :Mold](prefix :Pre, separator :Sep, suffix :Suf)(raw :Raw) :RawReader = {
		(for {
			pre <- Mold[Pre].guardMelt(prefix)
			sep <- Mold[Sep].guardMelt(separator)
			suf <- Mold[Suf].guardMelt(suffix)
			liq <- guardMelt(raw)
		} yield
			new SeparatedReader(liq,
				Opt.unless(isEmpty(pre))(prefix),
				Opt.unless(isEmpty(sep))(separator),
				Opt.unless(isEmpty(suf))(suffix)
			)
		) match {
			case Passed(reader) => reader
			case fail :Failed => new FailingReader(fail)
		}
	}

	/** A 'flat mappable' factory of [[net.noresttherein.sugar.format.Format.ReadNext reader monads]]
	  * allowing to parse an arbitrary sequence of objects from a value of this `Format`'s
	  * [[net.noresttherein.sugar.format.Format.Raw Raw]] type in order to produce a result.
	  * It is intended to be used as the first element in a ''for comprehension''.
	  * @see [[net.noresttherein.sugar.format.Format.ReadNext]]
	  * @see [[net.noresttherein.sugar.format.Format.reader]]
	  */
	trait RawReader {
		/** A reader monad parsing a value of type `S` using the implicitly given mold. */
		def apply[M](implicit mold :Mold[M]) :ReadNext[M] = mold.guardNext(_)

		/** A reader monad which tries to parse the prefix of the remaining input as a value of `S`, not consuming it. */
		def peek[M](implicit mold :Mold[M]) :ReadNext[Opt[M]] = liquid => Passed((mold.headOpt(liquid), liquid))

		/** A reader monad which attempts to read a value of `M` and compare it with `constant`.
		  * If they are unequal, the whole process fails.
		  */
		def expect[M](constant :M)(implicit mold :Mold[M]) :ReadNext[M] =
			liquid => mold.guardAdvance(constant)(emptyLiquid, liquid) map { case (_, m, rem) => (m, rem) }

		/** A reader monad parsing a single model's property, as defined by this format.
		  * This involves validating that input actually starts with a property named `name`.
		  * For example, in a JSON format, the reader would first expect to see `"{ '$property':"`,
		  * after which it would read a value using the implicitly passed `Mold[M]`, and finally
		  * it would expect to see "}" (possibly skipping ',' characters before and after the property format).
		  * If either of the fixed prefix or suffix doesn't match, the reader returns
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]].
		  */
		def property[M](name :String)(implicit mold :Mold[M]) :ReadNext[M] = apply(propertyMold[M](name))

		/** A monad returning always [[net.noresttherein.sugar.vars.Fallible.Failed Failed]].
		  * If it is present in the chain of flatMap calls, the whole procedure will fail with this result. */
		def fail(msg: => String) :ReadNext[Nothing] = liquid => Failed(() => msg)

		/** A monad returning always [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] with a default message. */
		def fail :ReadNext[Nothing] = liquid => Failed(() => "Failed to read from '" + liquid + "'.")

		/** Starts the monadic composition of a final read value, converting
		  * the [[net.noresttherein.sugar.format.Format.Liquid Liquid]] returned
		  * by the `ReadNext` returned by the function argument
		  * into [[net.noresttherein.sugar.format.Format.Raw Raw]] format. */
		def flatMap[M](f :RawReader => ReadNext[M]) :Fallible[M]
	}

	/** A reader monad parsing a value of type `S` from this `Format`'s
	  * [[net.noresttherein.sugar.format.Format.Liquid Liquid]] type. Can be chained with other instances
	  * in a ''for comprehension''. Instances are normally created
	  * using a [[net.noresttherein.sugar.format.Format.RawReader RawReader]] returned by this `Format`'s
	  * [[net.noresttherein.sugar.format.Format.reader reader]] method.
	  * @tparam M the type of the value read by this monad (and the argument type of functions accepted
	  *           by its [[net.noresttherein.sugar.format.Format.ReadNext.flatMap flatMap]]).
	  * @see [[net.noresttherein.sugar.format.Format.RawReader]]
	  */
	trait ReadNext[+M] {
		/** Reads a value from the input, returning it together with an unparsed suffix of the input.
		  * This is the only abstract method in this interface.
		  */
		def apply(input :Liquid) :Fallible[(M, Liquid)]

		/** Combines this reader with another reader in a logical disjunction.
		  * The new reader will first attempt to parse input with this instance and, if this fails,
		  * with the argument.
		  */
		def ||[U >: M](other :ReadNext[U]) :ReadNext[U] = input => apply(input) orElse other(input)

		/** A new reader verifying that attempts to read the first object with
		  * the [[net.noresttherein.sugar.format.Format.Mold Mold]] passed as a context bound and verifies
		  * that it equals the value given here; mismatch results
		  * in a [[net.noresttherein.sugar.vars.Fallible.Failed failure]] and further processing is aborted.
		  * Otherwise it proceeds to invoke this reader (after dropping from the input a prefix corresponding
		  * to the read value).
		  */
		def prefixed[O :Mold](expect :O) :ReadNext[M] = liquid =>
			Mold[O].guardAdvance(expect)(emptyLiquid, liquid) flatMap { case (_, _, rem) => apply(rem) }

		def map[O](f :M => O) :ReadNext[O] =
			liquid => apply(liquid) flatMap { case (s, suffix) => Passed((f(s), suffix)) }

		def flatMap[O](f :M => ReadNext[O]) :ReadNext[O] =
			liquid => apply(liquid) flatMap { case (s, suffix) => f(s)(suffix) }

		def filter(p :M => Boolean) :ReadNext[M] = {
			liquid => apply(liquid) flatMap { result =>
				if (p(result._1)) Passed(result)
				else Failed(() => "Failed to parse '" + liquid + "' as " + Format.this + ".")
			}
		}
	}

	private class DefaultRawReader[M](input :Liquid) extends RawReader {
		override def flatMap[O](f :RawReader => ReadNext[O]) :Fallible[O] = f(this)(input).map(_._1)
	}

	private class FailingReader(result :Failed) extends RawReader {
		override def flatMap[S](f :RawReader => ReadNext[S]) = result
	}

	private class SeparatedReader[Pre :Mold, Sep :Mold, Suf :Mold]
	                             (input :Liquid, prefix :Opt[Pre], separator :Opt[Sep], suffix :Opt[Suf])
		extends RawReader
	{
		//expects the suffix after reading when mapping, and the separator before next reader when flat mapping
		override def apply[S](implicit mold :Mold[S]) :ReadNext[S] =
			new SeparatedReadNext[S, Sep, Suf](separator) { unfiltered =>
				override def apply(input :Liquid) = Mold[S].guardNext(input)

				override def filter(p :S => Boolean) :ReadNext[S] =
					new SeparatedReadNext[S, Sep, Suf](separator) {
						override def apply(input :Liquid) =
							unfiltered(input) flatMap { result =>
								if (p(result._1)) Passed(result)
								else Failed(() => "Failed to parse '" + input + "' as " + Format.this + ".")
							}
						override def filter(p1 :S => Boolean) :ReadNext[S] =
							unfiltered.filter(s => p(s) && p1(s))
					}
			}
		//we verify the prefix before proceeding with the ReadNext returned by f
		override def flatMap[O](f :RawReader => ReadNext[O]) :Fallible[O] = {
			val withoutSuffix = prefix match {
				case Got(expect) =>
					Mold[Pre].guardDrop(expect)(input) flatMap (f(this)(_))
				case _ =>
					f(this)(input)
			}
			suffix match { //we could also implement suffix handling in ReadNext.map
				case Got(expect) =>
					withoutSuffix map {
						case (result, remainder) => Mold[Suf].guardDrop(expect)(remainder); result
					}
				case _ => withoutSuffix map (_._1)
			}
		}
	}

	private abstract class SeparatedReadNext[M :Mold, Sep :Mold, Suf :Mold](separator :Opt[Sep]) extends ReadNext[M] {
		override def flatMap[O](f :M => ReadNext[O]) :ReadNext[O] = separator match {
			case Got(expect) =>
				liquid =>
					for {
						mine       <- apply(liquid)
						(o, afterO) = mine
						afterSep   <- Mold[Sep].guardDrop(expect)(afterO)
						remainder  <- f(o)(afterSep)
					} yield remainder
			case _ =>
				super.flatMap(f)
		}
	}




	/** An ad-hoc writer of arbitrary values creating [[net.noresttherein.sugar.format.Format.Raw Raw]] (formatted)
	  * output using this format's implicit [[net.noresttherein.sugar.format.Format.Mold molds]].
	  * objects can be appended using its [[net.noresttherein.sugar.format.Format.Mold]]:
	  * {{{
	  *     JSON.writer + "[" + mage + "," + tank + "," + healer + "," + dps + "] to Raw
	  * }}}
	  * The result is a [[net.noresttherein.sugar.vars.Fallible Fallible]] containing the formatted output
	  * or an error message in case formatting of any of the values fails.
	  * @see [[net.noresttherein.sugar.format.Format.newBuilder]]
	  */ //Must be a def because it uses emptyLiquid, which is not initialized yet.
	def writer :RawWriter = new DefaultRawWriter

	/** An ad-hoc writer of arbitrary values creating [[net.noresttherein.sugar.format.Format.Raw Raw]] (formatted)
	  * output using this format's implicit [[net.noresttherein.sugar.format.Format.Mold molds]].
	  * objects can be appended using its [[net.noresttherein.sugar.format.Format.Mold]].
	  * This variant allows to take a prefix, suffix and a separator which will be inserted, respectively,
	  * before, between, and after all other data:
	  * {{{
	  *     JSON.make(",") + mage + tank + healer + dps to Raw
	  * }}}
	  * The result is a [[net.noresttherein.sugar.vars.Fallible Fallible]] containing the formatted output
	  * or an error message in case formatting of any of the values fails.
	  * @param separator An object inserted between any two elements, i.e. each time
	  *                  [[net.noresttherein.sugar.format.Format.RawBuilder.+ +]] is called on a builder returned
	  *                  by this builder, i.e. in place of any `+` in the full expression except the first.
	  *                  `Raw`/[[net.noresttherein.sugar.format.Format.Liquid Liquid]]. Note that,
	  *                  by default, the `separator` is melted each time in order to invoke the `Mold`'s method
	  *                  [[net.noresttherein.sugar.format.Format.Mold.append append]] in accordance to its contract.
	  *                  You may consider passing simply a `Liquid` which will result in a straightforward concatenation.
	  *                  Its type must have an implicit `Mold` - this includes in particular `Raw` and `Liquid`.
	  *                  Passing something melting to an [[net.noresttherein.sugar.format.Format.isEmpty empty]] liquid
	  *                  results in this step being omitted.
	  * @see [[net.noresttherein.sugar.format.Format.newBuilder]]
	  */
	def make[Sep :Mold](separator :Sep) :RawWriter =
		Mold[Sep].guardMelt(separator) match {
			case Passed(s)    => new DefaultRawWriter(Lack, Got(s), Lack)
			case fail :Failed => new DefaultRawWriter(fail, Lack, Lack, Lack)
		}

	/** An ad-hoc writer of arbitrary values creating [[net.noresttherein.sugar.format.Format.Raw Raw]] (formatted)
	  * output using this format's implicit [[net.noresttherein.sugar.format.Format.Mold molds]].
	  * objects can be appended using its `Mold`.
	  * This variant allows to take a prefix, suffix and a separator which will be inserted, respectively,
	  * before, between, and after all other data:
	  * {{{
	  *     JSON.make("[", ",", "]") + mage + tank + healer + dps to Raw
	  * }}}
	  * The result is a [[net.noresttherein.sugar.vars.Fallible Fallible]] containing the formatted output
	  * or an error message in case formatting of any of the values fails.
	  * @see [[net.noresttherein.sugar.format.Format.newBuilder]]
	  * @param prefix    An object included always at the very beginning of output
	  *                  `Raw`/[[net.noresttherein.sugar.format.Format.Liquid Liquid]].
	  *                  Its type must have an implicit `Mold` - this includes in particular `Raw` and `Liquid`.
	  * @param separator An object inserted between any two elements, i.e. each time
	  *                  [[net.noresttherein.sugar.format.Format.RawBuilder.+ +]] is called on a builder returned
	  *                  by this builder, i.e. in place of any `+` in the full expression except the first.
	  *                  `Raw`/[[net.noresttherein.sugar.format.Format.Liquid Liquid]].
	  *                  Its type must have an implicit `Mold` - this includes in particular `Raw` and `Liquid`.
	  *                  Passing something melting to an [[net.noresttherein.sugar.format.Format.isEmpty empty]] liquid
	  *                  results in this step being omitted.
	  * @param suffix    An object included always at the very end of output
	  *                  `Raw`/[[net.noresttherein.sugar.format.Format.Liquid Liquid]], just before
	  *                  [[net.noresttherein.sugar.format.Format.RawBuilder.to to]] (or other method returning the result)
	  *                  is called. Its type must have an implicit `Mold` - this includes in particular `Raw` and `Liquid`.
	  */
	def make[Pre :Mold, Sep :Mold, Suf :Mold](prefix :Pre, separator :Sep, suffix :Suf) :RawWriter =
		(for {
			pre  <- Mold[Pre].guardMelt(prefix)
			sep  <- Mold[Sep].guardMelt(separator)
			post <- Mold[Suf].guardMelt(suffix)
		} yield
			new DefaultRawWriter(
				Got(pre).filterNot(isEmpty), Got(sep).filterNot(isEmpty), Got(post).filterNot(isEmpty)
			)
		) match {
			case Passed(writer) => writer
			case fail :Failed => new DefaultRawWriter(fail, Lack, Lack, Lack)
		}

	/** An ad-hoc formatter of arbitrary values working as a pure [[scala.collection.mutable.Builder Builder]]
	  * alternative. Allows appending of values to format using method
	  * [[net.noresttherein.sugar.format.Format.RawWriter.+ +]].
	  * The [[net.noresttherein.sugar.format.Format.Raw raw]] result can be obtained through method
	  * [[net.noresttherein.sugar.format.Format.RawWriter.raw raw]]
	  * (or [[net.noresttherein.sugar.format.Format.RawWriter.out out]]
	  * for a [[net.noresttherein.sugar.format.Format.Liquid liquid]] value).
	  * @see [[net.noresttherein.sugar.format.Format.RawBuilder]]
	  */
	trait RawWriter {
		/** The result of formatting and combining all previously appended values in liquid format. */
		def out :Fallible[Liquid]

		/** The result of formatting and combining all previously appended values in raw format. */
		def raw :Fallible[Raw] = out.flatMap(guardCool)

		/** Same as [[net.noresttherein.sugar.format.Format.RawWriter.raw raw]], but as a two argument operator
		  * to allow calling it without a '.' after a sequence of [[net.noresttherein.sugar.format.Format.RawWriter.+ +]]
		  * calls.
		  */
		def to(raw :Raw.type) :Fallible[Raw] = this.raw

		/** Same as [[net.noresttherein.sugar.format.Format.RawWriter.out out]], but as a two argument operator
		  * to allow calling it without a '.' after a sequence of [[net.noresttherein.sugar.format.Format.RawWriter.+ +]]
		  * calls.
		  */
		def to(liquid :Liquid.type) :Fallible[Liquid] = out

		/** Formats the argument with its implicit `Mold` for this [[net.noresttherein.sugar.format.Format Format]]
		  * and appends it to previously formatted output. This handles also the cases
		  * of [[net.noresttherein.sugar.format.Format.Liquid Liquid]]
		  * and [[net.noresttherein.sugar.format.Format.Raw Raw]] data, as molds for these types are provided.
		  */
		def +[O :Mold](model :O) :RawWriter
	}

	/* Makes no sense as a monad, because it would need to return Liquid and a for comprehension would
	 * take a syntax essentially equivalent to just invoking each append as a separate call, assigning its result.
	 */
	private final class DefaultRawWriter(prefix :Fallible[Liquid], preSeparator :Opt[Liquid],
	                                     postSeparator :Opt[Liquid], postfix :Opt[Liquid])
		extends RawWriter with Serializable //because we have a Format.writer val
	{
		def this() = this(Passed(emptyLiquid), Lack, Lack, Lack)

		def this(prefix :Liquid) = this(Passed(prefix), Lack, Lack, Lack)

		def this(prefix :Opt[Liquid], separator :Opt[Liquid], postfix :Opt[Liquid]) =
			this(Passed(prefix.orDefault(emptyLiquid)), Lack, separator, postfix)

		override def out :Fallible[Liquid] = postfix match {
			case Got(suffix) => prefix flatMap (guardConcat(_, suffix))
			case _           => prefix
		}

		override def +[O :Mold](model :O) :RawWriter = {
			val appended = preSeparator match {
				case Got(s) => out flatMap (guardConcat(_, s)) flatMap (Mold[O].guardAppend(_, model))
				case _      => out flatMap (Mold[O].guardAppend(_, model))
			}
			new DefaultRawWriter(appended, postSeparator, postSeparator, postfix)
		}
	}


	/** Creates a mutable builder appending arbitrary values having
	  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] for this format
	  * with the usual [[net.noresttherein.sugar.format.Format.RawBuilder.+= +=]].
	  * The result can be obtained either through the standard
	  * [[net.noresttherein.sugar.format.Format.RawBuilder.result result]]`()`, or by calling `to Raw` or `to Liquid`:
	  * {{{
	  *     JSON.newBuilder += "[" += mage += "," += tank += "," += healer += "," += dps += "]" to JSON.Raw
	  * }}}
	  * Any errors during the procedure are reported as exceptions.
	  * @return an object providing an interface similar to the Scala [[scala.collection.mutable.Builder Builder]].
	  */
	def newBuilder :RawBuilder = new DefaultRawBuilder

	/** Creates a mutable builder appending arbitrary values having
	  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] for this format
	  * with the usual [[net.noresttherein.sugar.format.Format.RawBuilder.+= +=]]. The separator,
	  * if not [[net.noresttherein.sugar.format.Format.isEmpty empty]] after melting with its `Mold` type class,
	  * is inserted between every pair of appended values.
	  * The result can be obtained either through the standard
	  * [[net.noresttherein.sugar.format.Format.RawBuilder.result result]]`()`, or by calling `to Raw` or `to Liquid`:
	  * {{{
	  *     JSON.newBuilder(",") += mage += tank += healer += dps to JSON.Raw
	  * }}}
	  * Any errors during the procedure are reported as exceptions.
	  * @param separator an optional value inserted between any two calls to `+=`.
	  * @return an object providing an interface similar to the Scala [[scala.collection.mutable.Builder Builder]].
	  */
	@throws[FormattingException]("if the separator cannot be melted.")
	def newBuilder[Sep :Mold](separator :Sep) :RawBuilder = {
		val sep = Mold[Sep].melt(separator)
		if (isEmpty(sep)) new DefaultRawBuilder else new DefaultRawBuilder(Lack, Got(sep), Lack)
	}

	/** Creates a mutable builder appending arbitrary values having
	  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] for this format
	  * with the usual [[net.noresttherein.sugar.format.Format.RawBuilder.+= +=]].
	  * The arguments `prefix`, `separator` and `suffix` are melted using their `Mold` context bounds and,
	  * unless [[net.noresttherein.sugar.format.Format.isEmpty empty]], inserted before, between, and after
	  * all the elements when creating the result. It can be obtained either through the standard
	  * [[net.noresttherein.sugar.format.Format.RawBuilder.result result]]`()`, or by calling `to Raw` or `to Liquid`:
	  * {{{
	  *     JSON.newBuilder("[", ",", "]") += mage += tank += healer += dps to JSON.Raw
	  * }}}
	  * Any errors during the procedure are reported as exceptions. The builder is reusable: after returning
	  * [[net.noresttherein.sugar.format.Format.Liquid Liquid]] or [[net.noresttherein.sugar.format.Format.Raw Raw]]
	  * output, it is reset to the state following the return from this method. This can also happen
	  * using its [[net.noresttherein.sugar.format.Format.RawBuilder.clear clear]]`()` method.
	  * @param prefix    Any object with a `Mold` for this format, which will precede all values added to the bulider.
	  *                  This in particular includes [[net.noresttherein.sugar.format.Format.Raw Raw]]
	  *                  and [[net.noresttherein.sugar.format.Format.Liquid Liquid]] types.
	  * @param separator A value with this format's `Mold` inserted between any two calls to `+=`
	  *                  (i.e, before every one except the first).
	  * @param suffix    An object with a `Mold` for this format, which will be appended to the result
	  *                  when one of `to` or `result()` methods is called.
	  * @return an object providing an interface similar to the Scala [[scala.collection.mutable.Builder Builder]].
	  */
	@throws[FormattingException]("if for whatever reason any of the arguments cannot be melted.")
	def newBuilder[Pre :Mold, Sep :Mold, Suf :Mold](prefix :Pre, separator :Sep, suffix :Suf) :RawBuilder = {
		def prepare[X](model :X)(implicit mold :Mold[X]) = {
			val melted = mold.melt(model)
			if (isEmpty(melted)) Lack else Got(melted)
		}
		val pre = prepare(prefix)
		val sep = prepare(separator)
		val suf = prepare(suffix)
		new DefaultRawBuilder(pre, sep, suf)
	}

	/** A reusable, mutable builder of [[net.noresttherein.sugar.format.Format.Raw Raw]]
	  * or [[net.noresttherein.sugar.format.Format.Liquid Liquid]] data, allowing appending any values
	  * for which an implicit [[net.noresttherein.sugar.format.Format.Mold Mold]] exists.
	  * It's interface mirros the standard Scala [[scala.collection.mutable.Builder Builder]].
	  */
    trait RawBuilder {
		/** Appends the given value to previously created liquid data by the use
		  * of an implicit `Mold` for this format. If this is not the first call to `+=`, and the builder
		  * has been given a separator in its factory method, it will be inserted before the melted value.
		  * Appending objects melting to [[net.noresttherein.sugar.format.Format.isEmpty empty]] liquid still
		  * counts as the first call.
		  */
		@throws[FormattingException]("if for any reason the object cannot be formatted.")
		def +=[O :Mold](model :O) :this.type

		/** Converts previously appended values, followed by an optional suffix if given to this builder
		  * at its construction, to intermediate, liquid output. As this method takes a single argument
		  * (this format's [[net.noresttherein.sugar.format.Format.Liquid$ Liquid]] factory),
		  * it can be used as an infix operator, complementing nicely the usual
		  * [[net.noresttherein.sugar.format.Format.RawBuilder.+= +=]]. The builder can be reused after the method
		  * is called and anything appended from now on will not include previously appended values.
		  */
		@throws[FormattingException]("if the suffix given cannot be appended to previous data.")
		def to(liquid :Liquid.type) :Liquid

		/** Converts previously appended values, followed by an optional suffix if given to this builder
		  * at its construction, to raw output. As this method takes a single argument
		  * (this format's [[net.noresttherein.sugar.format.Format.Raw$ Raw]] factory),
		  * it can be used as an infix operator (unlike `result()` requiring dot notation),
		  * complementing nicely the usual [[net.noresttherein.sugar.format.Format.RawBuilder.+= +=]].
		  * The builder can be reused after the method is called and anything appended from now on will not
		  * include previously appended values.
		  */
		@throws[FormattingException]("if the suffix given cannot be appended to previous data or conversion to Raw fails.")
		def to(raw :Raw.type) :Raw = cool(to(Liquid))

		/** Converts previously appended values and any initially specified suffix to intermediate, liquid output.
		  * The builder can be reused after the method is called and anything appended from now on will not
		  * include previously appended values.
		  */
		@throws[FormattingException]("if the suffix given cannot be appended to previous data or conversion to Raw fails.")
		def result() :Raw = to(Raw)

		/** Clears all previously appended data, resetting the builder to the initial, empty state,
		  * ready to create new values. The builder will retain any parameters given to its at its construction.
		  */
		def clear() :Unit
	}

	private class DefaultRawBuilder(prefix :Opt[Liquid] = Lack, separator :Opt[Liquid] = Lack, suffix :Opt[Liquid] = Lack)
		extends RawBuilder
	{
		private[this] var liquid :Liquid = prefix getOrElse emptyLiquid
		private[this] var inInitialState = true

		override def +=[O :Mold](model :O) :this.type = separator match {
			case Got(s) =>
				if (inInitialState) {
					liquid = concat(liquid, s)
					inInitialState = false
				}
				liquid = Mold[O].append(liquid, model)
				this
			case _ =>
				liquid = Mold[O].append(liquid, model)
				this
		}
		override def to(liquid :Liquid.type) :Liquid = {
			val res = this.liquid; clear(); res
		}
		override def clear() :Unit = { liquid = prefix getOrElse emptyLiquid; inInitialState = true }
	}




	//would be nice to have apply[M](raw :Raw), but this conflicts with Moldmaker factory.
	/** Reads a value of `M` from the raw input using an implicit `Mold` for this format.
	  * Any content following the read data is ignored.
	  * @see [[net.noresttherein.sugar.format.Format.read]]
	  */
	@throws[ParsingException]("If the input does not contain a correctly formatted model of the implicit Mold.")
	def head[M :Mold](raw :Raw) :M = Mold[M].head(melt(raw))

	/** Parses the raw input into an instance of `S`. If parsing leaves
	  * a non [[net.noresttherein.sugar.format.Format.isEmpty empty]] remainder, an exception is thrown.
	  * @see [[net.noresttherein.sugar.format.Format.head]]
	  */ //consider: it would be good to name them cast & melt, but melt is taken by Raw => Liquid
	@throws[ParsingException](
		"If the input does not contain a correctly formatted model of the implicit Mold or has a superfluous suffix.")
	def read[M :Mold](raw :Raw) :M = Mold[M].cast(melt(raw))

	/** Formats the given value using an implicit `Mold` for this format as its raw representation. */
	@throws[FormattingException]("if for any reason the value cannot be formatted.")
	def write[M :Mold](model :M) :Raw = cool(Mold[M].melt(model))




	/** A type class used to convert between arbitrary Scala types (''solids'' ) and this `Format`'s
	  * [[net.noresttherein.sugar.format.Format.Liquid Liquid]] type, i.e. an intermediate form easily
	  * convertible to its [[net.noresttherein.sugar.format.Format.Raw Raw]] (formatted) type.
	  * Molds use nomenclature reflecting the physical casting process: instances of the type `S`
	  * they work on are called ''models'', their creation from the 'melted' raw data ''casting'',
	  * and the reverse process of decomposing a model into its liquid form - ''melting''.
	  *
	  * Molds are also often used in creation of other molds and should assume that liquid arguments
	  * are correctly formed the technical point of view, but not necessarily a correctly formatted model of this mold.
	  * Nominally, the similar applies also to [[net.noresttherein.sugar.format.Format.Mold.melt melting]]:
	  * the model object is expected to be initialized, although possibly contain properties which cannot be formatted.
	  * `Mold` supports three kinds of error handling:
	  *   1. the basic methods without prefixes or suffixes in their name and returning the values directly
	  *      must be expected to throw a [[net.noresttherein.sugar.format.FormatException FormatException]]
	  *      (or, possibly, other in case of a deeper problem).
	  *   1. Each of the above has also a variant with a name ending with `Opt`, which return the types of the former
	  *      inside an `Opt`. These methods should not throw any exceptions related to their arguments (i.e., repeatable)
	  *      at all and are used in situation where the format allows multiple different structures
	  *      and an appropriate `Mold` must be chosen from among a permittable list.
	  *   1. Methods starting with `guard` implement a purely functional approach and return
	  *      a [[net.noresttherein.sugar.vars.Fallible Fallible]]. They are likewise not allow to throw exception,
	  *      but instead only use a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] value to indicate
	  *      an error. This is a less lightweight approach than the previous one (although `Failed` supports
	  *      lazy error messages), but still more lightweight then throwing and catching an exception
	  *      with exact stack trace information.
	  *
	  * The semantics of methods with corresponding names and varying in return type only must be equivalent
	  * in their successful path. Molds can be created in four ways:.
	  *   1. By implementing the `Mold` interface directly, or indirectly through base traits introduced
	  *      for this purpose:
	  *      [[net.noresttherein.sugar.format.Format.SimpleThrowingMold SimpleThrowingMold]],
	  *      [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold SimpleOptBasedMold]],
	  *      [[net.noresttherein.sugar.format.Format.SimpleGuardingMold SimpleGuardingMold]]
	  *      (good for molds which do not use other molds) and
	  *      [[net.noresttherein.sugar.format.Format.SpecialThrowingMold SpecialThrowingMold]],
	  *      [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold SpecialOptBasedMold]],
	  *      [[net.noresttherein.sugar.format.Format.SimpleGuardingMold SpecialGuardingMold]]
	  *      which are based on more generic methods needed to properly use unknown `Mold` implementations.
	  *      They all leave for implementation by subclasses only a pair methods - the difference lies
	  *      in which kind of error reporting will those abstract methods use - throwing exceptions
	  *      (most convenient to implement, but least efficient in handling errors),
	  *      simply returning the result as an [[net.noresttherein.sugar.vars.Opt Opt]], without additional
	  *      error information (most efficient), or in a functional manner by flat mapping
	  *      a [[net.noresttherein.sugar.vars.Fallible Fallible]] containing either the result or error information.
	  *      The latter is both a recommended base trait and way of using all molds (as `Mold` interfane supports
	  *      all three kinds of error reporting, the above difference amounted only to which one was the primary.
	  *   1. By factory methods in the companion [[net.noresttherein.sugar.format.Format.Mold$ Mold]] object,
	  *      accepting parsing and formatting functions.
	  *   1. By mapping an existing mold, either with its [[net.noresttherein.sugar.format.Format.Mold.map map]] method,
	  *      or [[net.noresttherein.sugar.format.Format.Mold$.map the one in its companion object]],
	  *      or by [[net.noresttherein.sugar.format.Format.map mapping]]
	  *      an implicit [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]].
	  *   1. Through a monadic-style composition in a ''for comprehension'' (code assumes existence of implicit molds
	  *      `JSON.Mold[Int]` and `JSON.Mold[String]`):
	  *      {{{
	  *          case class Dragon(name :String, color :String, level :Int)
	  *
	  *          object Dragon {
	  *              implicit val DragonAsJSON = for {
	  *                  dragon <- JSON[Dragon]
	  *                  name   <- dragon("name")(_.name)
	  *                  color  <- dragon("color")(_.color)
	  *                  level  <- dragon("level")(_.level)
	  *              } yield Dragon(name, color, level)
	  *          }
	  *      }}}
	  *      The above code creates specifically an instance of `JSON.Mold[Dragon]`. Alternatively,
	  *      a generic [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]`[M]`, which uses
	  *      the same process to define `format.Mold[Dragon]` for ''any format'', can be made starting with
	  *      the `Format` object's [[net.noresttherein.sugar.format.Format$.apply(name:String) apply]] method:
	  *      {{{
	  *          implicit val DragonFormat = for {
	  *              dragon <- Format[Dragon]
	  *              name   <- dragon("name")(_.name)
	  *              color  <- dragon("color")(_.color)
	  *              level  <- dragon("level")(_.level)
	  *          } yield Dragon(name, color, level)
	  *      }}}
	  *      For any `format :Format`, an existence of an implicit `MoldLayout[M]` implies the existence
	  *      of an implicit `format.Mold[M]`.
	  *
	  * In order to parse input data and format objects as their raw format, a `Mold` for this type is required,
	  * but it is normally passed implicitly and, because they work on preprocessed data only,
	  * they aren't normally used by the application. See the documentation of enclosing
	  * [[net.noresttherein.sugar.format.Format Format]] class for information about gateway methods for this purpose.
	  *
	  * Applications are responsible for creating molds for all types from the business domain
	  * as well as any other specific types, as not all formats will support all general purpose types.
	  * In order however to allow some interoperability and make creating format agnostic `MoldLayout`s possible,
	  * every format must provide implementation for several basic types; they can be fined in the `Format` trait itself.
	  * In turn, as mentioned above, any type providing a `MoldLayout` will have a `Mold` available for any format.
	  *
	  * @tparam M the molded 'model' type.
	  * @see [[net.noresttherein.sugar.format.Format.Moldmaker]]
	  * @see [[net.noresttherein.sugar.format.Format.Parts]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SimpleMold]]
	  * @see [[net.noresttherein.sugar.format.Format.NamedMold]]
	  */ //todo: specialization: requires moving it out of Format. So do outside implicits requiring molds
	@implicitNotFound("I do not know how to represent ${M} in the desired format.")
	@implicitAmbiguous("Conflicting methods of representing ${M} in the desired format.")
	trait Mold[M] extends Any with Serializable {
		/** Parses the whole of the given formatted liquid as this mold's model type.
		  * If a prefix of `liquid` parses correctly as`M`,
		  * but leaves a [[net.noresttherein.sugar.format.Format.isEmpty non empty]] suffix,
		  * a [[net.noresttherein.sugar.format.ParsingException ParsingException]] is still thrown.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.castOpt]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.guard]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.head]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.next]]
		  */ //apply conflicts somewhat with Mold[S] - val Mold declares multiple apply methods
		@throws[ParsingException]("if liquid is not a correctly formatted model value of this mold.")
		def cast(liquid :Liquid) :M = {
			val (res, rem) = next(liquid)
			if (isEmpty(rem))
				res
			else
				throw ParsingException(Format.this :Format.this.type)(
					liquid, "Non empty remainder '" + rem + "' after parsing " + res + " from '" + liquid + "'."
				)
		}
		/** Attempts to parse the whole of the given formatted liquid as this mold's model type.
		  * If parsing leaves a [[net.noresttherein.sugar.format.Format.isEmpty non empty]] suffix of `liquid`,
		  * [[net.noresttherein.sugar.vars.Opt.Lack Lack]] is returned even if the prefix is in the correct format.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.cast]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.headOpt]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.nextOpt]]
		  */
		def castOpt(liquid :Liquid) :Opt[M] = nextOpt(liquid) match {
			case Got((model, rem)) if isEmpty(rem) => Got(model)
			case _ => Lack
		}
		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.cast cast]],
		  * parsing the whole of the given formatted liquid as this mold's model type,
		  * which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * If parsing leaves a [[net.noresttherein.sugar.format.Format.isEmpty non empty]] suffix of `liquid`,
		  * [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] is returned even if the prefix
		  * is in tje correct format.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.cast]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.guardHead]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.guardNext]]
		  */
		def guardCast(liquid :Liquid) :Fallible[M] = guardNext(liquid) match {
			case Passed((res, rem)) if isEmpty(rem) => Passed(res)
			case Passed((res, rem)) => Failed(
				() => "Non empty remainder '" + rem + "' after parsing " + res + " from  '" + liquid + "'."
			)
			case failed :Failed => failed
		}

		/** Parses a prefix of the given `liquid` as a model value of this mold, ignoring any leftover suffix.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.next next]]`._1`.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.headOpt]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.apply]]
		  */
		@throws[ParsingException]("if liquid doesn't start with a correctly formatted model value of this mold.")
		def head(liquid :Liquid) :M// = advance(liquid)._2

		/** Attempts to parse a prefix of `liquid` as a model value of this mold, ignoring any leftover suffix.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.nextOpt nextOpt]]`.map(_._1)`.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.head]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.castOpt]]
		  */
		def headOpt(liquid :Liquid) :Opt[M]// = advanceOpt(liquid) map (_._2)

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.head head]], parsing a prefix
		  * of `liquid` as a model value of this mold and ignoring any leftover suffix,
		  * which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.guardNext guardNext]]`.map(_._1)`.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.head]]
		  * @see [[net.noresttherein.sugar.format.Format.Mold.guardCast]]
		  */
		def guardHead(liquid :Liquid) :Fallible[M]// = guardAdvance(liquid) map (_._2)

		/** Parses a prefix of `liquid` as a model value of this mold, returning the parsed model together
		  * with an unparsed suffix following the parsed content.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.advance]]
		  */
		@throws[ParsingException]("if liquid doesn't start with a correctly formatted model value of this mold.")
		def next(liquid :Liquid) :(M, Liquid) = {
			val (_, model, unparsed) = advance(liquid)
			(model, unparsed)
		}

		/** Attempts to parse a prefix of `liquid` as a model value of this mold, returning the parsed model
		  * together with an unparsed suffix following the parsed content if successful.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.advanceOpt]]
		  */ //this could be a new unapply
		def nextOpt(liquid :Liquid) :Opt[(M, Liquid)] =
			advanceOpt(liquid) map { case (_, model, unparsed) => (model, unparsed) }

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.next next]],
		  * parsing a prefix of `liquid` as a model value of this mold and returning the parsed model together
		  * with an unparsed suffix following the parsed content if successful,
		  * which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @see [[net.noresttherein.sugar.format.Format.Mold.guardAdvance]]
		  */
		def guardNext(liquid :Liquid) :Fallible[(M, Liquid)] =
			guardAdvance(liquid) map { case (_, model, unparsed) => (model, unparsed) }

		/** Parses a prefix of `liquid` as a model value of this mold, splitting the input
		  * into parsed and unparsed fragments. The returned triplet consists of a prefix of `liquid` corresponding
		  * to the parsed model value, a parsed value, and a suffix of `liquid` following the parsed prefix.
		  * @return `advance(`[[net.noresttherein.sugar.format.Format.Liquid.empty Liquid.empty]]`, liquid)`.
		  */
		@throws[ParsingException]("if liquid doesn't start with a correctly formatted model value of this mold.")
		def advance(liquid :Liquid) :(Liquid, M, Liquid)// = advance(emptyLiquid, liquid)

		/** Attempts to parse a prefix of `liquid` as a model value of this mold, splitting the input
		  * into parsed and unparsed fragments.
		  * If `liquid` starts with a correctly formatted model value, then the method returns an `Opt` containing
		  * a triplet of a parsed prefix corresponding to the returned value, a parsed value,
		  * and an unparsed suffix of `liquid`.
		  * @return `advanceOpt(`[[net.noresttherein.sugar.format.Format.Liquid.empty Liquid.empty]]`, liquid)`.
		  */
		def advanceOpt(liquid :Liquid) :Opt[(Liquid, M, Liquid)]// = advanceOpt(emptyLiquid, liquid)

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.advance advance]], parsing
		  * a prefix of `liquid` as a model value of this mold and splitting the input into parsed
		  * and unparsed fragments, which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * If `liquid` starts with a correctly formatted model value, then the method returns
		  * [[net.noresttherein.sugar.vars.Fallible.Passed Passed]] containing a triplet
		  * of a parsed prefix corresponding to the returned value, a parsed value, and an unparsed suffix of `liquid`.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @return `guardAdvance(`[[net.noresttherein.sugar.format.Format.Liquid.empty Liquid.empty]]`, liquid)`.
		  */
		def guardAdvance(liquid :Liquid) :Fallible[(Liquid, M, Liquid)]// = guardAdvance(emptyLiquid, liquid)

		/** Parses a prefix of `suffix` as a model value of this mold. This is the most generic parsing
		  * method in this class; it differs from the single argument `advance` in that it also takes
		  * a `liquid` argument preceding `suffix` in some larger parsed entity. This is used by molds parsing
		  * individual properties of a larger `Mold`'s model and allows to implement 'look behind' functionality
		  * in monadic `Mold` [[net.noresttherein.sugar.format.Format.Moldmaker composition]] through a non-consuming
		  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`.`[[net.noresttherein.sugar.format.Format.Parts.prefix prefix]]
		  * part. This in turn is needed for implementing things like checksums of the formatted content.
		  * @param  prefix Already parsed formatted input for an outer mold. This does not mean it includes everything
		  *                from the original `liquid` argument to the most outer `Mold` parsing the argument given
		  *                by the application, but only data already parsed/formatted by a directly enclosing `Mold[O]`
		  *                (i.e., the mold of the class `O` owning the molded property), ''without''
		  *                a leading [[net.noresttherein.sugar.format.Format.open Format.open]] wrapper,
		  *                or any other modifications applied by the `Mold` adapter returned by
		  *                [[net.noresttherein.sugar.format.Format.wrap Format.wrap]]. For example,
		  *                in case of an [[net.noresttherein.sugar.format.XML XML]] format, this would not include
		  *                a leading tag `<O>`.
		  * @param  suffix the fragment of input containing everything following `prefix` in the original raw argument
		  *                given to the most outer `Mold`. unlike `prefix`, it does include
		  *                a [[net.noresttherein.sugar.format.Format.close closing]] 'tag', as well as everything
		  *                that follows (in particular, parts of an outer `Mold`).
		  * @return a triplet consisting of a [[net.noresttherein.sugar.format.Format.concat concatenation]] of `prefix`
		  *         and the parsed fragment of `suffix`, a parsed value, and an unparsed fragment of `suffix` following
		  *         the parsed value.
		  */
		@throws[ParsingException]("if suffix doesn't start with a correctly formatted model value of this mold.")
		def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid)

		/** Attempts to parse a prefix of `suffix` as a model value of this mold. This is the most generic parsing
		  * method in this class; it differs from the single argument `advance` in that it also takes
		  * a `liquid` argument preceding `suffix` in some larger parsed entity. This is used by molds parsing
		  * individual properties of a larger `Mold`'s model and allows to implement 'look behind' functionality
		  * in monadic `Mold` [[net.noresttherein.sugar.format.Format.Moldmaker composition]] through a non-consuming
		  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`.`[[net.noresttherein.sugar.format.Format.Parts.prefix prefix]]
		  * part. This in turn is needed for implementing things like checksums of the formatted content.
		  * @return if `suffix` starts with a correctly formatted model value of this mold, then the returned `Opt`
		  *         contains a triplet consisting of a [[net.noresttherein.sugar.format.Format.concat concatenation]]
		  *         of `prefix` and the parsed fragment of `suffix`, a parsed value,
		  *         and an unparsed fragment of `suffix` following the parsed value.
		  */
		def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)]

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.advance advance]],
		  * parsing a prefix of `suffix` as a model value of this mold, which reports errors
		  * as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]], instead of throwing an exception.
		  * This is the most generic parsing
		  * method in this class; it differs from the single argument `advance` in that it also takes
		  * a `liquid` argument preceding `suffix` in some larger parsed entity. This is used by molds parsing
		  * individual properties of a larger `Mold`'s model and allows to implement 'look behind' functionality
		  * in monadic `Mold` [[net.noresttherein.sugar.format.Format.Moldmaker composition]] through a non-consuming
		  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`.`[[net.noresttherein.sugar.format.Format.Parts.prefix prefix]]
		  * part. This in turn is needed for implementing things like checksums of the formatted content.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @return if `suffix` starts with a correctly formatted model value of this mold,
		  *         then the returned [[net.noresttherein.sugar.vars.Fallible.Passed Passed]]
		  *         contains a triplet consisting of a [[net.noresttherein.sugar.format.Format.concat concatenation]]
		  *         of `prefix` and the parsed fragment of `suffix`, a parsed value,
		  *         and an unparsed fragment of `suffix` following the parsed value.
		  */
		def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)]

		/** Validates that `suffix` starts with properly formatted `constant`.
		  * The comparison happens on the model level, by parsing `suffix` using regular `advance(prefix, suffix)`,
		  * and comparing the parsed value with `constant` using `equals`. If the comparison fails
		  * (or `suffix` doesn't start with a correctly formatted model value of this mold at all), an exception
		  * is thrown. Otherwise the returned `(parsed, model, unparsed)` triple is exactly the same
		  * as for the regular, non-comparing `advance`. Note that this means that the parsed fragment may potentially
		  * not equal `this.`[[net.noresttherein.sugar.format.Format.Mold.melt melt]]`(constant)`
		  * (in particular in `String`-based formats ignoring whitespace).
		  */
		@throws[ParsingException]("if suffix doesn't start with correctly formatted constant.")
		def advance(constant :M)(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) = {
			val res = advance(prefix, suffix)
			if (constant == res._2)
				res
			else
				throw ParsingException(Format.this :Format.this.type)(suffix,
					"Failed to parse '" + suffix + "' as " + this + ": expected " + constant + ", got " + res._2 + "."
				)
		}
		/** Safely validates that `suffix` starts with properly formatted `constant`.
		  * The comparison happens on the model level, by parsing `suffix` using regular `advance(prefix, suffix)`,
		  * and comparing the parsed value with `constant` using `equals`. If the comparison fails
		  * (or `suffix` doesn't start with a correctly formatted model value of this mold at all), the method
		  * returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]]. Otherwise the returned `(parsed, model, unparsed)`
		  * triple is exactly the same as for the regular, non-comparing `advance`. Note that this means that the parsed
		  * fragment may potentially not equal `this.`[[net.noresttherein.sugar.format.Format.Mold.melt melt]]`(constant)`,
		  * (in particular in `String`-based formats ignoring whitespace).
		  */
		def advanceOpt(constant :M)(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
			advanceOpt(prefix, suffix).filter(_._2 == constant)

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.advance advance]],
		  * validating that `suffix` starts with properly formatted `constant`,.
		  * which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * The comparison happens on the model level, by parsing `suffix` using regular `advance(prefix, suffix)`,
		  * and comparing the parsed value with `constant` using `equals`. If the comparison fails
		  * (or `suffix` doesn't start with a correctly formatted model value of this mold at all), the method
		  * returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]]. Otherwise the returned `(parsed, model, unparsed)`
		  * triple is exactly the same as for the regular, non-comparing `advance`. Note that this means that the parsed
		  * fragment may potentially not equal `this.`[[net.noresttherein.sugar.format.Format.Mold.melt melt]]`(constant)`,
		  * (in particular in `String`-based formats ignoring whitespace).
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  */
		def guardAdvance(constant :M)(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
			guardAdvance(prefix, suffix) match {
				case res @ Passed((_, model, _)) if model == constant => res
				case Passed((_, model, _)) => Failed(
					() => "Failed to parse '" + suffix + "' as " + this + ": expected " + constant + ", got " + model + "."
				)
				case failed :Failed => failed
			}

		/** Validates that `suffix` starts with properly formatted `constant`. This method is exactly equivalent
		  * to [[net.noresttherein.sugar.format.Format.Mold.advance advance]]`(constant)(prefix, suffix)`,
		  * but omits the superfluous parsed duplicate of `constant`.
		  * @return A concatenation of `prefix` with the parsed fragment of `suffix` containing formatted `constant`,
		  *         and the remainder of `suffix` after dropping the parsed `fragment`.
		  */
		@throws[ParsingException]("if suffix doesn't start with correctly formatted constant.")
		def expect(constant :M)(prefix :Liquid, suffix :Liquid) :(Liquid, Liquid) = {
			val (parsed, _, unparsed) = advance(constant)(prefix, suffix)
			(parsed, unparsed)
		}
		/** Safely validates that `suffix` starts with properly formatted `constant`. This method is exactly equivalent
		  * to [[net.noresttherein.sugar.format.Format.Mold.advanceOpt advanceOpt]]`(constant)(prefix, suffix)`,
		  * but omits the superfluous parsed duplicate of `constant`.
		  * @return A concatenation of `prefix` with the parsed fragment of `suffix` containing formatted `constant`,
		  *         and the remainder of `suffix` after dropping the parsed `fragment`.
		  */
		def expectOpt(constant :M)(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, Liquid)] =
			advanceOpt(constant)(prefix, suffix) map { case (parsed, _, unparsed) => (parsed, unparsed) }

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.expect expect]], validating
		  * that `suffix` starts with properly formatted `constant`, which reports errors
		  * as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]], instead of throwing an exception.
		  * This method is exactly equivalent
		  * to [[net.noresttherein.sugar.format.Format.Mold.guardAdvance guardAdvance]]`(constant)(prefix, suffix)`,
		  * but omits the superfluous parsed duplicate of `constant`.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @return A concatenation of `prefix` with the parsed fragment of `suffix` containing formatted `constant`,
		  *         and the remainder of `suffix` after dropping the parsed `fragment`,
		  *         or a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] if there is no match.
		  */
		def guardExpect(constant :M)(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, Liquid)] =
			guardAdvance(constant)(prefix, suffix) map { case (parsed, _, unparsed) => (parsed, unparsed) }

		/** Validates that `liquid` starts with properly formatted `constant` and returns its suffix
		  * remaining after dropping the parsed `constant` fragment.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.advance advance]]`(constant)(Liquid.empty, liquid)._3`.
		  */
		@throws[ParsingException]("if suffix doesn't start with correctly formatted constant.")
		def drop(constant :M)(liquid :Liquid) :Liquid = advance(constant)(emptyLiquid, liquid)._3

		/** Safely validates that `liquid` starts with properly formatted `constant` and returns its suffix
		  * remaining after dropping the parsed `constant` fragment.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.advanceOpt advanceOpt]]`(constant)(Liquid.empty, liquid).map(_._3)`.
		  */
		def dropOpt(constant :M)(liquid :Liquid) :Opt[Liquid] = advanceOpt(constant)(emptyLiquid, liquid) map (_._3)

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.drop drop]], validating
		  * that `liquid` starts with properly formatted `constant` and returning its suffix remaining
		  * after dropping the parsed `constant` fragment,
		  * which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.guardAdvance guardAdvance]]`(constant)(Liquid.empty, liquid).map(_._3)`.
		  */
		def guardDrop(constant :M)(liquid :Liquid) :Fallible[Liquid] =
			guardAdvance(constant)(emptyLiquid, liquid) map (_._3)


		/** Formats the given `model` value.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.append append]]`(`[[net.noresttherein.sugar.format.Format.Liquid.empty empty]]`, model)`.
		  */
		@throws[FormattingException]("if model cannot be formatted by this mold for any reason.")
		def melt(model :M) :Liquid// = append(emptyLiquid, model)

		/** Attempts to format the given `model` value.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.appendOpt appendOpt]]`(`[[net.noresttherein.sugar.format.Format.Liquid.empty empty]]`, model)`.
		  */
		def meltOpt(model :M) :Opt[Liquid]// = appendOpt(emptyLiquid, model)

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.melt melt]], formatting
		  * the given `model` value, which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception.
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  * @return [[net.noresttherein.sugar.format.Format.Mold.guardAppend guardAppend]]`(`[[net.noresttherein.sugar.format.Format.Liquid.empty empty]]`, model)`.
		  */
		def guardMelt(model :M) :Fallible[Liquid]// = guardAppend(emptyLiquid, model)

		/** Formats the given `model` value and appends the result to `prefix`.
		  * In most cases, this is equivalent to
		  * [[net.noresttherein.sugar.format.Format.concat concat]]`(prefix, `[[net.noresttherein.sugar.format.Format.Mold.melt melt]]`(model)`.
		  * The extra `prefix` argument however allows molds for individual properties of a larger mold's model value
		  * to inspect previously formatted properties, for example to write a checksum for the whole object.
		  * This is in particular used by
		  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`.`[[net.noresttherein.sugar.format.Format.Parts.prefix prefix]]
		  * in monadic `Mold` [[net.noresttherein.sugar.format.Format.Moldmaker composition]].
		  */
		@throws[FormattingException]("if model cannot be formatted by this mold for any reason.")
		def append(prefix :Liquid, model :M) :Liquid

		/** Attempts to format the given `model` value and append the result to `prefix`.
		  * In most cases, this is equivalent to
		  * [[net.noresttherein.sugar.format.Format.Mold.meltOpt meltOpt]]`(model).map(`[[net.noresttherein.sugar.format.Format.concat concat]]`(prefix, _))`.
		  * The extra `prefix` argument however allows molds for individual properties of a larger mold's model value
		  * to inspect previously formatted properties, for example to write a checksum for the whole object.
		  * This is in particular used by
		  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`.`[[net.noresttherein.sugar.format.Format.Parts.prefix prefix]]
		  * in monadic `Mold` [[net.noresttherein.sugar.format.Format.Moldmaker composition]].
		  */
		def appendOpt(prefix :Liquid, model :M) :Opt[Liquid]

		/** A variant of method [[net.noresttherein.sugar.format.Format.Mold.append append]],
		  * formatting the given `model` value and appending the result to `prefix`,
		  * which reports errors as a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]],
		  * instead of throwing an exception. In most cases, this is equivalent to
		  * [[net.noresttherein.sugar.format.Format.Mold.guardMelt guardMelt]]`(model).map(`[[net.noresttherein.sugar.format.Format.concat concat]]`(prefix, _)`.
		  * The extra `prefix` argument however allows molds for individual properties of a larger mold's model value
		  * to inspect previously formatted properties, for example to write a checksum for the whole object.
		  * This is in particular usd by
		  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`.`[[net.noresttherein.sugar.format.Format.Parts.prefix prefix]]
		  * in monadic `Mold` [[net.noresttherein.sugar.format.Format.Moldmaker composition]].
		  * All 'guard' methods can be composed in a for comprehension or as a chain
		  * of `Fallible.`[[net.noresttherein.sugar.vars.FallibleExtension.flatMap flatMap]] calls.
		  */
		def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid]

		//todo: implement somehow an alternative.

		/** Creates a mold for some type `T` which can be converted to and from this mold's model type `M`. */
		def map[T](read :M => T, write :T => M) :Mold[T] = new MappedMold(this, read, write)

		/** Creates a mold for some type `T` which can be converted to and from this mold's model type `M`.
		  * @param name  the [[net.noresttherein.sugar.format.Format.NamedMold.name name]] of the created mold,
		  *              that is an application name for the new molded type.
		  * @param read  a function applied to every value parsed by this mold to obtain the new model.
		  * @param write a function applied to every model value of the name mold before formatting it with this mold.
		  */
		def map[T](name :String, read :M => T, write :T => M) :NamedMold[T] =
			new NamedMappedMold(name, this, read, write)

		/** Creates a mold for some type `T` which can be converted to and from this mold's model type `M`. */
		def optMap[T](read :M => Potential[T], write :T => Potential[M]) :Mold[T] =
			new OptMappedMold(this, read, write)

		/** Creates a mold for some type `T` which can be converted to and from this mold's model type `M`.
		  * @param name  the [[net.noresttherein.sugar.format.Format.NamedMold.name name]] of the created mold,
		  *              that is an application name for the new molded type.
		  * @param read  a function applied to every value parsed by this mold to obtain the new model.
		  * @param write a function applied to every model value of the name mold before formatting it with this mold.
		  */
		def optMap[T](name :String, read :M => Potential[T], write :T => Potential[M]) :NamedMold[T] =
			new NamedOptMappedMold(name, this, read, write)

		/** A `Mold` parsing the same values as this mold, but additionally paired with liquid fragments corresponding
		  * to the parsed model. When formatting, the first element of the pair is ignored.
		  */
		def mirror :Mold[(Liquid, M)] = new MirrorMold(this)

		/** An optional `Mold` for the same model type. [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]]
		  * values are formatted as [[net.noresttherein.sugar.format.Format.Liquid.empty empty]] liquids.
		  * When parsing, an [[net.noresttherein.sugar.vars.Potential.Existent Existent]] value is returned on success
		  * and `Inexistent` if this mold fails to parse the argument(s).
		  *
		  * Note that this is not a universal implementation appropriate for all formats;
		  * it is provided only as an utility.
		  */
		def potential :Mold[Potential[M]] = new PotentialMold(this)

		/** An optional `Mold` for the same model type. [[net.noresttherein.sugar.vars.Opt.Lack Lack]]
		  * values are formatted as [[net.noresttherein.sugar.format.Format.Liquid.empty empty]] liquids.
		  * When parsing, a [[net.noresttherein.sugar.vars.Opt.Got Got]] value is returned on success
		  * and `Inexistent` if this mold fails to parse the argument(s).
		  *
		  * Note that this is not a universal implementation appropriate for all formats;
		  * it is provided only as an utility.
		  */
        def opt :Mold[Opt[M]] = new OptMold(this)

		/** An optional `Mold` for the same model type. `None` is formatted
		  * as an [[net.noresttherein.sugar.format.Format.Liquid.empty empty]] liquid.
		  * When parsing, a `Some` is returned on success and `None` if this mold fails to parse the argument(s).
		  *
		  * Note that this is not a universal implementation appropriate for all formats;
		  * it is provided only as an utility.
		  */
		def option :Mold[Option[M]] = new OptionMold(this)

		/** A mold for the same model type, but making potential errors occurring when parsing or retrieving
		  * a value from a larger model available to the application.
		  * An instance of `Fallible[M]` is accessible directly when composing another `Mold`, instead of being
		  * unwrapped behind the scenes (and errors propagated without application's control).
		  */
		def guard :Mold[Fallible[M]] = new FallibleMold(this)
	}


	/** An overridable companion object to [[net.noresttherein.sugar.format.Format.Mold! Mold]], serving as its factory. */
	val Mold :MoldFactory = new MoldFactory



	/** Base type of the [[net.noresttherein.sugar.format.Format.Mold$ companion object]] to
	  * [[net.noresttherein.sugar.format.Format.Mold! Mold]]. Requires subclasses to provide implicit `Mold`
	  * instances for various basic type in order to allow some basic functionality abstracting over actual
	  * [[net.noresttherein.sugar.format.Format Format]] instances (that is, define `Mold`s without knowing
	  * what is the [[net.noresttherein.sugar.format.Format.Liquid Liquid]] type their format.
	  */
	@SerialVersionUID(ver)
	class MoldFactory extends Serializable {
		/** Summons a `Mold[M]`, if available. */
		@inline def apply[M](implicit mold :Mold[M]) :Mold[M] = mold

		/** Creates a `Mold[Y]` by bidirectionally mapping an implicit `Mold[X]` */
		def map[X, Y](read :X => Y, write :Y => X)(implicit mold :Mold[X]) :Mold[Y] =
			mold.map(read, write)

		/** Creates a `Mold[Y]` by bidirectionally mapping an implicit `Mold[X]` */
		def map[X, Y](name :String, read :X => Y, write :Y => X)(implicit mold :Mold[X]) :NamedMold[Y] =
			new NamedMappedMold[X, Y](name, mold, read, write)

		/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
		  * The given functions are used directly to implement their eponymous methods.
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
		def apply[M](advance :(Liquid, Liquid) => (Liquid, M, Liquid), append :(Liquid, M) => Liquid) :Mold[M] =
			new CustomMold(advance, append)

		/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
		  * The given functions are used directly to implement their eponymous methods.
		  * This method is fully useful only for molds performing such functions
		  * as checksum calculation and verification - most molds do not need to inspect already parsed/formatted data.
		  * @param name    An arbitrary name of the molded type `M`, used only for debugging purposes.
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
		def apply[M](name :String, advance :(Liquid, Liquid) => (Liquid, M, Liquid), append :(Liquid, M) => Liquid)
				:NamedMold[M] =
			new NamedCustomMold(name, advance, append)

		/** The simplest factory method for a `Mold`, accepting functions necessary to implement its abstract methods.
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
		def apply[M](split :Liquid => (Liquid, M, Liquid), melt :M => Liquid) :Mold[M] =
			new SimplestMold(split, melt)

		/** The simplest factory method for a `Mold`, accepting functions necessary to implement its abstract methods.
		  * @param name    An arbitrary name of the molded type `M`, used only for debugging purposes.
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
		def apply[M](name :String, split :Liquid => (Liquid, M, Liquid), melt :M => Liquid) :NamedMold[M] =
			new NamedSimplestMold(name, split, melt)

		/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
		  * The functions are used directly to implement their eponymous methods.
		  * They must not throw any exceptions, and instead return
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
		def opt[M](advance :(Liquid, Liquid) => Potential[(Liquid, M, Liquid)],
		           append :(Liquid, M) => Potential[Liquid]) :Mold[M] =
			new CustomOptMold(advance, append)

		/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
		  * The functions are used directly to implement their eponymous methods.
		  * They must not throw any exceptions, and instead return
		  * an empty [[net.noresttherein.sugar.vars.Potential Potential]].
		  * This method is fully useful only for molds performing such functions
		  * as checksum calculation and verification - most molds do not need to inspect already parsed/formatted data.
		  * @param name    An arbitrary name of the molded type `M`, used only for debugging purposes.
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
		def opt[M](name :String, advance :(Liquid, Liquid) => Potential[(Liquid, M, Liquid)],
		           append :(Liquid, M) => Potential[Liquid])
				:NamedMold[M] =
			new NamedCustomOptMold(name, advance, append)

		/** The simplest factory method for a `Mold`, accepting functions necessary to implement its abstract methods.
		  * They must not throw any exceptions, and instead return
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
		def opt[M](split :Liquid => Potential[(Liquid, M, Liquid)], melt :M => Potential[Liquid]) :Mold[M] =
			new SimplestOptMold(split, melt)

		/** The simplest factory method for a `Mold`, accepting functions necessary to implement its abstract methods.
		  * They must not throw any exceptions, and instead return
		  * an empty [[net.noresttherein.sugar.vars.Potential Potential]].
		  * @param name    An arbitrary name of the molded type `M`, used only for debugging purposes.
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
		def opt[M](name :String, split :Liquid => Potential[(Liquid, M, Liquid)], melt :M => Potential[Liquid])
				:NamedMold[M] =
			new NamedSimplestOptMold(name, split, melt)

		/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
		  * The functions are used directly to implement their eponymous methods.
		  * They must not throw any exceptions, and instead return
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] instance.
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
		def guard[M](advance :(Liquid, Liquid) => Fallible[(Liquid, M, Liquid)],
		             append :(Liquid, M) => Fallible[Liquid]) :Mold[M] =
			new CustomGuardMold(advance, append)

		/** The most generic `Mold` factory method, using the given functions for parsing and formatting.
		  * The functions are used directly to implement their eponymous methods.
		  * They must not throw any exceptions, and instead return
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] instance.
		  * This method is fully useful only for molds performing such functions
		  * as checksum calculation and verification - most molds do not need to inspect already parsed/formatted data.
		  * @param name    An arbitrary name of the molded type `M`, used only for debugging purposes.
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
		def guard[M](name :String, advance :(Liquid, Liquid) => Fallible[(Liquid, M, Liquid)],
		             append :(Liquid, M) => Fallible[Liquid]) :NamedMold[M] =
			new NamedCustomGuardMold(name, advance, append)

		/** The simplest factory method for a `Mold`, accepting functions necessary to implement its abstract methods.
		  * They must not throw any exceptions, and instead return
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] instance.
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
		def guard[M](split :Liquid => Fallible[(Liquid, M, Liquid)], melt :M => Fallible[Liquid]) :Mold[M] =
			new SimplestGuardMold(split, melt)

		/** The simplest factory method for a `Mold`, accepting functions necessary to implement its abstract methods.
		  * They must not throw any exceptions, and instead return
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] instance.
		  * @param name    An arbitrary name of the molded type `M`, used only for debugging purposes.
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
		def guard[M](name :String, split :Liquid => Fallible[(Liquid, M, Liquid)], melt :M => Fallible[Liquid])
				:NamedMold[M] =
			new NamedSimplestGuardMold(name, split, melt)

		/** A `Mold` mapping the given constant to an empty `liquid`. Always returns `value` when parsing
		  * without consuming any of the input, and always returns
		  * [[net.noresttherein.sugar.format.Format.Liquid.empty empty]] when formatting, ignoring its input.
		  */
		def empty[M](value :M) :Mold[M] = new EmptyMold(value)

		val emptyString :Mold[String] = new EmptyMold("") //not clear what model type would be best

		/** An 'error' mold which throws an [[UnsupportedOperationException]] on every call (or returns
		  * [[net.noresttherein.sugar.vars.Opt.Lack Lack]]/[[net.noresttherein.sugar.vars.Fallible.Failed Failed]]).
		  * If an implicit [[scala.reflect.ClassTag ClassTag]] for the molded type `M` is present,
		  * the (inner) class name of `M` will be used in the mold's `toString` method.
		  */
		def unsupported[M](implicit model :Maybe[ClassTag[M]]) :Mold[M] = model.opt match {
			case Got(tag) => unsupported(tag.runtimeClass.innerName)
			case _        => unsupported("")
		}

		/** An 'error' mold which throws an [[UnsupportedOperationException]] on every call (or returns
		  * [[net.noresttherein.sugar.vars.Opt.Lack Lack]]/[[net.noresttherein.sugar.vars.Fallible.Failed Failed]]).
		  * @param model the molded class, used only by `toString` of the returned mold.
		  */
		def unsupported[M](model :Class[M]) :Mold[M] = unsupported(model.innerName)

		/** An 'error' mold which throws an [[UnsupportedOperationException]] on every call (or returns
		  * [[net.noresttherein.sugar.vars.Opt.Lack Lack]]/[[net.noresttherein.sugar.vars.Fallible.Failed Failed]]).
		  * @param name the name of the molded model,
		  *             used in returned mold's `toString` representation: `s"$format[$name]".`
		  */
		def unsupported[M](name :String) :Mold[M] =
			if (name.length == 0 || name == "_") new UnsupportedMold
			else new NamedUnsupportedMold(name)

		@SerialVersionUID(ver)
		private class UnsupportedMold[S] extends SpecialMold[S] {
			override def advance(prefix :Liquid, suffix :Liquid) = parseError(this, suffix)
			override def advanceOpt(prefix :Liquid, suffix :Liquid) = Lack
			override def guardAdvance(prefix :Liquid, suffix :Liquid) = Failed(() => parseErrorMsg(this, suffix))
			override def append(prefix :Liquid, model :S) = formatError(this, model)
			override def appendOpt(prefix :Liquid, model :S) = Lack
			override def guardAppend(prefix :Liquid, model :S) = Failed(() => formatErrorMsg(this, model))
			override def toString = Format.this.toString + "<unsupported>"
		}
		@SerialVersionUID(ver)
		private class NamedUnsupportedMold[S](override val name :String) extends UnsupportedMold[S] with NamedMold[S]
	}



	//consider: a way of limiting length; the problem with doing it on Mold level is what to do when writing.
	/** A mandatory `String` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val stringMold  :Mold[String]
	/** A mandatory `Char` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val charMold    :Mold[Char]
	/** A mandatory `Long` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val longMold    :Mold[Long]
	/** A mandatory `Int` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val intMold     :Mold[Int]
	/** A mandatory `Short` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val shortMold   :Mold[Short]
	/** A mandatory `Byte` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val byteMold    :Mold[Byte]
	/** A mandatory `Double` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val doubleMold  :Mold[Double]
	/** A mandatory `Float` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val floatMold   :Mold[Float]
	/** A mandatory `Boolean` mold, allowing the type to be used
	  * in a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] definition, to define molds abstracting
	  * over an actual `Format` used.
	  * @see [[net.noresttherein.sugar.format.Format.MoldLayoutMaker]]
	  */
	implicit val booleanMold :Mold[Boolean]
	//todo: molds for java.time types
//		implicit val InstantMold :Mold[Instant]
	/** An implicit `Mold` for type `Nothing`. It has a use in a context where an object to be melted/cast is parsed as an `Option` (or similar type)
	  * and requires an implicit mold. This mold allows to pass `Non` as an argument, satisfying the requirement for the existence of a mold for its contents.
	  */
	implicit val nothingMold :Mold[Nothing] = NothingMold
	/** A `Mold` embedding directly given `Liquid` values in the formatted result.
	  * When parsing, all of the input is automatically consumed, regardless of its contents.
	  */ //fixme: these two may conflict with stringMold
	private[format] def liquidMoldPrototype :Mold[Liquid] = LiquidMold
	/** A `Mold` embedding directly given `Raw` values in the formatted result.
	  * When parsing, all of the input is automatically consumed, regardless of its contents.
	  */
	private[format] def rawMoldPrototype     :Mold[Raw] = RawMold



	private[format] trait MoldDefaults[M] extends Mold[M] {
		override def head(liquid :Liquid) :M = advance(liquid)._2
		override def headOpt(liquid :Liquid) :Opt[M] = advanceOpt(liquid) map (_._2)
		override def guardHead(liquid :Liquid) :Fallible[M] = guardAdvance(liquid) map (_._2)
		override def advance(liquid :Liquid) :(Liquid, M, Liquid) = advance(emptyLiquid, liquid)
		override def advanceOpt(liquid :Liquid) :Opt[(Liquid, M, Liquid)] = advanceOpt(emptyLiquid, liquid)
		override def guardAdvance(liquid :Liquid) :Fallible[(Liquid, M, Liquid)] = guardAdvance(emptyLiquid, liquid)
	}

	/** Base trait for molds which do not need access to data previously parsed/formatted by molds for preceding
	  * properties of a larger enclosing `Mold`. This includes most 'normal' implementations,
	  * as long as they don't use other molds or know they don't need that extra information either.
 	  * @tparam M the molded 'model' type.
	  */
	trait SimpleMold[M] extends MoldDefaults[M] {
		//leaving advance(liquid) family for subclasses and implementing two argument versions instead
		// would make for a simpler interface, but it would have to create a new tuple.
		override def append(prefix :Liquid, model :M) :Liquid = concat(prefix, melt(model))
		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = meltOpt(model).flatMap(concatOpt(prefix, _))
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] =
			guardMelt(model).flatMap(guardConcat(prefix, _))
	}

	/** Base trait for molds which need to access data previously parsed/formatted by molds for preceding properties
	  * of a larger enclosing `Mold`. These are special purpose molds implementing features such as calculating
	  * and validating checksums. In general, molds using other arbitrary molds will usually also implement this trait
	  * in order to pass full information to said molds.
	  */
	trait SpecialMold[M] extends MoldDefaults[M] {
		override def melt(model :M) :Liquid = append(emptyLiquid, model)
		override def meltOpt(model :M) :Opt[Liquid] = appendOpt(emptyLiquid, model)
		override def guardMelt(model :M) :Fallible[Liquid] = guardAppend(emptyLiquid, model)
	}



	/** Base trait for `Mold` implementations leaving for subclasses to implement only 'basic' methods returning
	  * direct results and report errors by throwing an exception.
	  * Leaves to implement the overloaded [[net.noresttherein.sugar.format.Format.Mold.advance advance]] methods,
	  * [[net.noresttherein.sugar.format.Format.Mold.append append]]
	  * and [[net.noresttherein.sugar.format.Format.Mold.melt melt]].
	  * Implementations of other methods are derived from the former.
	  */ //todo: SimpleExcetionBasedMold, etc. - (advance(liquid :Liquid) + melt(model :M))
	trait ThrowingMold[M] extends Mold[M] {
		override def castOpt(liquid :Liquid) :Opt[M] =
			try { Got(cast(liquid)) } catch {
				case _ :Exception => Lack
			}
		override def guardCast(liquid :Liquid) :Fallible[M] =
			try { Passed(cast(liquid)) } catch {
				case e :Exception => Failed(e)
			}
		override def headOpt(liquid :Liquid) :Opt[M] =
			try { Got(head(liquid)) } catch {
				case _ :Exception => Lack
			}
		override def guardHead(liquid :Liquid) :Fallible[M] =
			try { Passed(head(liquid)) } catch {
				case e :Exception => Failed(e)
			}
		override def guardNext(liquid :Liquid) :Fallible[(M, Liquid)] =
			try { Passed(next(liquid)) } catch {
				case e :Exception => Failed(e)
			}
		override def nextOpt(liquid :Liquid) :Opt[(M, Liquid)] =
			try { Got(next(liquid)) } catch {
				case _ :Exception => Lack
			}
		override def advanceOpt(liquid :Liquid) :Opt[(Liquid, M, Liquid)] =
			try { Got(advance(liquid)) } catch {
				case _ :Exception => Lack
			}
		override def guardAdvance(liquid :Liquid) :Fallible[(Liquid, M, Liquid)] =
			try { Passed(advance(liquid)) } catch {
				case e :Exception => Failed(e)
			}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
			try { Got(advance(prefix, suffix)) } catch {
				case _ :Exception => Lack
			}
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
			try { Passed(advance(prefix, suffix)) } catch {
				case e :Exception => Failed(e)
			}
		override def meltOpt(model :M) :Opt[Liquid] =
			try { Got(melt(model)) } catch {
				case _ :Exception => Lack
			}
		override def guardMelt(model :M) :Fallible[Liquid] =
			try { Passed(melt(model)) } catch {
				case e :Exception => Failed(e)
			}
		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] =
			try { Got(append(prefix, model)) } catch {
				case _ :Exception => Lack
			}
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] =
			try { Passed(append(prefix, model)) } catch {
				case e :Exception => Failed(e)
			}
	}

	/** A base trait for [[net.noresttherein.sugar.format.Format.Mold molds]] which do not need access
	  * to preceding parsed/formatted data and are based on methods indicating errors by throwing
	  * a [[net.noresttherein.sugar.format.FormatException FormatException]].
	  * Leaves to implement only two abstract methods:
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.advance(liquid:Liquid) advance]]`(liquid :Liquid)`
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.melt melt]]`(model :M)`
	  * @see [[net.noresttherein.sugar.format.Format.SimpleGuardingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialThrowingMold]]
	  */
	trait SimpleThrowingMold[M] extends SimpleMold[M] with ThrowingMold[M]

	/** A base trait for [[net.noresttherein.sugar.format.Format.Mold molds]] based on methods reporting errors
	  * by throwing a [[net.noresttherein.sugar.format.FormatException FormatException]] and which need access
	  * to data parsed or formatted by molds for preceding properties when nested under a larger mold.
	  * Leaves to implement only two abstract methods:
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.advance(prefix:Liquid,suffix:Liquid) advance]]`(prefix :Liquid, suffix :Liquid)`
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.append append]]`(prefix :Liquid, model :M)`
	  * @see [[net.noresttherein.sugar.format.Format.SimpleThrowingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialGuardingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialOptBasedMold]]
	  */
	trait SpecialThrowingMold[M] extends SpecialMold[M] with ThrowingMold[M]



	/** Base trait for `Mold` implementations leaving only 'opt' methods
	  * returning an [[net.noresttherein.sugar.vars.Opt Opt]]
	  * ([[net.noresttherein.sugar.format.Format.Mold.advanceOpt advanceOpt]],
	  * [[net.noresttherein.sugar.format.Format.Mold.appendOpt appendOpt]]
	  * and [[net.noresttherein.sugar.format.Format.Mold.meltOpt meltOpt]]) for subclasses to implement.
	  * Implementations of other methods are derived from the former.
	  * Extending trait [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold SimpleOptBasedMold]]
	  * is the best base trait choice for stand alone molds (not relying on other molds) which do not need
	  * very detailed error reporting.
	  */
	trait OptBasedMold[M] extends Mold[M] {
		override def cast(liquid :Liquid) :M = parse(castOpt(liquid), liquid)
		override def guardCast(liquid :Liquid) :Fallible[M] = parseGuard(castOpt(liquid), liquid)
		override def head(liquid :Liquid) :M = parse(headOpt(liquid), liquid)
		override def guardHead(liquid :Liquid) :Fallible[M] = parseGuard(headOpt(liquid), liquid)
		override def next(liquid :Liquid) :(M, Liquid) = parse(nextOpt(liquid), liquid)
		override def guardNext(liquid :Liquid) :Fallible[(M, Liquid)] = parseGuard(nextOpt(liquid), liquid)
		override def advance(liquid :Liquid) :(Liquid, M, Liquid) = parse(advanceOpt(liquid), liquid)
		override def guardAdvance(liquid :Liquid) :Fallible[(Liquid, M, Liquid)] = parseGuard(advanceOpt(liquid), liquid)
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) =
			parse(advanceOpt(prefix, suffix), suffix)
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
			parseGuard(advanceOpt(prefix, suffix), suffix)

		override def append(prefix :Liquid, model :M) :Liquid = format(appendOpt(prefix, model), model)
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] =
			formatGuard(appendOpt(prefix, model), model)
		override def melt(model :M) :Liquid = format(meltOpt(model), model)
		override def guardMelt(model :M) :Fallible[Liquid] = formatGuard(meltOpt(model), model)

		private def parseGuard[X](result :Opt[X], input :Liquid) = result match {
			case Got(res) => Passed(res)
			case _ => Failed(() => parseErrorMsg(this, input))
		}
		private def formatGuard[X](result :Opt[Liquid], model :M) = result match {
			case Got(res) => Passed(res)
			case _ => Failed(() => formatErrorMsg(this, model))
		}
		private def parse[X](result :Opt[X], input :Liquid) :X = result match {
			case Got(res) => res
			case _ => parseError(this, input)
		}
		private def format(result :Opt[Liquid], model :M) :Liquid = result match {
			case Got(res) => res
			case _ => formatError(this, model)
		}
	}

	/** Base trait for molds which do not need access to preceding parsed/formatted data and are based on methods
	  * returning results as an `Opt` without additional error information.
	  * Leaves to implement only two abstract methods:
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.advanceOpt(liquid:Liquid) advanceOpt]]`(liquid :Liquid)`
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.meltOpt meltOpt]]`(model :M)`
	  * @see [[net.noresttherein.sugar.format.Format.SimpleThrowingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SimpleGuardingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialOptBasedMold]]
	  */
	trait SimpleOptBasedMold[M] extends SimpleMold[M] with OptBasedMold[M]

	/** Base trait for [[net.noresttherein.sugar.format.Format.Mold molds]] based on methods returning an `Opt`
	  * and which need access to data previously parsed/formatted by molds for preceding properties of a mold
	  * for a larger model including this mold.
	  * Leaves to implement only two abstract methods:
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.advanceOpt(prefix:Liquid,suffix:Liquid) advanceOpt]]`(prefix :Liquid, suffix :Liquid)`
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.appendOpt appendOpt]]`(prefix :Liquid, model :M)`
	  * @see [[net.noresttherein.sugar.format.Format.SpecialThrowingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialGuardingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold]]
	  */
	trait SpecialOptBasedMold[M] extends SpecialMold[M] with OptBasedMold[M]



	/** Base trait for `Mold` implementations which leaves only 'guard' methods
	  * returning a [[net.noresttherein.sugar.vars.Fallible Fallible]]
	  * ([[net.noresttherein.sugar.format.Format.Mold.guardAdvance guardAdvance]] and
	  * [[net.noresttherein.sugar.format.Format.Mold.guardAppend guardAppend]]) for subclasses to implement.
	  * Implementations of other methods are derived from the former.
	  */
	trait GuardingMold[M] extends Mold[M] {
		override def cast(liquid :Liquid) :M = parse(guardCast(liquid), liquid)
		override def head(liquid :Liquid) :M = parse(guardHead(liquid), liquid)
		override def next(liquid :Liquid) :(M, Liquid) = parse(guardNext(liquid), liquid)
		override def advance(liquid :Liquid) :(Liquid, M, Liquid) = parse(guardAdvance(liquid), liquid)
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) =
			parse(guardAdvance(prefix, suffix), suffix)

		override def append(prefix :Liquid, model :M) :Liquid = format(guardAppend(prefix, model), model)
		override def melt(model :M) :Liquid = format(guardMelt(model), model)

		override def castOpt(liquid :Liquid) :Opt[M] = guardCast(liquid).toOpt
		override def headOpt(liquid :Liquid) :Opt[M] = guardHead(liquid).toOpt
		override def nextOpt(liquid :Liquid) :Opt[(M, Liquid)] = guardNext(liquid).toOpt
		override def advanceOpt(liquid :Liquid) :Opt[(Liquid, M, Liquid)] = guardAdvance(liquid).toOpt
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
			guardAdvance(prefix, suffix).toOpt

		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = guardAppend(prefix, model).toOpt
		override def meltOpt(model :M) :Opt[Liquid] = guardMelt(model).toOpt

		private def parse[X](result :Fallible[X], input :Liquid) = result match {
			case Passed(res) => res
			case Failed(msg) => throw ParsingException(Format.this :Format.this.type)(input, msg)
		}
		private def format[X](result :Fallible[Liquid], model :X) = result match {
			case Passed(res) => res
			case Failed(msg) => throw new FormattingException(Format.this, model, msg)
		}
	}

	/** Base trait for [[net.noresttherein.sugar.format.Format.Mold molds]] which do not need access
	  * to preceding parsed/formatted data and which are based on methods reporting errors
	  * with a [[net.noresttherein.sugar.vars.Fallible Fallible]].
	  * Leaves to implement only two abstract methods:
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.guardAdvance(liquid:Liquid) guardAdvance]]`(liquid :Liquid)`
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.guardMelt guardMelt]]`(model :M)`
	  * @see [[net.noresttherein.sugar.format.Format.SimpleThrowingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SimpleOptBasedMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialGuardingMold]]
	  */
	trait SimpleGuardingMold[M] extends SimpleMold[M] with GuardingMold[M]

	/** Base trait for [[net.noresttherein.sugar.format.Format.Mold molds]] based on methods reporting errors
	  * with a [[net.noresttherein.sugar.vars.Fallible Fallible]] and which need access to data
	  * previously parsed/formatted by molds for preceding properties of a mold for a larger model including this mold.
	  * Leaves to implement only two abstract methods:
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.advanceOpt(prefix:Liquid,suffix:Liquid) advanceOpt]]`(prefix :Liquid, suffix :Liquid)`
	  *   1. [[net.noresttherein.sugar.format.Format.Mold.appendOpt appendOpt]]`(prefix :Liquid, model :M)`
	  * @see [[net.noresttherein.sugar.format.Format.SpecialThrowingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialGuardingMold]]
	  * @see [[net.noresttherein.sugar.format.Format.SpecialOptBasedMold]]
	  */
	trait SpecialGuardingMold[M] extends SpecialMold[M] with GuardingMold[M]



	/** A `Mold` with a name representing the modeled class, used solely in `toString`,
	  * showing itself as `s"$format[$name]"`.
	  * Helps with error messages included in thrown exceptions
	  * and returned [[net.noresttherein.sugar.vars.Fallible Fallible]].
	  * While the [[net.noresttherein.sugar.format.Format.NamedMold.name name]] property is not normally
	  * used by the mold directly, [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] instances
	  * are most often initialized with the name of the modeled model type, potentially used in the format
	  * of the latter, and they will create `NamedMold` instances initialized with the same name.
	  */
	trait NamedMold[M] extends Mold[M] {
		def name :String
		override def mirror :NamedMold[(Liquid, M)] = new NamedMirrorMold(this)
		override def potential :NamedMold[Potential[M]] = new NamedPotentialMold(this)
		override def opt :NamedMold[Opt[M]] = new NamedOptMold(this)
		override def option :NamedMold[Option[M]] = new NamedOptionMold(this)
		override def guard :NamedMold[Fallible[M]] = new NamedFallibleMold(this)

		override def map[O](read :M => O, write :O => M) :Mold[O] =
			new NamedMappedMold(this, read, write)
		override def optMap[O](read :M => Potential[O], write :O => Potential[M]) :Mold[O] =
			new NamedOptMappedMold(this, read, write)

		override def toString :String = Format.this.toString + "[" + name + "]"
	}

	private abstract class SpecialNamedMold[M](override val name :String) extends SpecialMold[M] with NamedMold[M]


	/** A `Mold` being given a name after a fact. Works as a one-to-one proxy;
	  * only `toString` is enhanced by usage of the modeled entity name.
	  */
	@SerialVersionUID(ver)
	private class RenamedMold[M](override val name :String, mold :Mold[M]) extends NamedMold[M] {
		override def cast(liquid :Liquid) = mold.cast(liquid)
		override def castOpt(liquid :Liquid) = mold.castOpt(liquid)
		override def guardCast(liquid :Liquid) = mold.guardCast(liquid)
		override def head(liquid :Liquid) = mold.head(liquid)
		override def headOpt(liquid :Liquid) = mold.headOpt(liquid)
		override def guardHead(liquid :Liquid) = mold.guardHead(liquid)
		override def next(liquid :Liquid) = mold.next(liquid)
		override def nextOpt(liquid :Liquid) = mold.nextOpt(liquid)
		override def guardNext(liquid :Liquid) = mold.guardNext(liquid)
		override def advance(liquid :Liquid) = mold.advance(liquid)
		override def advanceOpt(liquid :Liquid) = mold.advanceOpt(liquid)
		override def guardAdvance(liquid :Liquid) = mold.guardAdvance(liquid)
		override def advance(prefix :Liquid, suffix :Liquid) = mold.advance(prefix, suffix)
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = mold.advanceOpt(prefix, suffix)
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = mold.guardAdvance(prefix, suffix)

		override def advance(constant :M)(prefix :Liquid, suffix :Liquid) = mold.advance(constant)(prefix, suffix)
		override def advanceOpt(constant :M)(prefix :Liquid, suffix :Liquid) = mold.advanceOpt(constant)(prefix, suffix)
		override def guardAdvance(constant :M)(prefix :Liquid, suffix :Liquid) =
			mold.guardAdvance(constant)(prefix, suffix)

		override def append(prefix :Liquid, model :M) = mold.append(prefix, model)
		override def appendOpt(prefix :Liquid, model :M) = mold.appendOpt(prefix, model)
		override def guardAppend(prefix :Liquid, model :M) = mold.guardAppend(prefix, model)
		override def melt(model :M) = mold.melt(model)
		override def meltOpt(model :M) = mold.meltOpt(model)
		override def guardMelt(model :M) = mold.guardMelt(model)

		override def mirror = new NamedMirrorMold(name, mold)
		override def potential = new NamedPotentialMold(name, mold)
		override def opt = new NamedOptMold(name, mold)
		override def option = new NamedOptionMold(name, mold)

		override def map[O](read :M => O, write :O => M) :NamedMold[O] = new NamedMappedMold(name, mold, read, write)
		override def optMap[O](read :M => Potential[O], write :O => Potential[M]) :NamedMold[O] =
			new NamedOptMappedMold(name, mold, read, write)
	}


	//consider: We could put all these private molds in a package protected class (with this format as an argument)
	// and assign it to a private field here. This would move a considerable part of implementation outside,
	// leading to a smaller file.

	private trait ReadOnlyMold[M] extends Mold[M] {
		override def melt(model :M) :Liquid = emptyLiquid
		override def meltOpt(model :M) :Opt[Liquid] = Got(emptyLiquid)
		override def guardMelt(model :M) :Fallible[Liquid] = Passed(emptyLiquid)
		override def append(prefix :Liquid, model :M) :Liquid = prefix
		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = Got(prefix)
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] = Passed(prefix)
	}
	private trait SafeCastingMold[M] extends Mold[M] {
		override def headOpt(liquid :Liquid) :Opt[M] = Got(head(liquid))
		override def guardHead(liquid :Liquid) :Fallible[M] = Passed(head(liquid))
		override def nextOpt(liquid :Liquid) :Opt[(M, Liquid)] = Got(next(liquid))
		override def guardNext(liquid :Liquid) :Fallible[(M, Liquid)] = Passed(next(liquid))
		override def advance(liquid :Liquid) = advance(emptyLiquid, liquid)
		override def advanceOpt(liquid :Liquid) :Opt[(Liquid, M, Liquid)] = Got(advance(liquid))
		override def guardAdvance(liquid :Liquid) :Fallible[(Liquid, M, Liquid)] = Passed(advance(liquid))
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] = Got(advance(prefix, suffix))
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
			Passed(advance(prefix, suffix))
	}
	private trait SafeMeltingMold[M] extends Mold[M] {
		override def meltOpt(model :M) :Opt[Liquid] = Got(melt(model))
		override def guardMelt(model :M) :Fallible[Liquid] = Passed(melt(model))
		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = Got(append(prefix, model))
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] = Passed(append(prefix, model))
	}
	/** A `Mold` which never throws any exceptions - implements 'opt' and 'guard' methods with their basic variants.
	  * This happens with molds which forward parsing/formatting errors to the application in their model type,
	  * instead of automatically propagating them.
	  */
	private trait SafeMold[M] extends SafeCastingMold[M] with SafeMeltingMold[M]

	private trait AbstractEmptyMold[M] extends SafeMold[M] {
		override def cast(liquid :Liquid) :M =
			if (isEmpty(liquid))
				head(liquid)
			else
				throw ParsingException(Format.this :Format.this.type)(
					liquid, toString + " expected empty input, got '" + liquid + "'."
				)
		override def castOpt(liquid :Liquid) :Opt[M] =
			if (isEmpty(liquid)) Got(head(liquid)) else Lack

		override def guardCast(liquid :Liquid) :Fallible[M] =
			if (isEmpty(liquid))
				Passed(head(liquid))
			else
				Failed(() => toString + " expected empty input, got '" + liquid + "'")

		override def next(liquid :Liquid) = (head(liquid), liquid)
		override def advance(prefix :Liquid, suffix :Liquid) = (prefix, head(suffix), suffix)

		override def append(prefix :Liquid, model :M) = prefix
		override def melt(model :M) :Liquid = emptyLiquid
	}

	@SerialVersionUID(ver)
	private class EmptyMold[M](value :M) extends AbstractEmptyMold[M] {
		override def head(liquid :Liquid) = value
		override def toString = Format.this.toString + ".empty(" + value + ")"
	}

	@SerialVersionUID(ver)
	private object NothingMold extends SimpleMold[Nothing] {
		override def advance(prefix :Liquid, suffix :Liquid) :Nothing =
			throw ParsingException(Format.this :Format.this.type)(suffix, "NothingMold of " + Format.this + " cannot be used to parse anything.")
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[Nothing] = Lack
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[Nothing] =
			Failed(() => "NothingMold of " + Format.this + " cannot be used to parse anything.")

		override def melt(model :Nothing) :Liquid =
			throw new FormattingException(Format.this, model, "NothingMold of " + Format.this + " cannot be used to parse anything.")
		override def meltOpt(model :Nothing) :Opt[Liquid] = Lack
		override def guardMelt(model :Nothing) :Fallible[Liquid] =
			Failed(() => "NothingMold of " + Format.this + " cannot be used to parse anything.")

		override def toString :String = Format.this.toString + "[Nothing]"
	}


	private trait AbstractSuffixMold[M] extends Mold[M] {
		override def cast(liquid :Liquid) = head(liquid)
		override def castOpt(liquid :Liquid) = headOpt(liquid)
		override def guardCast(liquid :Liquid) = guardHead(liquid)
		override def next(liquid :Liquid) = (head(liquid), emptyLiquid)
		override def nextOpt(liquid :Liquid) = headOpt(liquid) map ((_, emptyLiquid))
		override def guardNext(liquid :Liquid) = guardHead(liquid) map ((_, emptyLiquid))
		override def advance(prefix :Liquid, suffix :Liquid) = (prefix, head(suffix), emptyLiquid)
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = headOpt(suffix) map ((prefix, _, emptyLiquid))
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = guardHead(suffix) map ((prefix, _, emptyLiquid))
		override def append(prefix :Liquid, model :M) = melt(model)
	}
	@SerialVersionUID(ver)
	private object LiquidMold extends SafeMold[Liquid] with AbstractSuffixMold[Liquid] {
		override def head(liquid :Liquid) :Liquid = liquid
		override def melt(model :Liquid) :Liquid = model
		override def toString :String = Format.this.toString + "[Liquid]"
	}
	@SerialVersionUID(ver)
	private object RawMold extends SimpleMold[Raw] with AbstractSuffixMold[Raw] {
		override def head(liquid :Liquid) :Raw = cool(liquid)
		override def headOpt(liquid :Liquid) :Opt[Raw] = coolOpt(liquid)
		override def guardHead(liquid :Liquid) :Fallible[Raw] = guardCool(liquid)
		override def melt(model :Raw) :Liquid = Format.this.melt(model)
		override def meltOpt(model :Raw) :Opt[Liquid] = Format.this.meltOpt(model)
		override def guardMelt(model :Raw) :Fallible[Liquid] = Format.this.guardMelt(model)
		override def toString :String = Format.this.toString + "[Raw]"
	}



	@SerialVersionUID(ver)
	private class CustomMold[M](parse :(Liquid, Liquid) => (Liquid, M, Liquid), format :(Liquid, M) => Liquid)
		extends SpecialThrowingMold[M]
	{
		override def advance(prefix :Liquid, suffix :Liquid) = parse(prefix, suffix)
		override def append(prefix :Liquid, model :M) = format(prefix, model)
		override def toString = Format.this.toString + "[_]@"+ this.hashCodeString
	}

	@SerialVersionUID(ver)
	private class NamedCustomMold[M](override val name :String,
	                                 parse :(Liquid, Liquid) => (Liquid, M, Liquid), format :(Liquid, M) => Liquid)
	   extends CustomMold[M](parse, format) with NamedMold[M]

	@SerialVersionUID(ver)
	private class CustomOptMold[M](parse :(Liquid, Liquid) => Potential[(Liquid, M, Liquid)],
	                               format :(Liquid, M) => Potential[Liquid])
		extends SpecialOptBasedMold[M]
	{
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = parse(prefix, suffix)
		override def appendOpt(prefix :Liquid, model :M) = format(prefix, model)
		override def toString = Format.this.toString + "[_]@" + this.hashCodeString
	}

	@SerialVersionUID(ver)
	private class NamedCustomOptMold[M](override val name :String,
	                                    parse :(Liquid, Liquid) => Potential[(Liquid, M, Liquid)],
	                                    format :(Liquid, M) => Potential[Liquid])
		extends CustomOptMold[M](parse, format) with NamedMold[M]

	@SerialVersionUID(ver)
	private class CustomGuardMold[M](parse :(Liquid, Liquid) => Fallible[(Liquid, M, Liquid)],
	                                 format :(Liquid, M) => Fallible[Liquid])
		extends SpecialGuardingMold[M]
	{
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = parse(prefix, suffix)
		override def guardAppend(prefix :Liquid, model :M) = format(prefix, model)
		override def toString = Format.this.toString + "[_]@" + this.hashCodeString
	}

	@SerialVersionUID(ver)
	private class NamedCustomGuardMold[M](override val name :String,
	                                    parse :(Liquid, Liquid) => Fallible[(Liquid, M, Liquid)],
	                                    format :(Liquid, M) => Fallible[Liquid])
		extends CustomGuardMold[M](parse, format) with NamedMold[M]

	@SerialVersionUID(ver)
	private class SimplestMold[M](parse :Liquid => (Liquid, M, Liquid), format :M => Liquid)
		extends SimpleThrowingMold[M]
	{
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) = {
			val (parsed, model, remainder) = parse(suffix)
			(concat(prefix, parsed), model, remainder)
		}
		override def melt(model :M) = format(model)
		override def toString = Format.this.toString + "[_]@" + this.hashCodeString
	}

	@SerialVersionUID(ver)
	private class NamedSimplestMold[M](override val name :String,
	                                   parse :Liquid => (Liquid, M, Liquid), format :M => Liquid)
		extends SimplestMold[M](parse, format) with NamedMold[M]

	@SerialVersionUID(ver)
	private class SimplestOptMold[M](parse :Liquid => Potential[(Liquid, M, Liquid)], format :M => Potential[Liquid])
		extends SimpleOptBasedMold[M]
	{
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = parse(suffix) match {
			case Existent((parsed, model, remainder)) => concatOpt(prefix, parsed) match {
				case Got(done) => Got((done, model, remainder))
				case _ => Lack
			}
			case _ => Lack
		}
		override def meltOpt(model :M) = format(model)
		override def toString = Format.this.toString + "[_]@" + this.hashCodeString
	}

	@SerialVersionUID(ver)
	private class NamedSimplestOptMold[M](override val name :String,
		                                  parse :Liquid => Potential[(Liquid, M, Liquid)], format :M => Potential[Liquid])
		extends SimplestOptMold[M](parse, format) with NamedMold[M]

	@SerialVersionUID(ver)
	private class SimplestGuardMold[M](parse :Liquid => Fallible[(Liquid, M, Liquid)], format :M => Fallible[Liquid])
		extends SimpleGuardingMold[M]
	{
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = parse(suffix) match {
			case Passed((parsed, model, remainder)) => guardConcat(prefix, parsed) match {
				case Passed(done) => Passed((done, model, remainder))
				case fail :Failed => fail
			}
			case fail => fail
		}
		override def guardMelt(model :M) = format(model)
		override def toString = Format.this.toString + "[_]@" + this.hashCodeString
	}

	@SerialVersionUID(ver)
	private class NamedSimplestGuardMold[M](override val name :String,
		                                    parse :Liquid => Fallible[(Liquid, M, Liquid)], format :M => Fallible[Liquid])
		extends SimplestGuardMold[M](parse, format) with NamedMold[M]



	@SerialVersionUID(ver)
	private class PotentialMold[M](mold :Mold[M]) extends SpecialMold[Potential[M]] {
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, Potential[M], Liquid) =
			mold.advanceOpt(prefix, suffix) match {
				case Got((parsed, model, rem)) => (parsed, Existent(model), rem)
				case _ => (prefix, Inexistent, suffix)
			}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, Potential[M], Liquid)] =
			Got(advance(prefix, suffix))
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, Potential[M], Liquid)] =
			Passed(advance(prefix, suffix))

		override def append(prefix :Liquid, model :Potential[M]) :Liquid = model match {
			case Existent(o) => mold.append(prefix, o)
			case _ => prefix
		}
		override def appendOpt(prefix :Liquid, model :Potential[M]) :Opt[Liquid] = model match {
			case Existent(o) => mold.appendOpt(prefix, o)
			case _ => Got(prefix)
		}
		override def guardAppend(prefix :Liquid, model :Potential[M]) :Fallible[Liquid] = model match {
			case Existent(o) => mold.guardAppend(prefix, o)
			case _ => Passed(prefix)
		}
		override def toString :String = mold.toString + ".potential"
	}

	@SerialVersionUID(ver)
	private class NamedPotentialMold[M](moldName :String, mold :Mold[M])
		extends PotentialMold(mold) with NamedMold[Potential[M]]
	{
		def this(mold :NamedMold[M]) = this(mold.name, mold)
		override def name :String = "Potential[" + moldName + "]"
	}

	private abstract class OptMoldBase[S, T] extends SpecialMold[S] {
		implicit val lift :S => Opt[T]
		final override def append(prefix :Liquid, model :S)      = appendValue(prefix, model)
		final override def appendOpt(prefix :Liquid, model :S)   = appendValueOpt(prefix, model)
		final override def guardAppend(prefix :Liquid, model :S) = appendValueGuard(prefix, model)
		final override def melt(model :S) :Liquid = meltValue(model)
		final override def meltOpt(model :S) :Opt[Liquid] = unapplyValue(model)
		final override def guardMelt(model :S) :Fallible[Liquid] = meltValueGuard(model)
		protected def appendValue(prefix :Liquid, suffix :Opt[T]) :Liquid
		protected def appendValueOpt(prefix :Liquid, suffix :Opt[T]) :Opt[Liquid]
		protected def appendValueGuard(prefix :Liquid, suffix :Opt[T]) :Fallible[Liquid]
		protected def meltValue(model :Opt[T]) :Liquid = appendValue(emptyLiquid, model)
		protected def unapplyValue(model :Opt[T]) :Opt[Liquid] = appendValueOpt(emptyLiquid, model)
		protected def meltValueGuard(model :Opt[T]) :Fallible[Liquid] = appendValueGuard(emptyLiquid, model)
	}

	@SerialVersionUID(ver)
	private class OptMold[M](mold :Mold[M]) extends OptMoldBase[Opt[M], M] {
		implicit final override val lift = identity[Opt[M]]
		override def advance(prefix :Liquid, suffix :Liquid) =
			mold.advanceOpt(prefix, suffix) match {
				case Got((parsed, model, rem)) => (parsed, Got(model), rem)
				case _ => (prefix, Lack, suffix)
			}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = Got(advance(prefix, suffix))
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = Passed(advance(prefix, suffix))
		override def appendValue(prefix :Liquid, model :Opt[M]) = model match {
			case Got(o) => mold.append(prefix, o)
			case _ => prefix
		}
		override def appendValueOpt(prefix :Liquid, model :Opt[M]) = model match {
			case Got(o) => mold.appendOpt(prefix, o)
			case _ => Got(prefix)
		}
		override def appendValueGuard(prefix :Liquid, model :Opt[M]) = model match {
			case Got(o) => mold.guardAppend(prefix, o)
			case _ => Passed(prefix)
		}
		override def toString = mold.toString + ".opt"
	}

	@SerialVersionUID(ver)
	private class NamedOptMold[M](moldName :String, mold :Mold[M])
		extends OptMold[M](mold) with NamedMold[Opt[M]]
	{
		def this(mold :NamedMold[M]) = this(mold.name, mold)
		override def name :String = "Opt[" + moldName + "]"
	}

	@SerialVersionUID(ver)
	private class OptionMold[M](mold :Mold[M]) extends SpecialMold[Option[M]] {
		override def advance(prefix :Liquid, suffix :Liquid) =
			mold.advanceOpt(prefix, suffix) match {
				case Got((parsed, model, rem)) => (parsed, Some(model), rem)
				case _ => (prefix, None, suffix)
			}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = Got(advance(prefix, suffix))
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = Passed(advance(prefix, suffix))

		override def append(prefix :Liquid, model :Option[M]) = model match {
			case Some(o) => mold.append(prefix, o)
			case _ => prefix
		}
		override def appendOpt(prefix :Liquid, model :Option[M]) = model match {
			case Some(o) => mold.appendOpt(prefix, o)
			case _ => Got(prefix)
		}
		override def guardAppend(prefix :Liquid, model :Option[M]) = model match {
			case Some(o) => mold.guardAppend(prefix, o)
			case _ => Passed(prefix)
		}
		override def toString = mold.toString + ".option"
	}

	@SerialVersionUID(ver)
	private class NamedOptionMold[M](moldName :String, mold :Mold[M])
		extends OptionMold[M](mold) with NamedMold[Option[M]]
	{
		def this(mold :NamedMold[M]) = this(mold.name, mold)
		override def name :String = "Option[" + moldName + "]"
	}

	@SerialVersionUID(ver)
	private class FallibleMold[M](mold :Mold[M]) extends SpecialMold[Fallible[M]] {
		override def advance(prefix :Liquid, suffix :Liquid) =
			mold.guardAdvance(prefix, suffix) match {
				case Passed((parsed, model, unparsed)) => (parsed, Passed(model), unparsed)
				case fail :Failed => (prefix, fail, suffix)
			}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = Got(advance(prefix, suffix))
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = Passed(advance(prefix, suffix))

		override def append(prefix :Liquid, model :Fallible[M]) = model match {
			case Passed(f) => mold.append(prefix, f)
			case _ => prefix
		}
		override def appendOpt(prefix :Liquid, model :Fallible[M]) = model match {
			case Passed(f) => mold.appendOpt(prefix, f)
			case _ => Got(prefix)
		}
		override def guardAppend(prefix :Liquid, model :Fallible[M]) = model match {
			case Passed(f) => mold.guardAppend(prefix, f)
			case _ => Passed(prefix)
		}
		override def toString = mold.toString + ".fallible"
	}

	@SerialVersionUID(ver)
	private class NamedFallibleMold[M](moldName :String, mold :Mold[M])
		extends FallibleMold[M](mold) with NamedMold[Fallible[M]]
	{
		def this(mold :NamedMold[M]) = this(mold.name, mold)
		override def name :String = "Fallible[" + moldName + "]"
	}



	@SerialVersionUID(ver)
	private class MappedMold[X, Y](mold :Mold[X], read :X => Y, write :Y => X) extends SpecialMold[Y] {
		private def guardRead(x :X) =
			try { Passed(read(x)) } catch {
				case e :Exception => Failed(() => "Failed to map " + x + " from " + mold + ": " + e + ".")
			}
		private def optRead(x :X) =
			try { Got(read(x)) } catch {
				case _ :Exception => Lack
			}
		override def cast(liquid :Liquid) = read(mold.cast(liquid))
		override def castOpt(liquid :Liquid) = mold.castOpt(liquid) flatMap optRead
		override def guardCast(liquid :Liquid) = mold.guardCast(liquid) flatMap guardRead
		override def head(liquid :Liquid) = read(mold.head(liquid))
		override def headOpt(liquid :Liquid) = mold.headOpt(liquid) flatMap optRead
		override def guardHead(liquid :Liquid) = mold.guardHead(liquid) flatMap guardRead
		override def next(liquid :Liquid) = {
			val (_, x, unparsed) = mold.advance(liquid)
			(read(x), unparsed)
		}
		override def nextOpt(liquid :Liquid) = mold.advanceOpt(liquid) match {
			case Got((_, x, unparsed)) => optRead(x) map ((_, unparsed))
			case _ => Lack
		}
		override def guardNext(liquid :Liquid) = mold.guardAdvance(liquid) match {
			case Passed((_, x, unparsed)) => guardRead(x) map ((_, unparsed))
			case fail :Failed => fail
		}
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, Y, Liquid) = {
			val (parsed, x, rem) = mold.advance(prefix, suffix)
			(parsed, read(x), rem)
		}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) =
			mold.advanceOpt(prefix, suffix) match {
				case Got((parsed, x, unparsed)) => optRead(x) map ((parsed, _, unparsed))
				case _ => Lack
			}
		override def guardAdvance(prefix :Liquid, suffix :Liquid) =
			mold.guardAdvance(prefix, suffix) match {
				case Passed((parsed, x, unparsed)) => guardRead(x) map ((parsed, _, unparsed))
				case fail :Failed => fail
			}

		override def append(prefix :Liquid, model :Y) :Liquid = mold.append(prefix, write(model))
		override def appendOpt(prefix :Liquid, model :Y) :Opt[Liquid] =
			try { mold.appendOpt(prefix, write(model)) } catch {
				case _ :Exception => Lack
			}
		override def guardAppend(prefix :Liquid, model :Y) :Fallible[Liquid] =
			try { mold.guardAppend(prefix, write(model)) } catch {
				case e :Exception => Failed(() => "Failed to map " + model + " for " + mold + ": " + e + ".")
			}
		override def toString :String = mold.toString + ".map"
	}

	@SerialVersionUID(ver)
	private class NamedMappedMold[X, Y](override val name :String, mold :Mold[X], read :X => Y, write :Y => X)
		extends MappedMold[X, Y](mold, read, write) with NamedMold[Y]
	{
		def this(mold :NamedMold[X], read :X => Y, write :Y => X) =
			this("map(" + mold.name + ")", mold, read, write)
	}

	@SerialVersionUID(ver)
	private class OptMappedMold[X, Y](mold :Mold[X], read :X => Potential[Y],
	                                  write :Y => Potential[X])
		extends SpecialMold[Y]
	{
		private def forceRead(liquid :Liquid, x :X) = read(x) match {
			case Existent(res) => res
			case _ =>
				throw ParsingException(Format.this :Format.this.type)(liquid, "Failed to map " + x + " from " + mold + ".")
		}
		private def forceWrite(y :Y) = write(y) match {
			case Existent(res) => res
			case _ => throw new FormattingException(Format.this, y, "Failed to map " + y + " for " + mold + ".")
		}
		private def readFail(x :X)  = Failed(() => "Could not map " + x + " from " + mold + ".")
		private def guardRead(x :X) = read(x) match {
			case Existent(y) => Passed(y)
			case _           => readFail(x)
		}
		override def cast(liquid :Liquid) :Y = forceRead(liquid, mold.cast(liquid))
		override def castOpt(liquid :Liquid) = mold.castOpt(liquid).toPotential.flatMap(read)
		override def guardCast(liquid :Liquid) = mold.guardCast(liquid) flatMap guardRead
		override def head(liquid :Liquid) :Y = forceRead(liquid, mold.head(liquid))
		override def headOpt(liquid :Liquid) = mold.headOpt(liquid).toPotential.flatMap(read)
		override def guardHead(liquid :Liquid) = mold.guardHead(liquid) flatMap guardRead
		override def next(liquid :Liquid) = {
			val (_, x, rem) = mold.advance(liquid)
			(forceRead(liquid, x), rem)
		}
		override def nextOpt(liquid :Liquid) = mold.advanceOpt(liquid) match {
			case Got((_, x, rem)) => read(x).map((_, rem))
			case _ => Lack
		}
		override def guardNext(liquid :Liquid) = mold.guardAdvance(liquid) match {
			case Passed((_, x, rem)) => read(x) match {
				case Existent(model) => Passed((model, rem))
				case _               => readFail(x)
			}
		}
		override def advance(prefix :Liquid, suffix :Liquid) = {
			val (parsed, x, unparsed) = mold.advance(prefix, suffix)
			(parsed, forceRead(suffix, x), unparsed)
		}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) = mold.advanceOpt(prefix, suffix) match {
			case Got((parsed, x, unparsed)) => read(x).map((parsed, _, unparsed))
			case _ => Lack
		}
		override def guardAdvance(prefix :Liquid, suffix :Liquid) = mold.guardAdvance(prefix, suffix) match {
			case Passed((parsed, x, unparsed)) => read(x) match {
				case Existent(model) => Passed((parsed, model, unparsed))
				case _               => readFail(x)
			}
			case fail :Failed => fail
		}

		override def append(prefix :Liquid, model :Y) :Liquid = mold.append(prefix, forceWrite(model))
		override def appendOpt(prefix :Liquid, model :Y) = write(model) match {
			case Existent(x) => mold.appendOpt(prefix, x)
			case _ => Lack
		}
		override def guardAppend(prefix :Liquid, model :Y) = write(model) match {
			case Existent(x) => mold.guardAppend(prefix, x)
			case _           => Failed(() => "Could not map " + model + " for " + mold + ".")
		}
		override def toString = mold.toString + ".optMap"
	}

	@SerialVersionUID(ver)
	private class NamedOptMappedMold[X, Y](override val name :String, mold :Mold[X],
	                                       read :X => Potential[Y], write :Y => Potential[X])
		extends OptMappedMold[X, Y](mold, read, write) with NamedMold[Y]
	{
		def this(mold :NamedMold[X], read :X => Potential[Y], write :Y => Potential[X]) =
			this("map(" + mold.name+ ")", mold, read, write)
	}



	@SerialVersionUID(ver)
	private class MirrorMold[M](mold :Mold[M]) extends SpecialMold[(Liquid, M)] {
		override def cast(liquid :Liquid) :(Liquid, M) = (liquid, mold.cast(liquid))
		override def castOpt(liquid :Liquid) :Opt[(Liquid, M)] = mold.castOpt(liquid) match {
			case Got(o) => Got((liquid, o))
			case _ => Lack
		}
		override def guardCast(liquid :Liquid) :Fallible[(Liquid, M)] = mold.guardCast(liquid) match {
			case Passed(o) => Passed((liquid, o))
			case fail :Failed => fail
		}
		override def head(liquid :Liquid) :(Liquid, M) = {
			val (parsed, model, _) = mold.advance(liquid)
			(parsed, model)
		}
		override def headOpt(liquid :Liquid) :Opt[(Liquid, M)] =
			mold.advanceOpt(liquid).map(res => (res._1, res._2))
		override def guardHead(liquid :Liquid) :Fallible[(Liquid, M)] =
			mold.guardAdvance(liquid).map(res => (res._1, res._2))

		override def next(liquid :Liquid) :((Liquid, M), Liquid) = {
			val (parsed, model, rem) = mold.advance(liquid)
			((parsed, model), rem)
		}
		override def nextOpt(liquid :Liquid) :Opt[((Liquid, M), Liquid)] =
			mold.advanceOpt(liquid).map(res => ((res._1, res._2), res._3))
		override def guardNext(liquid :Liquid) :Fallible[((Liquid, M), Liquid)] =
			mold.guardAdvance(liquid).map(res => ((res._1, res._2), res._3))

		//fixme: This prevents mold from accessing the prefix, leading to potentially invalid/inconsistent results.
		//  We need a dedicated method mirror(prefix :Liquid, suffix :Liquid) :(Liquid, (Liquid, S), Liquid)
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, (Liquid, M), Liquid) = {
			val (parsed, model, rem) = mold.advance(suffix)
			(concat(prefix, parsed), (parsed, model), rem)
		}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, (Liquid, M), Liquid)] =
			mold.advanceOpt(suffix) match {
				case Got((parsed, model, rem)) => concatOpt(prefix, parsed) match {
					case Got(done) => Got((done, (parsed, model), rem))
					case _ => Lack
				}
				case _ => Lack
			}
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, (Liquid, M), Liquid)] =
			mold.guardAdvance(suffix) match {
				case Passed((parsed, model, rem)) => guardConcat(prefix, parsed) match {
					case Passed(done) => Passed((done, (parsed, model), rem))
					case fail :Failed => fail
				}
				case fail :Failed => fail
			}
		override def append(prefix :Liquid, model :(Liquid, M)) :Liquid = concat(prefix, model._1)
		override def appendOpt(prefix :Liquid, model :(Liquid, M)) :Opt[Liquid] = concatOpt(prefix, model._1)
		override def guardAppend(prefix :Liquid, model :(Liquid, M)) :Fallible[Liquid] = guardConcat(prefix, model._1)

		override def toString :String = mold.toString + ".mirror"
	}

	@SerialVersionUID(ver)
	private class NamedMirrorMold[M](moldName :String, mold :Mold[M])
		extends MirrorMold[M](mold) with NamedMold[(Liquid, M)]
	{
		def this(mold :NamedMold[M]) = this(mold.name, mold)
		override def name = "(" + moldName + ", melted)"
	}



	/** Mold returned by [[net.noresttherein.sugar.format.Format.PropertyPart PropertyPart]]`.`[[net.noresttherein.sugar.format.Format.Part.map map]]. */
	@SerialVersionUID(ver)
	private class MapMold[M, P](override val name :String, partName :String, get :M => P, construct :P => M)
	                           (implicit partMold :Mold[P])
		extends SpecialNamedMold[M](name)
	{
		override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) = {
			val (done, part, rem) = partMold.advance(prefix, suffix)
			(done, construct(part), rem)
		}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
			partMold.advanceOpt(prefix, suffix) match {
				case Got((done, part, rem)) => try {
					Got((done, construct(part), rem))
				} catch {
					case _ :Exception => Lack
				}
				case _ => Lack
			}
		override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
			partMold.guardAdvance(prefix, suffix) match {
				case Passed((done, part, rem)) => try {
					Passed((done, construct(part), rem))
				} catch {
					case e :Exception => Failed(e)
				}
				case fail :Failed => fail
			}
		override def append(prefix :Liquid, model :M) :Liquid = partMold.append(prefix, get(model))
		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = partMold.appendOpt(prefix, get(model))
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] = partMold.guardAppend(prefix, get(model))

		override def toString :String = mapMoldString(name, partName)
	}

	/** Mold returned by [[net.noresttherein.sugar.format.Format.PropertyPart PropertyPart]]`.`[[net.noresttherein.sugar.format.Format.Part.flatMap flatMap]]. */
	@SerialVersionUID(ver)
	private class FlatMapMold[M, P](override val name :String, partName :String, get :M => P, construct :P => Mold[M])
	                               (implicit partMold :Mold[P])
		extends SpecialNamedMold[M](name)
	{
		override def advance(prefix :Liquid, suffix :Liquid) = {
			val (done, part, rem) = partMold.advance(prefix, suffix)
			construct(part).advance(done, rem)
		}
		override def advanceOpt(prefix :Liquid, suffix :Liquid) =
			partMold.advanceOpt(prefix, suffix) match {
				case Got((done, part, rem)) => construct(part).advanceOpt(done, rem)
				case _ => Lack
			}
		override def guardAdvance(prefix :Liquid, suffix :Liquid) =
			partMold.guardAdvance(prefix, suffix) match {
				case Passed((done, part, rem)) => construct(part).guardAdvance(done, rem)
				case fail :Failed => fail
			}
		override def append(prefix :Liquid, model :M) :Liquid = {
			val part = get(model)
			construct(part).append(partMold.append(prefix, part), model)
		}
		override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = {
			val part = get(model)
			partMold.appendOpt(prefix, part) match {
				case Got(appended) => construct(part).appendOpt(appended, model)
				case _ => Lack
			}
		}
		override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] = {
			val part = get(model)
			partMold.guardAppend(prefix, part) match {
				case Passed(appended) => construct(part).guardAppend(appended, model)
				case fail :Failed => fail
			}
		}
		override def toString = flatMapMoldString(name, partName)
	}

	/** A mold returned from `filter` of parts which do not consume or append anything. */
	@SerialVersionUID(ver)
	private class EmptyFilteredMold[M, P](override val name: String, partName: String, isFlatMap: Boolean,
		                                  unfiltered: Mold[M], parse: (Liquid, Liquid) => P, format: (Liquid, M) => P,
		                                  predicate: P => Boolean)
		extends SpecialNamedMold[M](name)
	{
		override def advance(prefix :Liquid, suffix :Liquid) =
			if (predicate(parse(prefix, suffix))) unfiltered.advance(prefix, suffix)
			else parsingError(suffix)

		override def advanceOpt(prefix :Liquid, suffix :Liquid) =
			if (predicate(parse(prefix, suffix))) unfiltered.advanceOpt(prefix, suffix)
			else Lack

		override def guardAdvance(prefix :Liquid, suffix :Liquid) =
			if (predicate(parse(prefix, suffix))) unfiltered.guardAdvance(prefix, suffix)
			else parsingFail(suffix)

		override def append(prefix :Liquid, model :M) =
			if (predicate(format(prefix, model))) unfiltered.append(prefix, model)
			else formattingError(model)

		override def appendOpt(prefix :Liquid, model :M) =
			if (predicate(format(prefix, model))) unfiltered.appendOpt(prefix, model)
			else Lack

		override def guardAppend(prefix :Liquid, model :M) =
			if (predicate(format(prefix, model))) unfiltered.guardAppend(prefix, model)
			else formattingFail(model)

		protected def formattingErrorMsg(model :M) :String = {
			val msgPrefix = "Failed to format " + model
			val msgSuffix = partName match {
				case "" | "_" => "predicate not satisfied."
				case _ => partName + " predicate not satisfied."
			}
			name match {
				case "" | "_" => msgPrefix + ": " + msgSuffix
				case _ => msgPrefix + " as " + name + ": " + msgSuffix
			}
		}
		protected def formattingFail(model :M) = Failed(() => formattingErrorMsg(model))
		protected def formattingError(model :M) :Nothing =
			throw new FormattingException(Format.this, model, formattingErrorMsg(model))

		//todo: make it accept a prefix and a suffix
		protected def parsingErrorMsg(liquid :Liquid) :String = {
			val msgPrefix = "Failed to parse " + Format.this + " '" + liquid + "'"
			val msgSuffix = partName match {
				case "" | "_" => "predicate not satisfied."
				case _ => partName + " predicate not satisfied."
			}
			name match {
				case "" | "_" => msgPrefix + ": " + msgSuffix
				case _ => msgPrefix + " as " + name + ": " + msgSuffix
			}
		}
		protected def parsingFail(liquid  :Liquid) = Failed(() => parsingErrorMsg(liquid))
		protected def parsingError(liquid :Liquid) :Nothing =
			throw ParsingException(Format.this :Format.this.type)(liquid, parsingErrorMsg(liquid))

		override def toString =
			if (isFlatMap) filteredFlatMapMoldString(name, partName)
			else filteredMapMoldString(name, partName)
	}



	/** Creates a `Moldmaker`, a monadic builder for a [[net.noresttherein.sugar.format.Format.Mold Mold]]
	  * responsible for parsing and formatting values of type `M`. It contains a single method
	  * [[net.noresttherein.sugar.format.Format.MoldmakerTemplate.flatMap flatMap]], which takes a constructor
	  * of a ''partial'' `Mold[M]`, used to read and write solely the constants of its models,
	  * and wrapping it up to a complete, final `Mold[M]`. The latter task is delegated
	  * to this formats [[net.noresttherein.sugar.format.Format.wrap wrap]] method, surrounding the parsed/formatted
	  * data with a [[net.noresttherein.sugar.format.Format.open prefix]]
	  * and a [[net.noresttherein.sugar.format.Format.close suffix]] - for example opening and closing tags
	  * `<$name>...</$name>` or `"{"` and `"}"` in JSON. The constructor function in turn takes as an argument
	  * a [[net.noresttherein.sugar.format.Format.Parts Parts]] object for this format, which is a factory
	  * of pseudo monads used for individual [[net.noresttherein.sugar.format.Format.Part parts]] of the model
	  * (i.e., properties) and composes them together.
	  *
	  * See [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] and `Parts` documentation for more details
	  * on the process and examples of usage.
	  *
	  * The method (and the moldmaker) serves also a second purpose, not strictly related to its function:
	  * extension methods [[net.noresttherein.sugar.format.extensions.ParsingFormattingExtensions.as as]]
	  * and [[net.noresttherein.sugar.format.extensions.ParsingFormattingExtensions.cast cast]], available
	  * after importing from [[net.noresttherein.sugar.extensions sugar.extensions]]
	  * or [[net.noresttherein.sugar.format.extensions format.extensions]]
	  * on a [[net.noresttherein.sugar.format.Format.Raw Raw]] data instance of any format,
	  * will cast a model of type specified by that format's `Moldmaker` given as an argument:
	  * {{{
	  *     "<Int>42</Int>" as XML[Int]
	  * }}}
	  *
	  * Note that specific [[net.noresttherein.sugar.format.Format Format]] implementations may narrow the return type
	  * if they define a `Moldmaker` subtype offering additional features.
	  * @param name A name representing the type of the molded model in the formatted data.
	  *             It is used as an argument to `open` and `close` molds used to demarcate the beginning and end
	  *             of the form created by the returned `Moldmaker`, for example to write/validate XML tag
	  *             name. Not all formats use this parameter in any functional way, but it is also used
	  *             in `toString` of the created `Mold`, for example "XML[Witch]".
	  */
	def apply[S](name :String) :Moldmaker[S] = new NamedMoldmaker[S](name)

	/** Creates a `Moldmaker`, a monadic builder for a [[net.noresttherein.sugar.format.Format.Mold Mold]]`[M]`
	  * responsible for parsing and formatting values of the specified class. This method forwards
	  * to its overloaded variant accepting the model's name,
	  * passing it [[net.noresttherein.sugar.prettyprint.extensions.ClassNameExtension.innerName inner class name]]
	  * (stripped of all packages, enclosing objects or classes and demangled) of the specified class.
	  * See that method's documentation for more details.
	  * @param modelClass The class of model objects parsed and formatted by molds created by the returned `Moldmaker`.
	  */
	def apply[S](modelClass :Class[S]) :Moldmaker[S] = apply(modelClass.innerName)

	/** Creates a `Moldmaker`, a monadic builder for a [[net.noresttherein.sugar.format.Format.Mold Mold]]
	  * responsible for parsing and formatting values of the specified type argument. This method forwards
	  * to its overloaded variant accepting the model's name,
	  * passing it [[net.noresttherein.sugar.prettyprint.extensions.ClassNameExtension.innerName inner class name]]
	  * (stripped of all packages, enclosing objects or classes and demangled) of the class
	  * provided by the implicit [[scala.reflect.ClassTag ClassTag]]`[M]`.
	  * See the documentation of other `apply` methods
	  * and the [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] for more details.
	  */
	def apply[S :ClassTag] :Moldmaker[S] = apply[S](classTag[S].runtimeClass.innerName)


	/** The generic interface of this format's [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]].
	  * It allows to create [[net.noresttherein.sugar.format.Format.Mold! Mold]]s through quasi-monadic composition
	  * of [[net.noresttherein.sugar.format.Format.Part Part]]s, created by a factory
	  * [[net.noresttherein.sugar.format.Format.Parts Parts]] given as the argument to this interface's
	  * [[net.noresttherein.sugar.format.Format.MoldmakerTemplate.flatMap flatMap]] arguments.
	  *
	  * See [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] documentation for usage information.
	  * Each standard `Moldmaker` carries the name of the modeled entity given on its creation.
	  * Instances can be obtained using `Format`'s `apply` methods:
	  * {{{
	  *     val maker = format[Dragon]("dragon") //passes explicit name of the modeled entities
	  *     val maker = format(classOf[Dragon])  //uses inner class name of `Dragon` as the modeled entity
	  *     val maker = format[Dragon]           //same as above, relies on a ClassTag
	  * }}}
	  * It exists in this generic form to allow `Format` subclasses to define subtypes covariant in `Parts`
	  * type parameter, so that the functions passed to `flatMap` are given a subtype
	  * of [[net.noresttherein.sugar.format.Format.Parts Parts]] specific to the particular `Format` implementation.
	  * @tparam M     the modeled type; the type parameter of the created molds.
	  * @tparam Parts a [[net.noresttherein.sugar.format.Format.Parts Parts]] subtype provided as the factory
	  *               for [[net.noresttherein.sugar.format.Format.Part Part]]s for individual properties
	  *               of the modeled type `S`.
	  */
	trait MoldmakerTemplate[M, +Parts[X]] {
		/** The format in which the final `Mold` will be made. It is the enclosing class of this class.
		  * This value exists purely so that when parsing one can specify the format and the desired type
		  * at the same time (format presented solely for an illustrative purpose):
		  * {{{
		  *     """<Dragon>
		  *         <name><String>Kaladrax</String></name>
		  *         <color><String>Red</String></color>
		  *         <level><Int>23</Int></level>
		  *     </Dragon>""" parse XML[Dragon]
		  * }}}
		  */
		val format :Format.this.type// = Format.this

		/** Takes a `Mold` constructor for internals of type `M` and wraps it up as a complete standalone format
		  * for that type.
		  * @see [[net.noresttherein.sugar.format.Format.open]]
		  * @see [[net.noresttherein.sugar.format.Format.close]]
		  */
		def flatMap(construct :Parts[M] => Mold[M]) :Mold[M]
	}

	/** A factory of this `Format`'s [[net.noresttherein.sugar.format.Format.Mold Mold]]`[M]` instances.
	  * It allows to combine molds for individual properties of the modeled type `S` into a mold for the whole model
	  * using monadic-like composition. The interface defines a single method:
	  *
	  * [[net.noresttherein.sugar.format.Format.MoldmakerTemplate.flatMap flatMap]]`(f :`[[net.noresttherein.sugar.format.Format.Parts Parts]]`[M] => Mold[M]) :Mold[M]`.
	  *
	  * The constructor function accepted as the argument is responsible for creating a mold for the ''contents''
	  * of the modeled object, and `flatMap` (using `Format`'s [[net.noresttherein.sugar.format.Format.wrap wrap]])
	  * adapts that form to a complete `Mold[M]`, by potentially adding the information about the modeled type itself.
	  * The argument function is given as an argument an instance of this `Format`'s
	  * [[net.noresttherein.sugar.format.Format.Parts Parts]]`[M]` - a factory of composable
	  * [[net.noresttherein.sugar.format.Format.Part Part]]`[M, P]` instances, each adapting an existing `Mold[P]`
	  * for a property `P` of `S` to a `Mold[P]` embeddable within the layout for `S`. Each `Part` provides monadic
	  * methods themselves:
	  *   1. [[net.noresttherein.sugar.format.Format.Part.flatMap flatMap]]`(f :P => Mold[M])` continues the composition,
	  *      receiving a parsed property value as an argument, and all previously parsed properties through a closure
	  *      of nested outer `flatMap` calls;
	  *   1. [[net.noresttherein.sugar.format.Format.Part.map map]]`(f :P => M)` finalizes the process
	  *      and is responsible for creating a whole model from the last part `P` and all previous ones available
	  *      through a closure of all previous `flatMap` calls;
	  *   1. [[net.noresttherein.sugar.format.Format.Part.filter filter]]`(f :P => Boolean)` can be used to abort
	  *      the parsing/formatting process based on previous property values.
	  *
	  * This process is designed to be used in ''for-comprehension'' syntax:
	  * {{{
	  *     //assumes implicit format.Mold[Int] and format.Mold[String]
	  *     case class Dragon(name :String, color :String, level :Int)
	  *
	  *     def dragonMold(format :Format) =
	  *         for {
	  *             dragon <- format[Dragon] //binds dragon to a Moldmaker[Dragon]
	  *             name   <- dragon("name")(_.name) //binds name to a read String and associates it with property name
	  *             color  <- dragon("color")(_.color)
	  *             level  <- dragon("level")(_.level)
	  *         } yield Dragon(name, color, level)
	  * }}}
	  * The above expression can be viewed in two ways:
	  *   1. as a formatter: a list of named properties of various types, together with getter functions
	  *      for said properties and appropriate molds, which can be used to serialize the values of all properties
	  *      of the modeled object, and
	  *   1. as a parser: a composition of reader monads for the above properties, ending with the construction
	  *      of the modeled object from read property values in the `yield` clause.
	  *
	  * While molds for individual formats can be created in a manner described above, it is also possible
	  * to leverage the same process to create a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]`[S]`
	  * instead, essentially a function `format :Format => format.Mold[S]`, by starting with the companion object
	  * [[net.noresttherein.sugar.format.Format$ Format]] rather than a `Format` instance:
	  * {{{
	  *     object Dragon {
	  *         implicit val mold :MoldFactory[S] = for {
	  *             dragon <- Format[Dragon] //binds dragon to a Moldmaker[Dragon] of an arbitrary format
	  *             name   <- dragon("name")(_.name)
	  *             color  <- dragon("color")(_.color)
	  *             level  <- dragon("level")(_.level)
	  *         } yield Dragon(name, color, level)
	  *     }
	  * }}}
	  * An implicit `MoldLayout[S]` implies existence of implicit `format.Mold[S]` for any `format :Format`,
	  * so the layout can be reused to represent the format of a modelled object
	  * for example in JSON, YAML and XML at the same time.
	  *
	  * Some `Format` subclasses provide specialized `Parts` implementations, with additional features.
	  */
	type Moldmaker[M] = MoldmakerTemplate[M, Parts]

	private[format] class NamedMoldmaker[M](name :String) extends NamedParts[M](name) with Moldmaker[M] {
		override val format = Format.this
		override def flatMap(construct :Parts[M] => Mold[M]) = wrap[M](name)(construct(this))
		override def toString = "Moldmaker[" + Format.this + "](" + name + ")"
	}



	/** A factory of [[net.noresttherein.sugar.format.Format.Part Part]] quasi-monads representing parts
	  * of the modeled subject `M`. It is used in the process of building
	  * a [[net.noresttherein.sugar.format.Format.Mold! Mold]]`[M]` in conjunction with builder
	  * [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] - see that type for instruction about usage.
	  *
	  * Supported part categories primarily include proper properties, as created
	  * by method [[net.noresttherein.sugar.format.Format.Parts.apply apply]]`(name :String)(part :M => P)`,
	  * but there are also special methods allowing more control over the process of mold construction,
	  * such as [[net.noresttherein.sugar.format.Format.Parts.suffix look-ahead]]
	  * and [[net.noresttherein.sugar.format.Format.Parts.prefix look-behind]].
	  *
	  * Note that while formatting is done normally behind the scenes - the parts of the model are melted
	  * in the order in which they were composed without user input -
	  * all [[net.noresttherein.sugar.format.Format.Part.filter filter]]s are still applied to the appended values,
	  * so the mold can abort the formatting or simply inject some computation in the filter predicate.
	  * @tparam M the molded model type - the type parameter of the created `Mold`.
	  */
	trait Parts[M] {
		/** The format in which the final `Mold` will be made. It is the enclosing class of this class.
		  * Useful particularly for getting access to `Mold` and other types defined in `Format` instances.
		  */
		val format :Format.this.type = Format.this

		/** Alias for `format.Mold[A]`, tells the compiler look for implicit molds in
		  * `this.`[[net.noresttherein.sugar.format.Format.Parts.format format]].
		  */
		type Mold[A] = format.Mold[A]

		/** A part representing a property of type `P` of molded type `M`.
		  * If followed by [[net.noresttherein.sugar.format.Format.Part.filter filter]]
		  * (or `if` in a for comprehension) the condition will be checked for the value property
		  * both when parsing and formatting and will abort the process with an error if not satisfied.
		  * @param name The name of the property (not the property type, but property itself).
		  *             It is used by formats such as [[net.noresttherein.sugar.format.XML XML]]
		  *             as a tag/attribute name inside the greater `Mold[M]`.
		  *             It is also used in the returned part's `toString` method.
		  * @param part A property getter. Must not throw exceptions -
		  *             use instead [[net.noresttherein.sugar.vars.Potential Potential]]
		  *             or [[net.noresttherein.sugar.vars.Fallible Fallible]] as the property type.
		  */
		def apply[P :Mold](name :String)(part :M => P) :Part[M, P]

		/** A part representing a property of type `P` of molded type `M`.
		  * Parsing of the property is allowed to fail without automatically propagating the error.
		  * Instead the application receives a `Potential[P]` which can be inspected before continuing the parsing.
		  * If [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] is passed as the argument to `part`,
		  * nothing is consumed from the input or appended to the output.
		  * The case of [[net.noresttherein.sugar.vars.Potential.Existent Existent]] value works the same way
		  * as with a standard `Part`.
		  *
		  * If followed by [[net.noresttherein.sugar.format.Format.Part.filter filter]]
		  * (or `if` in a for comprehension) the condition will be checked for the value property
		  * both when parsing and formatting and will abort the process with an error if not satisfied.
		  * @param name The name of the property (not the property type, but property itself).
		  *             It is used by formats such as [[net.noresttherein.sugar.format.XML XML]]
		  *             as a tag/attribute name inside the greater `Mold[M]`.
		  *             It is also used in the returned part's `toString` method.
		  * @param part A property getter. Any exceptions thrown are caught and `Inexistent` is being passed as
		  *             the property value to the mold, resulting in no change to the output.
		  *             However, if the part is followed by `filter`, the predicate receives the empty `Potential`
		  *             and can decide on the further course of action.
		  */
		def opt[P :Mold](name :String)(part :M => P) :Part[M, Potential[P]] =
			apply(name)(Potential.guard(part))(Mold[P].potential)

		/** A part representing a property of type `P` of molded type `M`, both in its solid and liquid forms.
		  * Together with the value of the property, the application receives it
		  * in its [[net.noresttherein.sugar.format.Format.Liquid Liquid]] format -
		  * the result of melting it (when formatting), or the consumed fragment of the input when parsing.
		  * If followed by [[net.noresttherein.sugar.format.Format.Part.filter filter]]
		  * (or `if` in a for comprehension) the condition will be checked for the value property
		  * both when parsing and formatting and will abort the process with an error if not satisfied.
		  * @param name The name of the property (not the property type, but property itself).
		  *             It is used by formats such as [[net.noresttherein.sugar.format.XML XML]]
		  *             as a tag/attribute name inside the greater `Mold[M]`.
		  *             It is also used in the returned part's `toString` method.
		  * @param part A property getter. Must not throw exceptions -
		  *             use instead [[net.noresttherein.sugar.vars.Potential Potential]]
		  *             or [[net.noresttherein.sugar.vars.Fallible Fallible]] as the property type.
		  */
		def mirror[P :Mold](name :String)(part :M => P) :Part[M, (Liquid, P)] =
			apply(name)(model => (emptyLiquid, part(model)))(Mold[P].mirror)

		/** A part representing a property of type `P` of molded type `M`, both in its solid and liquid forms.
		  * Parsing of the property is allowed to fail without automatically propagating the error.
		  * Instead the application receives a `Potential[(Liquid, P)]` which can be inspected
		  * before continuing the parsing. Together with the value of the property, the application receives it
		  * in its [[net.noresttherein.sugar.format.Format.Liquid Liquid]] format -
		  * the result of melting it (when formatting), or the consumed fragment of the input when parsing.
		  * If [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] is passed as the argument to `part`,
		  * nothing is consumed from the input or appended to the output.
		  * The case of [[net.noresttherein.sugar.vars.Potential.Existent Existent]] value works the same way
		  * as with a standard `Part`. If followed by [[net.noresttherein.sugar.format.Format.Part.filter filter]]
		  * (or `if` in a for comprehension) the condition will be checked for the value property
		  * both when parsing and formatting and will abort the process with an error if not satisfied.
		  * @param name The name of the property (not the property type, but property itself).
		  *             It is used by formats such as [[net.noresttherein.sugar.format.XML XML]]
		  *             as a tag/attribute name inside the greater `Mold[M]`.
		  *             It is also used in the returned part's `toString` method.
		  * @param part A property getter. Any exceptions thrown are caught and `Inexistent` is being passed as
		  *             the property value to the mold, resulting in no change to the output.
		  *             However, if the part is followed by `filter`, the predicate receives the empty `Potential`
		  *             and can decide on the further course of action.
		  */
		def mirrorOpt[P :Mold](name :String)(part :M => P) :Part[M, Potential[(Liquid, P)]] =
			apply(name)(Potential.guard(part)(_).map((emptyLiquid, _)))(Mold[P].mirror.potential)

		/** A part representing an optional property of type `P` of molded type `M` similarly
		  * to [[net.noresttherein.sugar.format.Format.Parts.opt opt]], but which doesn't write anything when formatting
		  * and does not consume anything from the input when parsing,
		  * even for [[net.noresttherein.sugar.vars.Potential.Existent Existent]] values.
		  * When formatting, the value is initialized using passed here getter, executed within a `try` block.
		  * @param part A property getter. Any exceptions thrown are caught and `Inexistent` is being passed as
		  *             the property value to the mold, resulting in no change to the output.
		  *             However, if the part is followed by `filter`, the predicate receives the empty `Potential`
		  *             and can decide on the further course of action.
		  */
		def peek[P :Mold](part :M => P) :Part[M, Potential[P]] //Would a property name be helpful here?

		/** A part representing already parsed/formatted data for all previous parts of `M` in the created `Mold[M]`.
		  * The [[net.noresttherein.sugar.format.Format.open open]] tag prefixing all properties is not included
		  * (if the format even defines it).
		  */
		def prefix :Part[M, Liquid]

		/** A part representing yet unparsed data - a suffix of the input raw data. Essentially, it returns
		  * the `suffix` argument of
		  * [[net.noresttherein.sugar.format.Format.Mold Mold]]`.`[[net.noresttherein.sugar.format.Format.Mold.advance advance]].
		  * Unlike with [[net.noresttherein.sugar.format.Format.Parts.prefix prefix]], this potentially can include
		  * data past the range of the formatted model of this mold - other properties of an object having
		  * the molded model as its part/property. This is not guaranteed and can depend on the `Format` implementation,
		  * but the `suffix` returned by this part will contain data for all the following properties
		  * of the molded model `M` up to the [[net.noresttherein.sugar.format.Format.close closing]] mold
		  * (if defined).
		  *
		  * When formatting, the data for parts following this part has not yet been processed and appended,
		  * so this part always returns an empty `Liquid`.
		  * @see [[net.noresttherein.sugar.format.Format.Parts.unparsed]]
		  */
		def suffix :Part[M, Liquid]

		/** A part representing yet unparsed data - a suffix of the input raw data. Essentially, it returns
		  * the `suffix` argument of
		  * [[net.noresttherein.sugar.format.Format.Mold Mold]]`.`[[net.noresttherein.sugar.format.Format.Mold.advance advance]].
		  * When parsing, the value is always wrapped in [[net.noresttherein.sugar.vars.Potential.Existent Existent]],
		  * but when formatting, it is [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]]
		  * to allow code within nested `Part.flatMap` calls to discover if it is currently parsing or formatting.
		  *
		  * Unlike with [[net.noresttherein.sugar.format.Format.Parts.prefix prefix]], this potentially can include
		  * data past the range of the formatted model of this mold - other properties of an object having
		  * the molded model as its part/property. This is not guaranteed and can depend on the `Format` implementation,
		  * but the `suffix` returned by this part will contain data for all the following properties
		  * of the molded model `M` up to the [[net.noresttherein.sugar.format.Format.close closing]] mold
		  * (if defined).
		  */
		def unparsed :Part[M, Potential[Liquid]]

		/** A part for an indicator of which process the code is running.
		  * It allows to execute within its [[net.noresttherein.sugar.format.Format.Part.flatMap flatMap]]
		  * or [[net.noresttherein.sugar.format.Format.Part.map map]] conditional code,
		  * different for formatting and for parsing.
		  */
		def process :Part[M, MoldingProcess]

		override def toString :String = "Parts(" + Format.this.toString + ")@" + this.shortHashString
	}

	/** Default `Parts` implementation carrying the name of the molded model type passed on to all created `Part`s. */
	private[format] class NamedParts[M](name :String) extends Parts[M] {
		override def apply[P :Mold](name :String)(part :M => P) :Part[M, P] =
			new PropertyPart(part, this.name, name)(propertyMold(name))

		override def mirror[P :Mold](name :String)(part :M => P) :Part[M, (Liquid, P)] =
			new MirrorPart(part, this.name, name)(propertyMold(name))

		override def peek[P :Mold](part :M => P) :Part[M, Potential[P]] =
			new PeekPart(part, this.name, "_")(propertyMold(name))

		override def prefix   :Part[M, Liquid]            = new LookBehindPart[M](name)
		override def suffix   :Part[M, Liquid]            = new LookAheadPart[M](name)
		override def unparsed :Part[M, Potential[Liquid]] = new UnparsedPart[M](name)
		override def process  :Part[M, MoldingProcess]    = new MoldingProcessPart[M](name)

		override def toString = "Parts[" + Format.this + "](" + name + ")"
	}



	/** A [[net.noresttherein.sugar.format.Format.Mold Mold]] builder element representing a single property or part
	  * of the molded model type. `Part`s for all such properties are composed together in a semi-monadic fashion
	  * by nesting calls to [[net.noresttherein.sugar.format.Format.Part.flatMap flatMap]],
	  * finished with [[net.noresttherein.sugar.format.Format.Part.map map]] at the deepest level.
	  * Every `flatMap` call introduces to the closure started by the most outer one a value of type `P`,
	  * representing the next parsed property. Once all data for the whole model is parsed, the model's constructor
	  * combining them all together is called from within `map`. Thus, while a `Part` in itself carries information
	  * about a single part, a function passed to `flatMap` is at the same time a closure binding previously
	  * parsed properties, and a constructor responsible for parsing the following one and creating the model.
	  *
	  * While composing parts is thus explicitly responsible for parsing a model, each at the same time also carries
	  * information about how to obtain its value from a model instance, and the formatting method is created
	  * by executing them in the same order and formatting their values with a `Mold` carried by the part.
	  *
	  * `Part` instances are obtained from their factory [[net.noresttherein.sugar.format.Format.Parts Parts]],
	  * which is not normally obtainable. Instead, it is the argument of functions passed to method
	  * [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]]`.`[[net.noresttherein.sugar.format.Format.MoldmakerTemplate.flatMap flatMap]].
	  * The function argument of the latter is a constructor for a `Mold[M]` created in the previously described manner,
	  * which can parse and format ''the contents'' of the model. However, a proper mold might need to precede
	  * and/or follow the contents with additional information, such as XML opening and closing tag
	  * marking the [[net.noresttherein.sugar.format.Format.Raw raw]] data fragment as an element for `M`.
	  * This information is added by the enclosing `Moldmaker.flatMap`.
	  * @tparam M the molded type - the type parameter of the created mold.
	  * @tparam P the type of a property of model `M` or, in general, a single value required when parsing and/or
	  *           formatting the model.
	  */
	trait Part[M, P] {
		/** Finalizes the parsing of the model, colling its constructor/factory method with all parameters
		  * parsed by enclosing calls to [[net.noresttherein.sugar.format.Format.Part.flatMap flatMap]].
		  * @param construct a constructor function for `M`.
		  */
		def map(construct :P => M) :Mold[M]
		/** Compose this mold with parts for following properties.
		  * @param construct a function accepting a value for ths part creating another `Part` instance
		  *                  for a following property and invoking either its `flatMap` or, if it is the last part,
		  *                  its `map` passing the model's constructor.
		  */
		def flatMap(construct :P => Mold[M]) :Mold[M]
		/** Introduces a validating condition for this part's value and all those captured in a closure
		  * from enclosing `flatMap` calls. The condition, unless specifically noted by documentation
		  * for a `Part` created by a particular factory method of [[net.noresttherein.sugar.format.Format.Parts Parts]],
		  * is executed both when parsing and formatting. If the test fails, the parsing/formatting process
		  * is aborted and an error is reported in a fashion depending on which
		  * of [[net.noresttherein.sugar.format.Format.Mold Mold]]'s methods was called.
		  */
		def filter(predicate :P => Boolean) :Part[M, P]
		/** The same as [[net.noresttherein.sugar.format.Format.Part.filter filter]], for use in a for comprehension. */
		def withFilter(predicate :P => Boolean) :Part[M, P] = filter(predicate)
	}



	/** Base trait for proxies to other `Part`s returned by that part's
	  * [[net.noresttherein.sugar.format.Format.Part.filter filter]] method.
	  * Provides solely some helper methods.
	  */
	private abstract class FilteredPartBase[M, P](modelName :String, partName :String) extends Part[M, P] {
		//todo: make it accept a prefix and a suffix
		protected def formattingErrorMsg(model :M) :String = {
			val msgPrefix = "Failed to format " + model
			val msgSuffix = partName match {
				case "" | "_" => "predicate not satisfied."
				case _ => partName + " predicate not satisfied."
			}
			modelName match {
				case "" | "_" => msgPrefix + ": " + msgSuffix
				case _ => msgPrefix + " as " + modelName + ": " + msgSuffix
			}
		}
		protected def formattingError(model :M) :Nothing =
			throw new FormattingException(Format.this, model, formattingErrorMsg(model))

		//todo: make it accept a prefix and a suffix
		protected def parsingErrorMsg(liquid :Liquid) :String = {
			val msgPrefix = "Failed to parse " + Format.this + " '" + liquid + "'"
			val msgSuffix = partName match {
				case "" | "_" => "predicate not satisfied."
				case _ => partName + " predicate not satisfied."
			}
			modelName match {
				case "" | "_" => msgPrefix + ": " + msgSuffix
				case _ => msgPrefix + " as " + modelName + ": " + msgSuffix
			}
		}
		protected def parsingError(liquid :Liquid) :Nothing =
			throw ParsingException(Format.this :Format.this.type)(liquid, parsingErrorMsg(liquid))
	}



	/** The default `Part` implementation for a single property of type `P` of the molded model `W`.
	  * Method [[net.noresttherein.sugar.format.Format.Part.filter filter]] works both for parsing and formatting.
	  */
	private class PropertyPart[M, P](get :M => P, modelName :String = "_", partName :String = "_")
	                                (implicit partMold :Mold[P])
		extends Part[M, P]
	{
		override def map(construct :P => M) :Mold[M] =
			new MapMold[M, P](modelName, partName, get, construct)

		override def flatMap(construct :P => Mold[M]) :Mold[M] =
			new FlatMapMold[M, P](modelName, partName, get, construct)

		override def filter(predicate :P => Boolean) :Part[M, P] =
			new FilteredPartBase[M, P](modelName, partName) {
				private[this] val p = predicate
				//map and flatMap can't have a common mold because they can't adapt Mold[M], as they need a value of P - not M
				override def map(construct :P => M) =
					new SpecialNamedMold[M](modelName) {
						override def advance(prefix :Liquid, suffix :Liquid) = {
							val (done, part, rem) = partMold.advance(prefix, suffix)
							if (p(part)) (done, construct(part), rem)
							else parsingError(suffix)
						}
						override def advanceOpt(prefix :Liquid, suffix :Liquid) =
							partMold.advanceOpt(prefix, suffix) match {
								case Got((done, part, rem)) if p(part) => Got((done, construct(part), rem))
								case _ => Lack
						}
						override def guardAdvance(prefix :Liquid, suffix :Liquid) =
							partMold.guardAdvance(prefix, suffix) match {
								case Passed((done, part, rem)) if p(part) => Passed((done, construct(part), rem))
								case Passed((_, _, _)) => Failed(() => parsingErrorMsg(suffix))
								case failed: Failed => failed
							}
						override def append(prefix :Liquid, model :M) = {
							val part = get(model)
							val res = partMold.append(prefix, part) //just so partMold's evaluate always before predicates
							if (p(part)) res
							else formattingError(model)
						}
						override def appendOpt(prefix :Liquid, model :M) = {
							val part = get(model)
							val res = partMold.appendOpt(prefix, part)
							if (p(part)) res
							else Lack
						}
						override def guardAppend(prefix :Liquid, model :M) = {
							val part = get(model)
							val res = partMold.guardAppend(prefix, part)
							if (p(part)) res
							else Failed(() => formattingErrorMsg(model))
						}
						override def toString :String = filteredMapMoldString(modelName, partName)
					}

				override def flatMap(construct :P => Mold[M]) =
					new SpecialNamedMold[M](modelName) {
						override def advance(prefix :Liquid, suffix :Liquid) = {
							val (done, part, rem) = partMold.advance(prefix, suffix)
							if (p(part)) construct(part).advance(done, rem)
							else parsingError(suffix)
						}
						override def advanceOpt(prefix :Liquid, suffix :Liquid) =
							partMold.advanceOpt(prefix, suffix) match {
								case Got((done, part, rem)) if p(part) => construct(part).advanceOpt(done, rem)
								case _ => Lack
							}
						override def guardAdvance(prefix :Liquid, suffix :Liquid) =
							partMold.guardAdvance(prefix, suffix) match {
								case Passed((done, part, rem)) if p(part) => construct(part).guardAdvance(done, rem)
								case Passed((_, _, _)) => Failed(() => parsingErrorMsg(suffix))
								case fail :Failed => fail
							}
						override def append(prefix :Liquid, model :M) = {
							val part = get(model)
							val liquid = partMold.append(prefix, part)
							if (p(part)) construct(part).append(liquid, model)
							else formattingError(model)
						}
						override def appendOpt(prefix :Liquid, model :M) = {
							val part = get(model)
							partMold.appendOpt(prefix, part) match {
								case Got(liquid) if p(part) => construct(part).appendOpt(liquid, model)
								case _ => Lack
							}
						}
						override def guardAppend(prefix :Liquid, model :M) = {
							val part = get(model)
							partMold.guardAppend(prefix, part) match {
								case Passed(liquid) if p(part) => construct(part).guardAppend(liquid, model)
								case Passed(_) => Failed(() => formattingErrorMsg(model))
								case fail :Failed => fail
							}
						}
						override def toString :String = filteredFlatMapMoldString(modelName, partName)
					}

				override def filter(predicate :P => Boolean) =
					PropertyPart.this.filter { part => p(part) && predicate(part) }

				override def toString :String = filteredPartString(this, modelName, partName, partMold)
			}

		override def toString :String = partString(this, modelName, partName, partMold)
	}



	/** A part for a single property `P` of molded model `W`,
	  * paired with the [[net.noresttherein.sugar.format.Format.Liquid! Liquid]] fragment
	  * to which the property value corresponds.
	  * @see [[net.noresttherein.sugar.format.Format.PropertyPart]]
	  */
	private class MirrorPart[M, P](get :M => P, modelName :String = "_", partName :String = "_")
	                              (implicit partMold :Mold[P])
		extends Part[M, (Liquid, P)]
	{
		override def map(construct :((Liquid, P)) => M) :Mold[M] =
			new SpecialNamedMold[M](modelName) {
				override def advance(prefix :Liquid, suffix :Liquid) = {
					//todo: Use prefix in partMold.advance (needs a new advance method).
					// Its lack is theoretically problematic, but this is supposed to be a part for a property,
					// so unlike to need it.
					val (parsed, part, rem) = partMold.advance(suffix)
					(concat(prefix, parsed), construct((parsed, part)), rem)
				}
				override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
					//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
					partMold.advanceOpt(suffix) match {
						case Got((parsed, part, rem)) => concatOpt(prefix, parsed) match {
							case Got(done) => Got((done, construct((parsed, part)), rem))
						}
						case _ => Lack
					}
				override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
					//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
					partMold.guardAdvance(suffix) match {
						case Passed((parsed, part, rem)) => guardConcat(prefix, parsed) match {
							case Passed(done) => Passed((done, construct((parsed, part)), rem))
							case fail :Failed => fail
						}
						case fail :Failed => fail
					}
				override def append(prefix :Liquid, model :M) :Liquid =
					partMold.append(prefix, get(model))
				override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] =
					partMold.appendOpt(prefix, get(model))
				override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] =
					partMold.guardAppend(prefix, get(model))

				override def toString = mapMoldString(modelName, "(" + partName + ", melted)")
			}

		override def flatMap(construct :((Liquid, P)) => Mold[M]) :Mold[M] =
			new SpecialNamedMold[M](modelName) {
				override def advance(prefix :Liquid, suffix :Liquid) = {
					//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
					val (parsed, part, rem) = partMold.advance(suffix)
					construct((parsed, part)).advance(concat(prefix, parsed), rem)
				}
				override def advanceOpt(prefix :Liquid, suffix :Liquid) =
					//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
					partMold.advanceOpt(suffix) match {
						case Got((parsed, part, rem)) => concatOpt(prefix, parsed) match {
							case Got(done) => construct((parsed, part)).advanceOpt(done, rem)
							case _ => Lack
						}

						case _ => Lack
					}
				override def guardAdvance(prefix :Liquid, suffix :Liquid) =
					//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
					partMold.guardAdvance(suffix) match {
						case Passed((parsed, part, rem)) => guardConcat(prefix, parsed) match {
							case Passed(done) => construct((parsed, part)).guardAdvance(done, rem)
							case fail :Failed => fail
						}
						case fail :Failed => fail
					}
				override def append(prefix :Liquid, model :M) :Liquid = {
					val part = get(model)
					val melted = partMold.melt(part)
					construct((melted, part)).append(concat(prefix, melted), model)
				}
				override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] = {
					val part = get(model)
					partMold.meltOpt(part) match {
						case Got(melted) => concatOpt(prefix, melted) match {
							case Got(done) => construct((melted, part)).appendOpt(done, model)
							case _ => Lack
						}
						case _ => Lack
					}
				}
				override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] = {
					val part = get(model)
					partMold.guardMelt(part) match {
						case Passed(melted) => guardConcat(prefix, melted) match {
							case Passed(done) => construct((melted, part)).guardAppend(done, model)
							case fail :Failed => fail
						}
						case fail :Failed => fail
					}
				}
				override def toString = flatMapMoldString(modelName, "(" + partName + ", melted)")
			}

		override def filter(predicate :((Liquid, P)) => Boolean) :Part[M, (Liquid, P)] =
			new FilteredPartBase[M, (Liquid, P)](modelName, partName) {
				private[this] val p = predicate
				private def partName = MirrorPart.this.partName + ", melted"

				override def map(construct :((Liquid, P)) => M) =
					new SpecialNamedMold[M](modelName) {
						override def advance(prefix :Liquid, suffix :Liquid) = {
							//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
							val (liquid, part, rem) = partMold.advance(suffix)
							val mirror = (liquid, part)
							if (p(mirror)) (concat(prefix, liquid), construct(mirror), rem)
							else parsingError(suffix)
						}
						override def advanceOpt(prefix :Liquid, suffix :Liquid) =
							//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
							partMold.advanceOpt(suffix) match {
								case Got((liquid, part, rem)) =>
									val mirror = (liquid, part)
									if (p(mirror))
										concatOpt(prefix, liquid) match {
											case Got(done) => Got((done, construct(mirror), rem))
											case _ => Lack
										}
									else Lack
								case _ => Lack
						}
						override def guardAdvance(prefix :Liquid, suffix :Liquid) =
							//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
							partMold.guardAdvance(suffix) match {
								case Passed((liquid, part, rem)) =>
									val mirror = (liquid, part)
									if (p(mirror)) {
										guardConcat(prefix, liquid) match {
											case Passed(done) => Passed((done, construct(mirror), rem))
											case fail :Failed => fail
										}
									} else Failed(() => parsingErrorMsg(suffix))
								case fail :Failed => fail
							}
						override def append(prefix :Liquid, model :M) = {
							val part = get(model)
							val melted = partMold.melt(part)
							if (p((melted, part))) concat(prefix, melted)
							else formattingError(model)
						}
						override def appendOpt(prefix :Liquid, model :M) = {
							val part = get(model)
							partMold.meltOpt(part) match {
								case Got(melted) if p((melted, part)) => concatOpt(prefix, melted)
								case _ => Lack
							}
						}
						override def guardAppend(prefix :Liquid, model :M) = {
							val part = get(model)
							partMold.guardMelt(part) match {
								case Passed(melted) if p((melted, part)) => guardConcat(prefix, melted)
								case Passed(_) => Failed(() => formattingErrorMsg(model))
								case fail :Failed => fail
							}
						}
						override def toString :String = filteredMapMoldString(modelName, partName)
					}

				override def flatMap(construct :((Liquid, P)) => Mold[M]) =
					new SpecialNamedMold[M](modelName) {
						override def advance(prefix :Liquid, suffix :Liquid) = {
							//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
							val (done, part, rem) = partMold.advance(suffix)
							val mirror = (done, part)
							if (p(mirror)) construct(mirror).advance(concat(prefix, done), rem)
							else parsingError(suffix)
						}
						override def advanceOpt(prefix :Liquid, suffix :Liquid) =
							//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
							partMold.advanceOpt(suffix) match {
								case Got((liquid, part, rem)) =>
									val mirror = (liquid, part)
									if (p(mirror))
										concatOpt(prefix, liquid) match {
											case Got(done) => construct(mirror).advanceOpt(done, rem)
											case _ => Lack
										}
									else Lack
								case _ => Lack
							}
						override def guardAdvance(prefix :Liquid, suffix :Liquid) =
							//not using prefix in advance may be problematic if the part needs it, but it shouldn't be the case
							partMold.guardAdvance(suffix) match {
								case Passed((liquid, part, rem)) =>
									val mirror = (liquid, part)
									if (p(mirror))
										guardConcat(prefix, liquid) match {
											case Passed(done) => construct(mirror).guardAdvance(done, rem)
											case fail :Failed => fail
										}
									else Failed(() => parsingErrorMsg(suffix))
								case fail :Failed => fail
							}

						override def append(prefix :Liquid, model :M) = {
							val part = get(model)
							val mirror = (partMold.melt(part), part)
							if (p(mirror))
								construct(mirror).append(concat(prefix, mirror._1), model)
							else formattingError(model)
						}
						override def appendOpt(prefix :Liquid, model :M) = {
							val part = get(model)
							partMold.meltOpt(part) match {
								case Got(melted) =>
									val mirror = (melted, part)
									concatOpt(prefix, melted) match {
										case Got(done) if p(mirror) => construct(mirror).appendOpt(done, model)
										case _ => Lack
									}
								case _ => Lack
							}
						}
						override def guardAppend(prefix :Liquid, model :M) = {
							val part = get(model)
							partMold.guardMelt(part) match {
								case Passed(melted) =>
									val mirror = (melted, part)
									guardConcat(prefix, melted) match {
										case Passed(done) if p(mirror) => construct(mirror).guardAppend(done, model)
										case Passed(_) => Failed(() => formattingErrorMsg(model))
										case fail :Failed => fail
									}
								case fail :Failed => fail
							}
						}
						override def toString :String = filteredFlatMapMoldString(modelName, partName)
					}

				override def filter(predicate :((Liquid, P)) => Boolean) =
					MirrorPart.this.filter { mirror => p(mirror) && predicate(mirror) }

				override def toString :String = filteredPartString(this, modelName, partName + ", melted", partMold)
			}

		override def toString :String = partString(this, modelName, partName + ", melted", partMold)
	}



	/** A part for a single property `P` of a molded model `W` which does not consume the parsed
	  * [[net.noresttherein.sugar.format.Format.Liquid! Liquid]]. The part's value is optional to support
	  * conditional logic depending on the actual parsed input. The part does not add anything to the formatted
	  * output when melting the model.
	  */ //consider: get :W => Potential[P]
	private class PeekPart[M, P](get :M => P, modelName :String = "_", partName :String = "_")
	                            (implicit partMold :Mold[P])
		extends Part[M, Potential[P]]
	{
		override def map(construct :Potential[P] => M) :Mold[M] =
			new AbstractEmptyMold[M] with NamedMold[M] {
				override def name = modelName
				override def head(liquid :Liquid) = construct(partMold.headOpt(liquid))
				override def advance(prefix :Liquid, suffix :Liquid) =
					(prefix, construct(partMold.advanceOpt(prefix, suffix) map (_._2)), suffix)
				override def toString = mapMoldString(modelName, "peek(" + partName + ")")
			}

		override def flatMap(construct :Potential[P] => Mold[M]) :Mold[M] =
			new SpecialNamedMold[M](modelName) {
				override def advance(prefix :Liquid, suffix :Liquid) =
					construct(partMold.headOpt(suffix)).advance(prefix, suffix)
				override def advanceOpt(prefix :Liquid, suffix :Liquid) =
					construct(partMold.headOpt(suffix)).advanceOpt(prefix, suffix)
				override def guardAdvance(prefix :Liquid, suffix :Liquid) =
					construct(partMold.headOpt(suffix)).guardAdvance(prefix, suffix)

				override def append(prefix :Liquid, model :M) :Liquid =
					construct(Potential.guard(get)(model)).append(prefix, model)
				override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] =
					construct(Potential.guard(get)(model)).appendOpt(prefix, model)
				override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] =
					construct(Potential.guard(get)(model)).guardAppend(prefix, model)

				override def toString = flatMapMoldString(modelName, "peek(" + partName + ")")
			}

		override def filter(predicate :Potential[P] => Boolean) :Part[M, Potential[P]] =
			new FilteredPartBase[M, Potential[P]](modelName, partName) {
				private def FilteredMold(unfiltered :Mold[M], isFlatMap :Boolean) =
					new EmptyFilteredMold[M, Potential[P]](modelName, partName, isFlatMap, unfiltered, (_, suffix) => partMold.headOpt(suffix), (_, model) => Potential.guard(get)(model), predicate)
				override def map(construct :Potential[P] => M) :Mold[M] =
					FilteredMold(PeekPart.this.map(construct), false)

				override def flatMap(construct :Potential[P] => Mold[M]) :Mold[M] =
					FilteredMold(PeekPart.this.flatMap(construct), true)

				override def filter(p :Potential[P] => Boolean) =
					PeekPart.this.filter { part => predicate(part) && p(part) }

				override def toString =
					filteredPartString(this, modelName, "peek(" + partName + ")", partMold)
			}

		override def toString :String =
			partString(this, modelName, "peek(" + partName + ")", partMold)
	}



	/** A `Part` representing the unparsed suffix of the input [[net.noresttherein.sugar.format.Format.Liquid! Liquid]].
	  * It does not consume anything when parsing or add anything to the output when formatting.
	  */
	private class LookAheadPart[M](modelName :String = "_", partName :String = "suffix") extends Part[M, Liquid] {
		override def map(construct :Liquid => M) =
			new AbstractEmptyMold[M] with NamedMold[M] {
				override def name = modelName
				override def head(liquid :Liquid) = construct(liquid)
				override def toString = mapMoldString(modelName, partName)
			}
		override def flatMap(construct :Liquid => Mold[M]) =
			new SpecialNamedMold[M](modelName) {
				override def advance(prefix :Liquid, suffix :Liquid) = construct(suffix).advance(prefix, suffix)
				override def advanceOpt(prefix :Liquid, suffix :Liquid) = construct(suffix).advanceOpt(prefix, suffix)
				override def guardAdvance(prefix :Liquid, suffix :Liquid) =
					construct(suffix).guardAdvance(prefix, suffix)

				override def append(prefix :Liquid, model :M) = construct(emptyLiquid).append(prefix, model)
				override def appendOpt(prefix :Liquid, model :M) = construct(emptyLiquid).appendOpt(prefix, model)
				override def guardAppend(prefix :Liquid, model :M) = construct(emptyLiquid).guardAppend(prefix, model)
				override def toString = flatMapMoldString(modelName, partName)
			}

		override def filter(predicate :Liquid => Boolean) =
			new FilteredPartBase[M, Liquid](modelName, partName) {
				def FilteredMold(unfiltered :Mold[M], isFlatMap :Boolean) =
					new EmptyFilteredMold[M, Liquid](modelName, partName, isFlatMap, unfiltered, (_, suffix) => suffix, (_, _) => emptyLiquid, predicate)
				override def map(construct :Liquid => M) :Mold[M] =
					FilteredMold(LookAheadPart.this.map(construct), false)

				override def flatMap(construct :Liquid => Mold[M]) :Mold[M] =
					FilteredMold(LookAheadPart.this.flatMap(construct), true)

				override def filter(p :Liquid => Boolean) =
					LookAheadPart.this.filter { suffix => predicate(suffix) && p(suffix) }

				override def toString = filteredPartString(this, modelName, partName, null)
			}

		override def toString = partString(this, modelName, partName, null)
	}



	/** A `Part` representing the unparsed suffix of the input [[net.noresttherein.sugar.format.Format.Liquid! Liquid]].
	  * It does not consume anything when parsing or add anything to the output when formatting.
	  * When parsing, the part value is always `Existent`, even if the following suffix is empty.
	  * When formatting, the part value is always `Inexistent`.
	  */
	private class UnparsedPart[M](modelName :String = "_", partName :String = "unprocessed")
		extends Part[M, Potential[Liquid]]
	{
		override def map(construct :Potential[Liquid] => M) =
			new AbstractEmptyMold[M] with NamedMold[M] {
				override def name = modelName
				override def head(liquid :Liquid) = construct(Existent(liquid))
				override def toString = mapMoldString(modelName, partName)
			}
		override def flatMap(construct :Potential[Liquid] => Mold[M]) =
			new SpecialNamedMold[M](modelName) {
				override def advance(prefix :Liquid, suffix :Liquid) =
					construct(Existent(suffix)).advance(prefix, suffix)
				override def advanceOpt(prefix :Liquid, suffix :Liquid) =
					construct(Existent(suffix)).advanceOpt(prefix, suffix)
				override def guardAdvance(prefix :Liquid, suffix :Liquid) =
					construct(Existent(suffix)).guardAdvance(prefix, suffix)

				override def append(prefix :Liquid, model :M) = construct(Inexistent).append(prefix, model)
				override def appendOpt(prefix :Liquid, model :M) = construct(Inexistent).appendOpt(prefix, model)
				override def guardAppend(prefix :Liquid, model :M) = construct(Inexistent).guardAppend(prefix, model)
				override def toString = flatMapMoldString(modelName, partName)
			}

		override def filter(predicate :Potential[Liquid] => Boolean) =
			new FilteredPartBase[M, Potential[Liquid]](modelName, partName) {
				def FilteredMold(unfiltered :Mold[M], isFlatMap :Boolean) =
					new EmptyFilteredMold[M, Potential[Liquid]](modelName, partName, isFlatMap, unfiltered, (_, suffix) => Existent(suffix), (_, _) => Inexistent, predicate)
				override def map(construct :Potential[Liquid] => M) :Mold[M] =
					FilteredMold(UnparsedPart.this.map(construct), false)

				override def flatMap(construct :Potential[Liquid] => Mold[M]) :Mold[M] =
					FilteredMold(UnparsedPart.this.flatMap(construct), true)

				override def filter(p :Potential[Liquid] => Boolean) =
					UnparsedPart.this.filter(potential => predicate(potential) && p(potential))
			}
		}



	/** A `Part` representing the already parsed/melted fragment of the input/output
	  * [[net.noresttherein.sugar.format.Format.Liquid! Liquid]]. It does not consume anything when casting
	  * and does not append anything to the formatted output when melting.
	  */
	private class LookBehindPart[M](modelName :String = "_", partName :String = "prefix") extends Part[M, Liquid] {
		override def map(construct :Liquid => M) =
			new SafeCastingMold[M] with ReadOnlyMold[M] with NamedMold[M] {
				override def name = modelName
				override def head(liquid :Liquid) = construct(emptyLiquid)
				override def advance(prefix :Liquid, suffix :Liquid) = (prefix, construct(prefix), suffix)
				override def toString = mapMoldString(modelName, partName)
			}

		override def flatMap(construct :Liquid => Mold[M]) =
			new SimpleMold[M] with ReadOnlyMold[M] with NamedMold[M]{
				override def name = modelName
				override def advance(prefix :Liquid, suffix :Liquid) = construct(prefix).advance(prefix, suffix)
				override def advanceOpt(prefix :Liquid, suffix :Liquid) = construct(prefix).advanceOpt(prefix, suffix)
				override def guardAdvance(prefix :Liquid, suffix :Liquid) = construct(prefix).guardAdvance(prefix, suffix)
				override def toString = flatMapMoldString(modelName, partName)
			}

		override def filter(predicate :Liquid => Boolean) =
			new FilteredPartBase[M, Liquid](modelName, partName) {
				private def FilteredMold(unfiltered :Mold[M], isFlatMap :Boolean) =
					new EmptyFilteredMold[M, Liquid](modelName, partName, isFlatMap, unfiltered, (prefix, _) => prefix, (prefix, _) => prefix, predicate)
				override def map(construct :Liquid => M) :Mold[M] =
					FilteredMold(LookBehindPart.this.map(construct), false)

				override def flatMap(construct :Liquid => Mold[M]) :Mold[M] =
					FilteredMold(LookBehindPart.this.flatMap(construct), true)

				override def filter(p :Liquid => Boolean) =
					LookBehindPart.this.filter { prefix => predicate(prefix) && p(prefix) }

				override def toString = filteredPartString(this, modelName, partName, null)
			}

		override def toString = partString(this, modelName, partName, null)
	}


	/** A `Part` whose value is `Casting` within parsing methods and `Melting` within formatting methods. */
	private class MoldingProcessPart[M](modelName :String = "_") extends Part[M, MoldingProcess] {
		private final val partName = "MoldingProcess"
		override def map(construct :MoldingProcess => M) = new EmptyMold(construct(Casting))

		override def flatMap(construct :MoldingProcess => Mold[M]) =
			new SpecialNamedMold[M](modelName) {
				override def advance(prefix :Liquid, suffix :Liquid) = construct(Casting).advance(prefix, suffix)
				override def advanceOpt(prefix :Liquid, suffix :Liquid) = construct(Casting).advanceOpt(prefix, suffix)
				override def guardAdvance(prefix :Liquid, suffix :Liquid) =
					construct(Casting).guardAdvance(prefix, suffix)

				override def append(prefix :Liquid, model :M) = construct(Melting).append(prefix, model)
				override def appendOpt(prefix :Liquid, model :M) = construct(Melting).appendOpt(prefix, model)
				override def guardAppend(prefix :Liquid, model :M) = construct(Melting).guardAppend(prefix, model)
				override def toString :String = flatMapMoldString(name, partName)
			}

		override def filter(predicate :MoldingProcess => Boolean) =
			new FilteredPartBase[M, MoldingProcess](modelName, partName) {
				def FilteredMold(unfiltered :Mold[M], isFlatMap :Boolean) =
					new EmptyFilteredMold[M, MoldingProcess](modelName, partName, isFlatMap, unfiltered, (_, _) => Casting, (_, _) => Melting, predicate)
				override def map(construct :MoldingProcess => M) :Mold[M] =
					FilteredMold(MoldingProcessPart.this.map(construct), false)

				override def flatMap(construct :MoldingProcess => Mold[M]) :Mold[M] =
					FilteredMold(MoldingProcessPart.this.flatMap(construct), true)

				override def filter(p :MoldingProcess => Boolean) =
					MoldingProcessPart.this.filter(process => predicate(process) && p(process))

				override def toString = filteredPartString(this, modelName, "MoldingProcess", null)
			}
	}



	//todo: hide these somewhere, as they impede binary compatibility.
	private[format] def mapMoldString(modelName :String, partName :String) :String =
		Format.this.toString + "[" + modelName + "/" + partName + "]"

	private[format] def flatMapMoldString(modelName :String, partName :String) :String =
		Format.this.toString + "[" + modelName + "/" + partName + "...]"

	private[format] def filteredMapMoldString(modelName :String, partName :String) :String =
		Format.this.toString + "[filter(" + modelName + "/" + partName + ")]"

	private[format] def filteredFlatMapMoldString(modelName :String, partName :String) :String =
		Format.this.toString + "[filter(" + modelName + "/" + partName + ")...]"

	private[format] def partString(part :Part[_, _], partName :String, partMold :Mold[_]) :String =
		if (partName.length == 0 || partName == "_") partMold match {
			case null =>
				"@" + part.hashCodeString
			case named :NamedMold[_] if named.name.length > 0 && named.name != "_" =>
				"@" + part.hashCodeString + ":" + named.name
			case _ =>
				"@" + part.hashCodeString
		} else
			partName

	private[format] def partString(part :Part[_, _], modelName :String, partName :String, partMold :Mold[_]) :String =
		Format.this.toString + "[" + modelName + "]." + partString(part, partName, partMold)

	private[format] def filteredPartString(part :Part[_, _], modelName :String, partName :String, partMold :Mold[_]) :String =
		Format.this.toString + "[" + modelName + "].filter(" + partString(part, partName, partMold) + ")"

	private[format] def parseErrorMsg[S](mold :Mold[S], input :Liquid) :String =
		"Failed to parse '" + input + "' as " + mold + "."

	private[format] def formatErrorMsg[S](mold :Mold[S], model :S) :String =
		"Failed to format " + model + " as " + mold + "."

	private[format] def parseError[S](mold :Mold[S], input :Liquid) :Nothing =
		throw ParsingException(this :this.type)(input, parseErrorMsg(mold, input))

	private[format] def formatError[S](mold :Mold[S], model :S) :Nothing =
		throw new FormattingException(this, model, formatErrorMsg(mold, model))






	/** Converts a formatted raw object (a `String`, `Array[Byte]`, etc.) to an intermediate form ready for
	  * parsing ('casting') using this format's [[net.noresttherein.sugar.format.Format.Mold molds]].
	  * This conversion is assumed to be fast (at most `O(log n)`) and introduce faster methods for views
	  * over portions of `raw` data. The conversion should not attempt to validate the data itself,
	  * but rather prepare structures necessary for molds to work. If some sort of initialization is necessary
	  * for the [[net.noresttherein.sugar.format.Format.Part partial]] molds to work,
	  * it should be implemented by the decorator `Mold` created by [[net.noresttherein.sugar.format.Format.wrap wrap]]
	  * from a `Mold` representing the contents of the model object only.
	  * @see [[net.noresttherein.sugar.format.Format.cool]]
	  */
    @throws[FormatException]("if the value cannot be melted.")
	def melt(raw :Raw) :Liquid

	/** Converts formatted raw input (a `String`, `Array[Byte]`, etc.) to an intermediate form ready for
	  * parsing ('casting') using this format's [[net.noresttherein.sugar.format.Format.Mold molds]].
	  * This conversion is assumed to be fast (at most `O(log n)`) and introduce faster methods for views
	  * over portions of `raw` data.
	  *
	  * While [[net.noresttherein.sugar.format.Format.melt melt]] is not typically expected to throw exceptions,
	  * as it is not the place where actual parsing starts, in order to guarantee that `Mold` methods returning
	  * an [[net.noresttherein.sugar.vars.Opt Opt]] do not unexpectedly result in an exception,
	  * this method is not allowed to throw exceptions for any reason related to the argument (i.e., repeatable errors).
	  * Errors result in returning [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * @return `Opt.`[[net.noresttherein.sugar.vars.Opt.guard guard]]`(cool)(liquid)`.
	  */
	def meltOpt(raw :Raw) :Opt[Liquid] = Opt.guard(melt _)(raw)

	/** Converts formatted raw input (a `String`, `Array[Byte]`, etc.) to an intermediate form ready for
	  * parsing ('casting') using this format's [[net.noresttherein.sugar.format.Format.Mold molds]].
	  * This conversion is assumed to be fast (at most `O(log n)`) and introduce faster methods for views
	  * over portions of `raw` data.
	  *
	  * While [[net.noresttherein.sugar.format.Format.melt melt]] is not typically expected to throw exceptions,
	  * as it is not the place where actual parsing starts, in order to guarantee that `Mold` methods returning
	  * a [[net.noresttherein.sugar.vars.Fallible Fallible]] do not unexpectedly result in an exception,
	  * this method is not allowed to throw exceptions for any reason related to the argument (i.e., repeatable errors).
	  * Errors result in returning [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] with the exception's message.
	  * @return `Fallible.`[[net.noresttherein.sugar.vars.Fallible.guard guard]]`(melt)(liquid)`.
	  */
	def guardMelt(raw :Raw) :Fallible[Liquid] = Fallible.guard(melt _)(raw)

	/** Converts an intermediate formatting ('melting') result of some Scala object into the final raw format.
	  * This conversion is assumed to work in terms of data structures only rather than the contents.
	  * The data is assumed to have been already correctly formatted and the method should not attempt to do so
	  * unless it is strictly necessary.
	  * @see [[net.noresttherein.sugar.format.Format.melt]]
	  */
	@throws[FormatException]("if the value cannot be converted to the raw form.")
	def cool(liquid :Liquid) :Raw

	/** Safely converts an intermediate formatting ('melting') result of some Scala object into the final raw format.
	  *
	  * While [[net.noresttherein.sugar.format.Format.cool cool]] is not typically expected to throw exceptions,
	  * as it is not the place where actual parsing starts, in order to guarantee that `Mold` methods returning
	  * an [[net.noresttherein.sugar.vars.Opt Opt]] do not unexpectedly result in an exception,
	  * this method is not allowed to throw exceptions for any reason related to the argument (i.e., repeatable errors).
	  * Errors result in returning [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * @return `Opt.`[[net.noresttherein.sugar.vars.Opt.guard guard]]`(cool)(liquid)`.
	  */
	def coolOpt(liquid :Liquid) :Opt[Raw] = Opt.guard(cool _)(liquid)

	/** Converts an intermediate formatting ('melting') result of some Scala object into the final raw format,
	  * propagating errors in the functional manner.
	  *
	  * While [[net.noresttherein.sugar.format.Format.cool cool]] is not typically expected to throw exceptions,
	  * as it is not the place where actual parsing starts, in order to guarantee that `Mold` methods returning
	  * a [[net.noresttherein.sugar.vars.Fallible Fallible]] do not unexpectedly result in an exception,
	  * this method is not allowed to throw exceptions for any reason related to the argument (i.e., repeatable errors).
	  * Errors result in returning [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] with the exception's message.
	  * @return `Fallible.`[[net.noresttherein.sugar.vars.Fallible.guard guard]]`(cool)(liquid)`.
	  */
	def guardCool(liquid :Liquid) :Fallible[Raw] = Fallible.guard(cool _)(liquid)

	/** The format of empty input data, such that
	  * [[net.noresttherein.sugar.format.Format.concat concat]]`(empty, liquid) == liquid`.
	  */
	protected val emptyLiquid :Liquid

	/** The format of raw empty input data.
	  * @return [[net.noresttherein.sugar.format.Format.cool cool]]`(`[[net.noresttherein.sugar.format.Format.emptyLiquid emptyLiquid]]`)`.
	  */
	protected def emptyRaw :Raw = cool(emptyLiquid)

	/** Checks if the formatted input data is empty. 'Empty' here needs not to be the same as the meaning
	  * of 'empty' defined by `Liquid` type itself: for example, a `String` consisting solely of whitespace characters,
	  * or containing only a comment element ignored during parsing may be considered empty by a `Format` implementation.
	  */
	protected def isEmpty(liquid :Liquid) :Boolean = liquid == emptyLiquid

	/** Appends a freshly formatted part of a larger object to the previously formatted output.
	  * This method is used by all [[net.noresttherein.sugar.format.Format.Mold molds]] of this format
	  * in order to implement methods such as [[net.noresttherein.sugar.format.Format.Mold.append append]].
	  *
	  * Molds created with a [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] are a special case:
	  * each [[net.noresttherein.sugar.format.Format.Part Part]] defined in a ''for comprehension'' used
	  * as the `Mold`'s definition is formatted separately and needs joining with previously formatted parts
	  * which might involve more than a simple concatenation (for example, a JSON format would need to insert ','
	  * between the properties). The responsibility for implementing this lies with
	  * [[net.noresttherein.sugar.format.Format.propertyMold propertyMold]] itself, which is used to adapt
	  * molds for property values as individual objects to a form embeddable in the format for the whole model.
	  * @param prefix result of formatting already processed (in the format order) parts of an input model object.
	  * @param suffix result of formatting an individual part/property of the formatted input object.
	  */
	@throws[FormatException]("if the format of prefix or suffix is invalid.")
	protected def concat(prefix :Liquid, suffix :Liquid) :Liquid

	/** Safely appends a freshly formatted part of a larger object to the previously formatted output.
	  * This method is used by all [[net.noresttherein.sugar.format.Format.Mold molds]] of this format
	  * in order to implement methods such as [[net.noresttherein.sugar.format.Format.Mold.append append]].
	  *
	  * While [[net.noresttherein.sugar.format.Format.concat concat]] is not typically expected
	  * to throw an exception as it does not need to perform any validation of the arguments,
	  * and [[net.noresttherein.sugar.format.Format.Liquid Liquid]] is normally a type with no dependence
	  * on application's domain, in order to guarantee that `Mold` methods returning
	  * an [[net.noresttherein.sugar.vars.Opt Opt]] do not unexpectedly result in an exception,
	  * this method is not allowed to throw exceptions for any reason related to the arguments (i.e., repeatable errors).
	  * Errors result in returning [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * @param prefix result of formatting already processed (in the format order) parts of an input model object.
	  * @param suffix result of formatting an individual part/property of the formatted input object.
	  * @return `Opt.`[[net.noresttherein.sugar.vars.Opt.guard guard]]`(concat(prefix, suffix))`.
	  */
	protected def concatOpt(prefix :Liquid, suffix :Liquid) :Opt[Liquid] =
		try { Got(concat(prefix, suffix)) } catch {
			case _ :Exception => Lack
		}

	/** Appends a freshly formatted part of a larger object to the previously formatted output, reporting errors
	  * in the functional manner.
	  * This method is used by all [[net.noresttherein.sugar.format.Format.Mold molds]] of this format
	  * in order to implement methods such as [[net.noresttherein.sugar.format.Format.Mold.append append]].
	  *
	  * While [[net.noresttherein.sugar.format.Format.concat concat]] is not typically expected
	  * to throw an exception as it does not need to perform any validation of the arguments,
	  * and [[net.noresttherein.sugar.format.Format.Liquid Liquid]] is normally a type with no dependence
	  * on application's domain, in order to guarantee that `Mold` methods returning
	  * an [[net.noresttherein.sugar.vars.Opt Opt]] do not unexpectedly result in an exception,
	  * this method is not allowed to throw exceptions for any reason related to the arguments (i.e., repeatable errors).
	  * Errors result in returning [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] with the exception's message.
	  * @param prefix result of formatting already processed (in the format order) parts of an input model object.
	  * @param suffix result of formatting an individual part/property of the formatted input object.
	  * @return `Fallible.`[[net.noresttherein.sugar.vars.Fallible.guard guard]]`(concat(prefix, suffix))`.
	  */
	protected def guardConcat(prefix :Liquid, suffix :Liquid) :Fallible[Liquid] =
		try { Passed(concat(prefix, suffix)) } catch {
			case e :Exception => Failed(e)
		}

	/** A mold for a property of type `S` of some larger molded model. It uses the implicit `Mold[S]` and adds
	  * to the formatted [[net.noresttherein.sugar.format.Format.Liquid liquid]] information identifying
	  * the formatted property. For example, a JSON mold would represent strings
	  * `s"'$propertyName' : ${valueMold.melt(value)}"` or `s", '$propertyName' : ${valueMold.melt(value)}"`.
	  *
	  * This method is used by every property [[net.noresttherein.sugar.format.Format.Part Part]] created
	  * by one of [[net.noresttherein.sugar.format.Format.Parts Parts]] factory methods to convert the implicit
	  * mold for the property type to a form embeddable within the raw format of the property owner:
	  * {{{
	  *     for {
	  *         dragon <- format[Dragon]  //creates a mold named "Dragon", from the reflected class name
	  *         level  <- dragon(_.level) //uses propertyMold("level")(Mold[Int]) (from the reflected property name)
	  *         color  <- dragon("color")(_.color) //uses propertyMold("color")(Mold[String]) (from the given part name)
	  *     } yield Dragon(level, color)
	  * }}}
	  * @param propertyName the name of the molded property, as potentially used by the returned property mold
	  *                     and entity mold containing it.
	  * @param valueMold    the mold defining how the property type itself is formatted, without any information
	  *                     about which property it is and how does it feature in the format of the containing entity.
	  */ //consider: making the three public
	protected def propertyMold[M](propertyName :String)(implicit valueMold :Mold[M]) :Mold[M]

	/** A mold for the 'opening' tag wrapping a formatted element, for example `<name>` in XML.
	  * Molds returned by [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] wrap
	  * content molds created by [[net.noresttherein.sugar.format.Format.Part Part]] quasi-monads
	  * in a pair of `open` and [[net.noresttherein.sugar.format.Format.close close]] fragments.
	  * In other formats, it may be an [[net.noresttherein.sugar.format.Format.Mold.empty empty]] mold.
	  */
	protected val open  :Mold[String]

	/** A mold for the 'closing' tag wrapping a formatted element, for example `</name>` in XML.
	  * Molds returned by [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] wrap
	  * content molds created by [[net.noresttherein.sugar.format.Format.Part Part]] quasi-monads
	  * in a pair of [[net.noresttherein.sugar.format.Format.open open]] and `close` fragments.
	  * In other formats, it may be an [[net.noresttherein.sugar.format.Format.Mold.empty empty]] mold.
	  */
	protected val close :Mold[String]

	/** A mold wrapping the [[net.noresttherein.sugar.format.Format.Liquid liquid]] format of `S` as defined by
	  * mold `parts` with fragments molded by [[net.noresttherein.sugar.format.Format.open open]]
	  * and [[net.noresttherein.sugar.format.Format.close close]] from the given mold `name`.
	  * It is an opportunity for something like XML format to wrap the contents of an element in an element tag.
	  *
	  * This method is used by all molds created using a [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]],
	  * with the `name` argument given to [[net.noresttherein.sugar.format.Format.apply[S](name:String) Format]]`(name)`
	  * (or inferred from the class name of the model) used for the `name` parameter here. The `parts` mold
	  * is the mold created by monadic composition of all constituent properties of the model, as returned by
	  * the function argument to `Moldmaker.`[[net.noresttherein.sugar.format.Format.MoldmakerTemplate.flatMap flatMap]].
	  * It can be also used to provide other, additional functionality, for example prettyfying the formatted output.
	  * @param name  The name of the mold and/or the molded type. It is used both in the created mold's `toString`
	  *              method for debugging and, potentially, in the formatted liquid as the model name.
	  * @param parts a mold for all properties of `S`.
	  */
	protected def wrap[M](name :String)(parts :Mold[M]) :Mold[M] =
		if (open == Mold.emptyString && close == Mold.emptyString)
			new RenamedMold(name, parts)
		else {
			class WrapperMold(override val name :String) extends SpecialNamedMold[M](name) {
				override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) = {
					val (opened, _, body)    = open.advance(name)(prefix, suffix)
					val (parsed, model, end) = parts.advance(emptyLiquid, body)
					val (closed, _, rem)     = close.advance(name)(parsed, end)
					(concat(opened, closed), model, rem)
				}
				override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
					for {
						(opened, _, body)    <- open.advanceOpt(name)(prefix, suffix)
						(parsed, model, end) <- parts.advanceOpt(emptyLiquid, body)
						(closed, _, rem)     <- close.advanceOpt(name)(parsed, end)
						full                 <- concatOpt(opened, closed)
					} yield (full, model, rem)

				override def guardAdvance(prefix :Liquid, suffix :Liquid) :Fallible[(Liquid, M, Liquid)] =
					open.guardAdvance(name)(prefix, suffix) flatMap { case (opened, _, body) =>
						parts.guardAdvance(emptyLiquid, body) flatMap { case (parsed, model, end) =>
							close.guardAdvance(name)(parsed, end) flatMap { case (closed, _, rem) =>
								guardConcat(opened, closed) map ((_, model, rem))
							}
						}
					}
/*
					for { //currently doesn't work because it requires Fallible.withFilter
						(opened, _, body)    <- open.guardAdvance(name)(prefix, suffix)
						(parsed, model, end) <- parts.guardAdvance(opened, body)
						(closed, _, rem)     <- close.guardAdvance(name)(parsed, end)
						full                 <- guardConcat(opened, closed)
					} yield (full, model, rem)
*/

				override def append(prefix :Liquid, model :M) :Liquid =
					close.append(parts.append(open.append(prefix, name), model), name)

				override def appendOpt(prefix :Liquid, model :M) :Opt[Liquid] =
					for {
						opened  <- open.appendOpt(prefix, name)
						written <- parts.appendOpt(opened, model)
						closed  <- close.appendOpt(written, name)
					} yield closed

				override def guardAppend(prefix :Liquid, model :M) :Fallible[Liquid] =
					for {
						opened  <- open.guardAppend(prefix, name)
						written <- parts.guardAppend(opened, model)
						closed  <- close.guardAppend(written, name)
					} yield  closed
			}
			new WrapperMold(name)
		}

}






/** A factory of [[net.noresttherein.sugar.format.Format.Mold Mold]] factories which leverages monadic composition
  * to define layouts of modeled entities abstracting over actual [[net.noresttherein.sugar.format.Format! Format]]
  * instances. Creation of a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]] uses the same process
  * as creating a mold for a particular format using a [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]],
  * with only the first/outermost expression in the composition changing from `s <- format[S]` to `s <- Format[S]`.
  * Each such `MoldLayout` can be then applied to any `Format` instance to create a mold in that format.
  */
object Format { //todo: move most of it to a FormatFactory base class and reuse with ReflectingFactory
	/** Creates a `MoldLayoutMaker`, a monadic builder
	  * for a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]`[M], a factory
	  * of [[net.noresttherein.sugar.format.Format.Mold Mold]]s responsible for parsing and formatting
	  * values of type `M`. It contains a single method
	  * [[net.noresttherein.sugar.format.Format.MoldLayoutMaker.flatMap flatMap]], which takes a constructor
	  * of a ''partial'' `Mold[S]`, used to read and write solely the constants of its models,
	  * and wrapping it up to a complete, final `MoldLayout[M]`.
	  *
	  * This process is exactly the same as building a `Mold` for a specific
	  * `format :`[[net.noresttherein.sugar.format.Format! Format]] with its
	  * [[net.noresttherein.sugar.format.Format!.apply(name:String) apply]] method of the same signature:
	  * `format[M](name)`. The only differences are that it starts with calling this method on
	  * object [[net.noresttherein.sugar.format.Format$ Format]], and that the final created object
	  * is a `MoldLayout[M]` rather than a `Mold[M]`. This allows to use them to create `Mold`s for arbitrary
	  * `Format` instances. See [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] documentation
	  * for details on composing the layout out of molds for individual properties of a model `M`.
	  * @param name A name representing the type of the molded model in the formatted data.
	  *             It is used as an argument to `open` and `close` molds used to demarcate the beginning and end
	  *             of the form created by the returned `MoldLayoutMaker`, for example to write/validate XML tag
	  *             name. Not all formats use this parameter in any functional way, but it is also used
	  *             in `toString` of the created `Mold`, for example "XML[Witch]".
	  */
	def apply[M](name :String) :MoldLayoutMaker[M] = new NamedMoldLayoutMaker(name)

	/** Creates a `MoldLayoutMaker`, a monadic builder
	  * for a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]`[M]`, a factory of
	  * [[net.noresttherein.sugar.format.Format.Mold Mold]]s responsible for parsing and formatting values
	  * of the specified class. This method forwards to its overloaded variant accepting the model's name,
	  * passing it [[net.noresttherein.sugar.prettyprint.extensions.ClassNameExtension.innerName inner class name]]
	  * (stripped of all packages, enclosing objects or classes and demangled) of the specified class.
	  * See that method's documentation for more details.
	  * @param modelClass The class of model objects with formats defined by layouts created
	  *                   with the returned `MoldLayoutMaker`.
	  * @see [[net.noresttherein.sugar.format.Format.Moldmaker]]
	  */
	def apply[M](modelClass :Class[M]) :MoldLayoutMaker[M] = apply(modelClass.innerName)

	/** Creates a `MoldLayoutMaker`, a monadic builder
	  *  for a [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]], a factory
	  * of [[net.noresttherein.sugar.format.Format.Mold! Mold]]s responsible for parsing and formatting values
	  * of the specified type argument. This method forwards to its overloaded variant accepting the model's name,
	  * passing it [[net.noresttherein.sugar.prettyprint.extensions.ClassNameExtension.innerName inner class name]]
	  * (stripped of all packages, enclosing objects or classes and demangled) of the class
	  * provided by the implicit [[scala.reflect.ClassTag ClassTag]]`[M]`.
	  * @see [[net.noresttherein.sugar.format.Format.Moldmaker]]
	  */
	def apply[M :ClassTag] :MoldLayoutMaker[M] = apply(classTag[M].runtimeClass.asInstanceOf[Class[M]])


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
	  */
	trait MoldLayout[M] extends Any with Serializable {
		/** Creates a mold of the described model type for the specified format. */
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
		  * A `Fallible[M]` instance is available in the layout, rather than being implicitly unwrapped.
		  * If parsing fails, regardless of the used [[net.noresttherein.sugar.format.Format.Mold Mold]] method,
		  * nothing is consumed and [[net.noresttherein.sugar.vars.Opt.Lack Lack]] is returned.
		  * Conversely, if `Lack` is melted by a created mold, nothing is appended to the output.
		  */
		def guard :MoldLayout[Fallible[M]] =
			new MoldLayout[Fallible[M]] {
				override def apply(format :Format) :format.Mold[Fallible[M]] = MoldLayout.this(format).guard
				override def toString :String = MoldLayout.this.toString + ".fallible"
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
		def name :String
		override def apply(format :Format) :format.NamedMold[M]

		override def map[O](read :M => O, write :O => M) :MoldLayout[O] =
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
		override def guard :NamedMoldLayout[Fallible[M]] =
			new AbstractNamedMoldLayout[Fallible[M]](name) {
				override def apply(format :Format) :format.NamedMold[Fallible[M]] =
					NamedMoldLayout.this(format).guard
			}

		override def toString :String = "MoldLayout[" + name + "]"
	}


	@SerialVersionUID(ver)
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

		@SerialVersionUID(ver)
		private class EmptyMoldLayout[S](value :S) extends MoldLayout[S] {
			override def apply(format :Format) = format.Mold.empty(value)
			override def toString = Format.this.toString + "MoldLayout.empty(" + value + ")"
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


	/** Factory methods for creating [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s from scratch. */
	trait MoldLayoutFactory[M] extends Serializable {
		//in Scala 3 we will get rid of it and instead have flatMap(mold :(parts :Parts[M]) => parts.format.Mold[M]))
		/** A mock `Format` instance standing for 'any `Format`'.
		  * It allows to define [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s
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
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] instance.
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
		def guard(advance :(Liquid, Liquid) => Fallible[(Liquid, M, Liquid)], append :(Liquid, M) => Fallible[Liquid])
				:MoldLayout[M]

		/** The simplest factory method for a `Mold`,accepting functions necessary to implement
		  * a [[net.noresttherein.sugar.format.Format.Mold Mold]] parsing and formatting values of this maker's
		  * model type. They must not throw any exceptions, and instead return
		  * a [[net.noresttherein.sugar.vars.Fallible.Failed Failed]] instance.
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
		def guard(split :Liquid => Fallible[(Liquid, M, Liquid)], melt :M => Fallible[Liquid]) :MoldLayout[M]
	}


	/** A factory of [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s,
	  * themselves factories of [[net.noresttherein.sugar.format.Format.Mold! Mold]]s
	  * for any [[net.noresttherein.sugar.format.Format Format]] given as the argument.
	  * This allows reusing a once defined the structure of formatted model object,
	  * and create actual parser/formatters for multiple formats, such as XML and JSON, for example.
	  *
	  * It allows to define layouts in two ways:
	  *   1. Monadic-like composition using its [[net.noresttherein.sugar.format.Format.MoldLayoutMaker.flatMap flatMap]],
	  *      In the exact same way as [[net.noresttherein.sugar.format.Format.Moldmaker Moldmaker]] -
	  *      see that type's documentation for more details. Example:
	  *      {{{
	  *          for {
	  *             dragon <- Format[Dragon]
	  *             name   <- dragon("name")(_.name)
	  *             color  <- dragon("color")(_.color)
	  *             level  <- dragon("level")(_.level)
	  *          } yield Dragon(name, color, level)
	  *      }}}
	  *   1. Factory methods creating adapter `Mold`s based on functions passed as arguments,
	  *      the same way that individual `Mold`s can be created by analogical factory methods
	  *      of [[net.noresttherein.sugar.format.Format.Mold$ Mold]] object from any `Format` instance.
	  *
	  * A `MoldLayoutMaker[S]` can be created by factory `apply` methods defined
	  * in singleton object [[net.noresttherein.sugar.format.Format$ Format]]; their signatures are exactly the same
	  * as those of factory methods defined in ''class'' [[net.noresttherein.sugar.format.Format Format]]
	  * (available on any `Format` instance), except they return
	  * [[net.noresttherein.sugar.format.Format.MoldLayout MoldLayout]]s instead of `Mold`s.
	  */
	trait MoldLayoutMaker[M] extends MoldLayoutFactory[M] {
		import fmt.{Mold, Parts}

		/** Takes a `Mold` constructor for internals of type `M` and wraps it up as a universal mold layout of that type.
		  * @see [[net.noresttherein.sugar.format.Format.wrap]]
		  */
		def flatMap(mold :Parts[M] => Mold[M]) :MoldLayout[M]
	}


	private class NamedMoldLayoutMaker[M](name :String)
		extends NamedMoldLayoutFactory[M](name) with MoldLayoutMaker[M]
	{
		import fmt.{Liquid, Mold, Parts}
		override val fmt :Format = null

		override def flatMap(mold :Parts[M] => Mold[M]) :MoldLayout[M] =
			new MoldLayout[M] {
				override def apply(format :Format) :format.Mold[M] =
					format[M](name).flatMap(mold.asInstanceOf[format.Parts[M] => format.NamedMold[M]])
			}
		override def toString = "MoldLayoutMaker[" + name + "]"
	}


	@SerialVersionUID(ver)
	private class MappedMoldLayout[X, Y](layout :MoldLayout[X], read :X => Y, write :Y => X) extends MoldLayout[Y] {
		override def apply(format :Format) = layout(format).map(read, write)
		override def toString = layout.toString + ".map"
	}
	@SerialVersionUID(ver)
	private class NamedMappedMoldLayout[X, Y](override val name :String, layout :MoldLayout[X],
	                                          read :X => Y, write :Y => X)
		extends MappedMoldLayout[X, Y](layout, read, write) with NamedMoldLayout[Y]
	{
		def this(layout :NamedMoldLayout[X], read :X => Y, write :Y => X) =
			this("map(" + layout.name + ")", layout, read, write)
		override def apply(format :Format) = layout(format).map(name, read, write)
	}



	/** An enumeration with constants identifying the actions of parsing and formatting. */
	object MoldingProcess extends Enumeration {
		/** Represents parsing. */
		final val Casting = Value("Casting")
		/** Represents formatting. */
		final val Melting = Value("Melting")
	}

	/** One of [[net.noresttherein.sugar.format.Format.MoldingProcess.Casting Casting]]
	  * or [[net.noresttherein.sugar.format.Format.MoldingProcess.Melting Melting]] - switch values giving information
	  * about which of the two actions is taking place.
	  */
	type MoldingProcess = MoldingProcess.Value
}




private abstract class AbstractNamedMoldLayout[S](override val name :String) extends NamedMoldLayout[S]


@SerialVersionUID(ver)
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

	override def guard(advance :(Liquid, Liquid) => Fallible[(Liquid, M, Liquid)],
	                   append :(Liquid, M) => Fallible[Liquid]) =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) =
				format.asInstanceOf[fmt.type].Mold.guard(this.name, advance, append).asInstanceOf[format.NamedMold[M]]
		}
	override def guard(split :fmt.Liquid => Fallible[(fmt.Liquid, M, fmt.Liquid)], melt :M => Fallible[fmt.Liquid])
			:MoldLayout[M] =
		new AbstractNamedMoldLayout[M](name) {
			override def apply(format :Format) =
				format.asInstanceOf[fmt.type].Mold.guard(this.name, split, melt).asInstanceOf[format.NamedMold[M]]
		}

	override def toString = "MoldLayoutFactory[" + name + "]"
}
