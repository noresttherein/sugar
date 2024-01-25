package net.noresttherein.sugar.format

import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Properties

import net.noresttherein.sugar.collections.ChoppedString
import net.noresttherein.sugar.collections.extensions.StringExtension
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.format.FormatSpec.BreathWeapon.{Damage, DiceCount, DiceType}
import net.noresttherein.sugar.format.FormatSpec.Dragon.{Breath, Colour, Level, Name}
import net.noresttherein.sugar.format.FormatSpec.XMLProps.format
import net.noresttherein.sugar.vars.Outcome
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}


object FormatSpec extends Properties("format") {

	trait DamageType
	object DamageType {
		implicit val xmlMold :XML.Mold[DamageType] = XML.Mold.guard(
			(liquid :ChoppedString) =>
				if (liquid.length < 4)
					Failed("Input too short for DamageType: \"" + liquid + "\".")
				else liquid.substring(0, 4) match {
					case "fire" => Done(("fire".chopped, Fire, liquid.drop(4)))
					case "cold" => Done(("cold".chopped, Cold, liquid.drop(4)))
					case "acid" => Done(("acid".chopped, Acid, liquid.drop(4)))
					case tpe    => Failed("Illegal input for DamageType: \"" + tpe + "\"...")
				},
			(dmg :DamageType) => Done(dmg.innerClassName.toLowerCase.chopped)
		)
	}
	case object Fire extends DamageType
	case object Cold extends DamageType
	case object Acid extends DamageType

	case class BreathWeapon(damageType :DamageType, diceCount :Int, diceType :Int)
	object BreathWeapon {
		final val ClassName = "BreathWeapon"
		final val Damage    = "damageType"
		final val DiceCount = "diceCount"
		final val DiceType  = "diceType"
		implicit val xmlMold :XML.Mold[BreathWeapon] = for {
			weapon   <- XML[BreathWeapon]
			dmg      <- weapon(Damage)(_.damageType)
			dice     <- weapon(DiceCount)(_.diceCount)
			diceType <- weapon(DiceType)(_.diceType)
		} yield BreathWeapon(dmg, dice, diceType)
	}

	case class Dragon(name :String, colour :String, breath :BreathWeapon, level :Int)
	object Dragon {
		final val ClassName = "Dragon"
		final val Name   = "name"
		final val Colour = "colour"
		final val Breath = "breath"
		final val Level  = "level"

		val Instance :Dragon = Dragon("Firkraag", "red", BreathWeapon(Fire, 2, 6), 21)
		val xml :String =
			s"<$ClassName><$Name>Firkraag</$Name><$Colour>red</$Colour><$Breath>" +
			s"<${BreathWeapon.ClassName}><$Damage>fire</$Damage><$DiceCount>2</$DiceCount><$DiceType>6</$DiceType>" +
			s"</${BreathWeapon.ClassName}></$Breath><$Level>21</$Level></$ClassName>"

		implicit val xmlMold :XML.Mold[Dragon] = for {
			dragon <- XML[Dragon]
			name   <- dragon(Name)(_.name)
			colour <- dragon(Colour)(_.colour)
			breath <- dragon(Breath)(_.breath)
			level  <- dragon(Level)(_.level)
		} yield Dragon(name, colour, breath, level)

	}


	abstract class FormatProps(name :String, dragon :Dragon, formattedDragon :String) extends Properties(name) {
			val format :FormatAsString
			implicit val mold :format.Mold[Dragon]

			property("read") = try { dragon =? format.read[Dragon](formattedDragon) } catch {
				case e :ParsingException =>
					System.err.println(e)
					System.err.println("Liquid: \"" + e.liquid + "\"")
					e.printStackTrace(System.err)
					throw e
			}
			property("write") = formattedDragon =? format.write(dragon)
	}

	object XMLProps extends FormatProps("XML", Dragon.Instance, Dragon.xml) {
		override val format                             = XML
		implicit override val mold :format.Mold[Dragon] = Dragon.xmlMold

		property("reader") = {
			val formatted = for {
				reader <- format.reader(Dragon.xml)
				_      <- reader.expect(Dragon.ClassName)(format.open)
				name   <- reader.property[String](Name)
				colour <- reader.property[String](Colour)
				breath <- reader.property[BreathWeapon](Breath)
				level  <- reader.property[Int](Level)
				_      <- reader.expect(Dragon.ClassName)(format.close)
			} yield Dragon(name, colour, breath, level)
			Outcome(Dragon.Instance) =? formatted
		}
	}

	include(XMLProps)


	property("extensions.castMethods.as") = {
		import extensions.castMethods
		(Dragon.xml as XML[Dragon]) =? Dragon.Instance
	}
	property("extensions.castMethods.cast") = {
		import extensions._
		(Dragon.xml cast XML[Dragon]) =? Dragon.Instance
	}
	property("extensions.castMethods.castAs") = {
		import extensions._
		Dragon.xml.castAs[Dragon](XML) =? Dragon.Instance
	}

	property("extensions.meltMethods.as") = {
		import extensions._
		(Dragon.Instance as XML) =? Dragon.xml
	}
	property("extensions.meltMethods.melt") = {
		import extensions._
		(Dragon.Instance as XML) =? Dragon.xml
	}
	property("extensions.LiquidExtension.read") = {
		import extensions._
		implicit val format :XML.type = XML
		Dragon.Instance =? Dragon.xml.read[Dragon]
	}
	property("extensions.LiquidExtension.next") = {
		import extensions._
		implicit val format :XML.type = XML
		val xml = Dragon.xml + Dragon.xml
		(Dragon.Instance, Dragon.xml.chopped) =? xml.next[Dragon]
	}
}
