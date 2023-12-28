package net.noresttherein.sugar.format

import net.noresttherein.sugar.collections.ChoppedString
import net.noresttherein.sugar.collections.extensions.StringExtension
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.format.FormatSpec.BreathWeapon.{Damage, DiceCount, DiceType}
import net.noresttherein.sugar.vars.Fallible.{Failed, Passed}
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Properties



object FormatSpec extends Properties("Format") {

	trait DamageType
	object DamageType {
		implicit val xmlMold :XML.Mold[DamageType] = XML.Mold.guard(
			(liquid :ChoppedString) =>
				if (liquid.length < 4)
					Failed("Input too short for DamageType: \"" + liquid + "\".")
				else liquid.substring(0, 4) match {
					case "fire" => Passed(("fire".chopped, Fire, liquid.drop(4)))
					case "cold" => Passed(("cold".chopped, Cold, liquid.drop(4)))
					case "acid" => Passed(("acid".chopped, Acid, liquid.drop(4)))
					case tpe    => Failed("Illegal input for DamageType: \"" + tpe + "\"...")
				},
			(dmg :DamageType) => Passed(dmg.innerClassName.toLowerCase.chopped)
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


	def formatProperty(format :FormatAsString, dragon :Dragon, formattedDragon :String)
	                  (implicit dragonMold :format.Mold[Dragon]): Properties =
	{
		class FormatProps extends Properties(format.toString) {
			property("read") = try { dragon =? format.read[Dragon](formattedDragon) } catch {
				case e :ParsingException =>
					System.err.println(e)
					System.err.println("Liquid: \"" + e.liquid + "\"")
					e.printStackTrace(System.err)
					throw e
			}
			property("write") = formattedDragon =? format.write(dragon)
		}
		new FormatProps
	}


	include(formatProperty(XML, Dragon.Instance, Dragon.xml))
}
