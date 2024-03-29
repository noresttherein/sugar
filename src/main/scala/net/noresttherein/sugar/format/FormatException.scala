package net.noresttherein.sugar.format

import net.noresttherein.sugar.exceptions.AbstractException
import net.noresttherein.sugar.reflect.prettyprint.{classNameOf, fullNameOf}




//todo: make the msg argument lazy
@SerialVersionUID(Ver)
class FormatException(val format :Format, msg :String, cause :Throwable = null,
                      enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends AbstractException(msg, cause, enableSuppression, writableStackTrace)
{
	override def className :String = classNameOf(this) + "[" + format + "]"
}


//todo: make the msg argument lazy
@SerialVersionUID(Ver)
class FormattingException(override val format :Format, val model :Any, msg :String, cause :Throwable = null,
                          enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends FormatException(format, msg, cause, enableSuppression, writableStackTrace)
{
	def this(format :Format, model :Any, cause :Throwable) =
		this(format, model, "Cannot format " + model + " as " + format + ". " + cause.getMessage, cause)

	def this(format :Format, model :Any) =
		this(format, model, "Cannot format " + model + " as " + format + ".", null)

	override def toString :String = {
		val msg = getLocalizedMessage
		if (msg == null) className + "(" + model + ")"
		else className + "(" + model + "): " + msg
	}
}

@SerialVersionUID(Ver)
object FormattingException {
	def apply(format :Format)(model :Any, cause :Throwable = null) :FormattingException =
		if (cause == null) new FormattingException(format, model)
		else new FormattingException(format, model, cause)
}


//todo: make the msg argument lazy
@SerialVersionUID(Ver)
abstract class ParsingException(override val format :Format, msg :String, cause :Throwable = null,
                                enableSuppression :Boolean = true, writableStackTrace :Boolean = true)
	extends FormatException(format, msg, cause, enableSuppression, writableStackTrace)
{
//	def this(format :Format)
	def liquid :format.Liquid
}

@SerialVersionUID(Ver)
object ParsingException {
	def expected(format :Format, name :String)(liquid :format.Liquid, cause :Throwable = null) :ParsingException = {
		//todo: don't include the full liquid, but only a prefix if it is long.
		val msg = "Failed to parse '" + liquid + "' as " + name + "."
		apply(format)(liquid, if (cause == null) msg else msg + " " + cause.getMessage, cause)
	}

//	def apply(format :Format)(liquid :format.Liquid, msg :String) :ParsingException =
//		apply(format)(liquid, msg, null)

	def illegal(format :Format)(liquid :format.Liquid, cause :Throwable = null) :ParsingException = {
		//todo: don't include the full liquid, but only a prefix if it is long.
		val msg = "Illegal " + format + ": '" + liquid + "'."
		apply(format)(liquid, if (cause == null) msg else msg + " " + cause.getMessage, cause)
	}

//	def illegal(format :Format)(liquid :format.Liquid, reason :String) :ParsingException = {
//
//	}

	def apply(format :Format)(liquid :format.Liquid, msg :String, cause :Throwable = null) :ParsingException = {
		val fmt :format.type = format
		val liq :fmt.Liquid = liquid
		new ParsingException(format, msg, cause) {
			override val format :fmt.type = fmt
			override val liquid = liq
			override def className = fullNameOf(classOf[ParsingException]) + "[" + format + "]"
		}
	}

}


