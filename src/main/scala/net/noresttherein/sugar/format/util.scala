package net.noresttherein.sugar.format

import net.noresttherein.sugar.extensions.classNameMethods




private object util {
	def mapMoldString(format :Format)(modelName :String, partName :String) :String =
		format.toString + "[" + modelName + "/" + partName + "]"

	def flatMapMoldString(format :Format)(modelName :String, partName :String) :String =
		format.toString + "[" + modelName + "/" + partName + "...]"

	def filteredMapMoldString(format :Format)(modelName :String, partName :String) :String =
		format.toString + "[filter(" + modelName + "/" + partName + ")]"

	def filteredFlatMapMoldString(format :Format)(modelName :String, partName :String) :String =
		format.toString + "[filter(" + modelName + "/" + partName + ")...]"

	def partString(format :Format)(part :format.Part[_, _], partName :String, partMold :format.Mold[_]) :String =
		if (partName.length == 0 || partName == "_") partMold match {
			case null =>
				"@" + part.hashCodeString
			case named :format.NamedMold[_] if named.name.length > 0 && named.name != "_" =>
				"@" + part.hashCodeString + ":" + named.name
			case _ =>
				"@" + part.hashCodeString
		} else
			partName

	def partString(format :Format, modelName :String)
	              (part :format.Part[_, _], partName :String, partMold :format.Mold[_]) :String =
		format.toString + "[" + modelName + "]." + partString(format)(part, partName, partMold)

	def filteredPartString(format :Format)
	                      (part :format.Part[_, _], modelName :String, partName :String, partMold :format.Mold[_]) :String =
		format.toString + "[" + modelName + "].filter(" + partString(format)(part, partName, partMold) + ")"

	def parseErrorMsg[S](format :Format)(mold :format.Mold[S], input :format.Liquid) :String =
		"Failed to parse '" + input + "' as " + mold + "."

	def formatErrorMsg[S](format :Format)(mold :format.Mold[S], model :S) :String =
		"Failed to format " + model + " as " + mold + "."

	def parseError[S](format :Format)(mold :format.Mold[S], input :format.Liquid) :Nothing =
		throw ParsingException(format :format.type)(input, parseErrorMsg(format)(mold, input))

	def formatError[S](format :Format)(mold :format.Mold[S], model :S) :Nothing =
		throw new FormattingException(format, model, formatErrorMsg(format)(mold, model))

}
