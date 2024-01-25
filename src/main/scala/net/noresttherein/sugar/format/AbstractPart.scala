package net.noresttherein.sugar.format


/*
	private abstract class AbstractPart[M, V, P](modelName :String, partName :String, valueMold :Mold[V])
		extends Part[M, P]
	{
		protected def newPart(prefix :Liquid, model :V, suffix :Liquid) :P
		protected def newPart(prefix :Liquid, model :M) :P = newPart(prefix, newValue(prefix, model), emptyLiquid)
		protected def newValue(prefix :Liquid, model :M) :V
		override def map(construct :P => M) :Mold[M] =
			new SpecialMold[M] with NamedMold[M] {
				override def name = modelName
				override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) = {
					val (parsed, value, unparsed) = valueMold.advance(prefix, suffix)
					val model = construct(newPart(parsed, value, unparsed))
					(parsed, model, unparsed)
				}
				override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
					valueMold.advanceOpt(prefix, suffix) match {
						case Got((parsed, value, unparsed)) =>
							Got(parsed, construct(newPart(prefix, value, suffix)), unparsed)
						case _ => Lack
					}
				override def guardAdvance(prefix :Liquid, suffix :Liquid) :Outcome[(Liquid, M, Liquid)] =
					valueMold.guardAdvance(prefix, suffix) match {
						case Done((parsed, value, unparsed))  =>
							Done(parsed, construct(newPart(parsed, value, unparsed)), unparsed)
						case fail :Failed => fail
					}
				override def append(prefix :Liquid, model :M) :Liquid = {
					val value = newValue(prefix, model)
					valueMold.append(prefix, value)
				}
				override def appendOpt(prefix :Liquid, model :M) = {
					val value = newValue(prefix, model)
					valueMold.appendOpt(prefix, value)
				}
				override def guardAppend(prefix :Liquid, model :M) = {
					val value = newValue(prefix, model)
					valueMold.guardAppend(prefix, value)
				}
				override def toString = mapMoldString(modelName, partName)
			}

		override def flatMap(construct :P => Mold[M]) :Mold[M] =
			new SpecialMold[M] with NamedMold[M] {
				override def name = modelName
				override def advance(prefix :Liquid, suffix :Liquid) :(Liquid, M, Liquid) = {
					val (parsed, value, unparsed) = valueMold.advance(prefix, suffix)
					val mold = construct(newPart(parsed, value, unparsed))
					mold.advance(parsed, unparsed)
				}
				override def advanceOpt(prefix :Liquid, suffix :Liquid) :Opt[(Liquid, M, Liquid)] =
					valueMold.advanceOpt(prefix, suffix) match {
						case Got((parsed, value, unparsed)) =>
							val mold = construct(newPart(parsed, value, unparsed))
							mold.advanceOpt(parsed, unparsed)
					}
				override def guardAdvance(prefix :Liquid, suffix :Liquid) :Outcome[(Liquid, M, Liquid)] =
					valueMold.guardAdvance(prefix, suffix) match {
						case Done((parsed, value, unparsed)) =>
							val mold = construct(newPart(parsed, value, unparsed))
							mold.guardAdvance(parsed, unparsed)
					}
				override def append(prefix :Liquid, model :M) = {
					val value = newValue(prefix, model)
					val liquid = valueMold.append(prefix, value)
					val part = newPart(prefix, value, emptyLiquid)
					construct(part).append(liquid, model)
				}
				override def appendOpt(prefix :Liquid, model :M) = {
					val value = newValue(prefix, model)
					val part = newPart(prefix, value, emptyLiquid)
					valueMold.appendOpt(prefix, value) match {
						case Got(liquid) => construct(part).appendOpt(liquid, model)
						case _           => Lack
					}
				}
				override def guardAppend(prefix :Liquid, model :M) = {
					val value = newValue(prefix, model)
					val part = newPart(prefix, value, emptyLiquid)
					valueMold.guardAppend(prefix, value) match {
						case Done(liquid) => construct(part).guardAppend(liquid, model)
						case fail :Failed => fail
					}
				}
				override def toString = flatMapMoldString(modelName, partName)
			}

		override def filter(predicate :P => Boolean) :Part[M, P] =
			new FilteredPartBase[M, P](modelName, partName) {
				override def map(construct :P => M) =
					new SpecialMold[M] with NamedMold[M] {
						override def name = modelName
						override def advance(prefix :Liquid, suffix :Liquid) = {
							val (parsed, value, unparsed) = valueMold.advance(prefix, suffix)
							val part = newPart(parsed, value, unparsed)
							if (predicate(part))
								(parsed, construct(part), unparsed)
							else
								parseError(this, suffix)
						}
						override def advanceOpt(prefix :Liquid, suffix :Liquid) =
							valueMold.advanceOpt(prefix, suffix) match {
								case Got((parsed, value, unparsed)) =>
									val part = newPart(parsed, value, unparsed)
									if (predicate(part))
										Got((parsed, construct(part), unparsed))
									else
										Lack
								case _ => Lack
							}
						override def guardAdvance(prefix :Liquid, suffix :Liquid) =
							valueMold.guardAdvance(prefix, suffix) match {
								case Done((parsed, value, unparsed)) =>
									val part = newPart(parsed, value, unparsed)
									if (predicate(part))
										Done((parsed, construct(part), unparsed))
									else
										Failed(() => parsingErrorMsg(suffix))
								case fail :Failed => fail
							}
						override def append(prefix :Liquid, model :M) = {
							val value = newValue(prefix, model)
							val part = newPart(prefix, value, emptyLiquid)
							if (predicate(part))
								valueMold.append(prefix, value)
							else
								formatError(this, model)
						}
						override def appendOpt(prefix :Liquid, model :M) = {
							val value = newValue(prefix, model)
							val part = newPart(prefix, value, emptyLiquid)
							if (predicate(part))
								valueMold.appendOpt(prefix, value)
							else
								Lack
						}
						override def guardAppend(prefix :Liquid, model :M) = {
							val value = newValue(prefix, model)
							val part = newPart(prefix, value, emptyLiquid)
							if (predicate(part))
								valueMold.guardAppend(prefix, value)
							else
								Failed(() => formattingErrorMsg(model))
						}
						override def toString = filteredMapMoldString(modelName, partName)
					}

				override def flatMap(construct :P => Mold[M]) =
					new SpecialMold[M] with NamedMold[M] {
						override def name = modelName
						override def advance(prefix :Liquid, suffix :Liquid) = {
							val (parsed, value, unparsed) = valueMold.advance(prefix, suffix)
							val part = newPart(prefix, value, suffix)
							if (predicate(part))
								construct(part).advance(parsed, unparsed)
							else
								parseError(this, suffix)
						}
						override def advanceOpt(prefix :Liquid, suffix :Liquid) =
							valueMold.advanceOpt(prefix, suffix) match {
								case Got((parsed, value, unparsed)) =>
									val part = newPart(prefix, value, suffix)
									if (predicate(part))
										construct(part).advanceOpt(parsed, unparsed)
									else
										Lack
								case _ => Lack
							}
						override def guardAdvance(prefix :Liquid, suffix :Liquid) =
							valueMold.guardAdvance(prefix, suffix) match {
								case Done((parsed, value, unparsed)) =>
									val part = newPart(prefix, value, suffix)
									if (predicate(part))
										construct(part).guardAdvance(parsed, unparsed)
									else
										Failed(() => parseErrorMsg(this, suffix))
								case fail :Failed => fail
							}
						override def append(prefix :Liquid, model :M) = {
							val value = newValue(prefix, model)
							val liquid = valueMold.append(prefix, value)
							val part = newPart(prefix, value, emptyLiquid)
							if (predicate(part))
								construct(part).append(liquid, model)
							else
								formatError(this, model)
						}
						override def appendOpt(prefix :Liquid, model :M) = {
							val value = newValue(prefix, model)
							valueMold.appendOpt(prefix, value) match {
								case Got(liquid)  =>
									val part = newPart(prefix, value, emptyLiquid)
									if (predicate(part))
										construct(part).appendOpt(liquid, model)
									else
										Lack
								case _ => Lack
							}
						}
						override def guardAppend(prefix :Liquid, model :M) = {
							val value = newValue(prefix, model)
							valueMold.guardAppend(prefix, value) match {
								case Done(liquid) =>
									val part = newPart(prefix, value, emptyLiquid)
									if (predicate(part))
										construct(part).guardAppend(liquid, model)
									else
										Failed(() => formatErrorMsg(this, model))
								case fail :Failed => fail
							}
						}

						override def toString = filteredFlatMapMoldString(modelName, partName)
					}

				override def filter(predicate2 :P => Boolean) =
					AbstractPart.this.filter(p => predicate(p) && predicate2(p))
			}

		private def partString = if (partName.length == 0 || partName == "_") "_" else partName

		override def toString :String =
			Format.this.toString + "[" + modelName + "]" + "." + partString
	}


	private class PropertyPart1[M, P](modelName :String = "_", partName :String = "_", get :M => P)
	                                 (implicit valueMold :Mold[P])
		extends AbstractPart[M, P, P](modelName, partName, valueMold)
	{
		protected override def newPart(prefix :Liquid, model :P, suffix :Liquid) = model
		protected override def newValue(prefix :Liquid, model :M) = get(model)
	}
	private class MirrorPart1[M, P](modelName :String = "_", partName :String = "_", get :M => P)
	                               (implicit valueMold :Mold[P])
		extends AbstractPart[M, P, (Liquid, P)](modelName, partName, valueMold)
	{
		protected override def newPart(prefix :Liquid, model :P, suffix :Liquid) = (prefix, model) //this is wrong
		protected override def newValue(prefix :Liquid, model :M) = (prefix, get(model))
	}
	private class PeekPart1[M, P](modelName :String = "_", partName :String = "_", get :M => P)
	                             (implicit valueMold :Mold[P])
		extends AbstractPart[M, P, Potential[P]](modelName, partName, valueMold.potential)
	{
		protected override def newPart(prefix :Liquid, model :P, suffix :Liquid) = ???
		protected override def newValue(prefix :Liquid, model :M) = ???
	}
*/
