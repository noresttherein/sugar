package net.noresttherein.sugar.vars

import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.reflect.Specialized.Fun1
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Maybe.Yes
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.undefined




/** A serializable `Function0[T]` adapted to `Ref` interface.
  * It is a ''single abstract method'' type,
  * leaving only [[net.noresttherein.sugar.vars.Ref.apply apply]]`()` to implement by subclasses.
  * @define Ref `Eval`
  * @define ref by-name value
  */
trait Eval[@specialized(SpecializedVars) +T] extends Ref[T] with (() => T) with Serializable {
	override def isFinal       = false
	override def isEmpty       = false
	override def isConst       = false
	override def isDefined     = true
	override def isDefinite    = true
	override def isFinalizable = false

	override def value :T = apply()
	override def get   :T = apply()
	override def const :T = unsupported_!("Eval.const")

	override def option :Option[T] =
		try { Some(value) } catch {
			case _ :NoSuchElementException => None
		}
	@inline final override def toOption    :Option[T] = option
	@inline final override def constOption :Option[T] = None

	override def opt :Opt[T] =
		try One(value) catch {
			case _ :NoSuchElementException => None
		}
	@inline final override def toOpt    :Opt[T] = opt
	@inline final override def constOpt :Opt[T] = None

	override def unsure :Unsure[T] =
		try { Sure(value) } catch {
			case _ :NoSuchElementException => Missing
		}
	@inline final override def toUnsure    :Unsure[T] = unsure
	@inline final override def unsureConst :Unsure[T] = Missing

	override def mkString :String = mkString("Eval")
	override def toString :String = //should it evaluate and print it?
		try String.valueOf(value) catch {
			case _ :NoSuchElementException => undefined.toString
		}
}


@SerialVersionUID(Ver)
case object Eval {
	@inline def apply[@specialized(SpecializedVars) T](eval :Eval[T]) :Eval[T] = eval

	def eval[@specialized(SpecializedVars) T](eval : => T) :Eval[T] = () => eval
}




/** A serializable `Function0[Opt[T]]` adapted to `Ref[T]` interface.
  * It is a ''single abstract method'' type,
  * leaving only [[net.noresttherein.sugar.vars.EvalOpt.eval eval]] to implement by subclasses
  * to return the value for [[net.noresttherein.sugar.vars.Ref.opt opt]].
  * @define Ref `EvalOpt`
  */
trait EvalOpt[@specialized(SpecializedVars) +T] extends Ref[T] with (() => T) with Serializable {
	override def isFinal       = false
	override def isEmpty       = false
	override def isConst       = false
	override def isDefined     = false
	override def isDefinite    = false
	override def isFinalizable = false

	override def value :T = maybe match {
		case Yes(v) => v
		case _      => noSuch_!("EvalOpt.value")
	}

	@inline final override def get     :T = value
	@inline final override def apply() :T = value
	override def const :T = unsupported_!("EvalOpt.const")

	protected def eval() :Opt[T]

	@inline final override def opt :Opt[T] = eval()
	@inline final override def toOpt :Opt[T] = eval()
	@inline final override def constOpt :Opt[T] = None

	def map[@specialized(Fun1) O](f :T => O) :EvalOpt[O] = () => eval().map(f)
	def flatMap[@specialized(SpecializedVars) O](f :T => EvalOpt[O]) :EvalOpt[O] = () => eval().flatMap(f(_).opt)

	override def mkString :String = mkString("EvalOpt")
	override def toString :String = //should it evaluate and print it?
		try maybe.mapOrElse(_.toString, undefined.toString) catch {
			case _ :NoSuchElementException => undefined.toString
		}
}


@SerialVersionUID(Ver)
case object EvalOpt {
	@inline def apply[@specialized(SpecializedVars) T](eval :EvalOpt[T]) :EvalOpt[T] = eval

	def eval[@specialized(SpecializedVars) T](generator : => Opt[T]) :EvalOpt[T] = () => generator
}
