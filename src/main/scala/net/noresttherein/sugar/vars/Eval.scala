package net.noresttherein.sugar.vars

import net.noresttherein.sugar.reflect.Specialized.{Fun1, Fun1Arg}
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Maybe.{Yes, No}
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
	override def const :T = throw new UnsupportedOperationException("Eval.const")

	override def option :Option[T] =
		try { Some(value) } catch {
			case _ :NoSuchElementException => None
		}
	@inline final override def toOption    :Option[T] = option
	@inline final override def constOption :Option[T] = None

	override def maybe :Maybe[T] =
		try { Yes(value) } catch {
			case _ :NoSuchElementException => No
		}
	@inline final override def toMaybe    :Maybe[T] = maybe
	@inline final override def maybeConst :Maybe[T] = No

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
object Eval {
	@inline def apply[@specialized(SpecializedVars) T](eval :Eval[T]) :Eval[T] = eval

	def eval[@specialized(SpecializedVars) T](eval : => T) :Eval[T] = () => eval
}




/** A serializable `Function0[T]` adapted to `Ref` interface.
  * It is a ''single abstract method'' type,
  * leaving only [[net.noresttherein.sugar.vars.Ref.opt opt]] to implement by subclasses.
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
		case _ => throw new NoSuchElementException("EvalOpt.value")
	}

	@inline final override def get     :T = value
	@inline final override def apply() :T = value
	override def const :T = throw new UnsupportedOperationException("EvalOpt.const")

	final override def maybe :Maybe[T] = eval().toMaybe
	@inline final override def toMaybe    :Maybe[T] = maybe
	@inline final override def maybeConst :Maybe[T] = No
	protected def eval() :Opt[T]

	@inline final override def opt :Opt[T] = eval()
	@inline final override def toOpt :Opt[T] = eval()
	@inline final override def constOpt :Opt[T] = None

	def map[@specialized(Fun1) O](f :T => O) :EvalOpt[O] = () => eval().map(f)
	def flatMap[@specialized(SpecializedVars) O](f :T => EvalOpt[O]) :EvalOpt[O] = () => eval().flatMap(f(_).maybe)

	override def mkString :String = mkString("EvalOpt")
	override def toString :String = //should it evaluate and print it?
		try maybe.mapOrElse(_.toString, undefined.toString) catch {
			case _ :NoSuchElementException => undefined.toString
		}
}


@SerialVersionUID(Ver)
object EvalOpt {
	@inline def apply[@specialized(SpecializedVars) T](eval :EvalOpt[T]) :EvalOpt[T] = eval

	def eval[@specialized(SpecializedVars) T](generator : => Maybe[T]) :EvalOpt[T] = () => generator
}
