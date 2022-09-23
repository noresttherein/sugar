package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A serializable `Function0[T]` adapted to `Ref` interface.
  * It is a ''single abstract method'' type,
  * leaving only [[net.noresttherein.sugar.vars.Ref.value value]] to implement by subclasses.
  * @define Ref `Eval`
  */
trait Eval[@specialized(SpecializedVars) +T] extends Ref[T] with (() => T) with Serializable {

  override def isFinal    = false
  override def isEmpty    = false
  override def isConst    = false
  override def isDefined  = true
  override def isDefinite = true
  override def isFinalizable = false

  override def value :T = apply()
  override def get   :T = apply()
  override def const :T = throw new UnsupportedOperationException("Eval.const")

  override def option :Option[T] =
    try { Some(value) } catch {
      case _ :NoSuchElementException => None
    }
  @inline final override def toOption :Option[T] = option
  @inline final override def constOption :Option[T] = None

  override def opt :Opt[T] =
    try { Got(value) } catch {
      case _ :NoSuchElementException => Lack
    }
  @inline final override def toOpt :Opt[T] = opt
  @inline final override def constOpt :Opt[T] = Lack

  override def unsure :Unsure[T] =
    try { Sure(value) } catch {
      case _ :NoSuchElementException => Missing
    }
  @inline final override def toUnsure :Unsure[T] = unsure
  @inline final override def constUnsure :Unsure[T] = Missing

  override def toString :String =
    try { String.valueOf(value) } catch {
      case _ :NoSuchElementException => undefined.toString
    }
}




object Eval {
  @inline def apply[@specialized(SpecializedVars) T](eval :Eval[T]) :Eval[T] = eval

  def eval[@specialized(SpecializedVars) T](eval: => T) :Eval[T] = () => eval
}






/** A serializable `Function0[T]` adapted to `Ref` interface.
  * It is a ''single abstract method'' type,
  * leaving only [[net.noresttherein.sugar.vars.Ref.opt opt]] to implement by subclasses.
  * @define Ref `EvalOpt`
  */
trait EvalOpt[@specialized(SpecializedVars) +T] extends Ref[T] with (() => T) with Serializable {
  override def isFinal    = false
  override def isEmpty    = false
  override def isConst    = false
  override def isDefined  = true
  override def isDefinite = true
  override def isFinalizable = false

  override def value :T = opt match {
    case Got(v) => v
    case _ => throw new NoSuchElementException("EvalOpt.value")
  }
  @inline final override def get :T = value
  @inline final override def apply() :T = value
  override def const :T = throw new UnsupportedOperationException("EvalOpt.const")

  @inline final override def toOpt :Opt[T] = opt
  @inline final override def constOpt :Opt[T] = Lack
}




object EvalOpt {
  @inline def apply[@specialized(SpecializedVars) T](eval :EvalOpt[T]) :EvalOpt[T] = eval

  def eval[@specialized(SpecializedVars) T](eval: => Opt[T]) :EvalOpt[T] =
    new EvalOpt[T] {
      override def opt = eval
    }
}
