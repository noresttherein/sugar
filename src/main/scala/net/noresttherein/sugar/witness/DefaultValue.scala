package net.noresttherein.sugar.witness

import java.util.function.Supplier

import scala.reflect.ClassTag

import net.noresttherein.sugar.vars.Opt.Lack
import net.noresttherein.sugar.vars.Opt




/** A type class providing the default value for a given type. Implicit values exist for all standard value types,
  * as well as some monadic types. There are however no constraints on what the value should be,
  * and applications are free to both define values for custom types, and override the provided values.
  * This makes it different from [[net.noresttherein.sugar.witness.NullValue NullValue]]`[T]`, which describes
  * specifically the default value for the type, as defined by the language (`_` in statement `var x :X = _`),
  * and includes `null` value for reference types, which this type class does not for increased safety.
  * @author Marcin Mościcki
  */
//Not specialized anymore, to avoid boxing in get on generic calls. This is not optimal for EvalValue,
// but creating an implementation caching boxed values requires subclasses for every type.
@SerialVersionUID(Ver)
abstract class DefaultValue[+T] extends Serializable {
	def get :T
	def supplier  :Supplier[_ <: T]
	def toFunction0 :() => T
}


@SerialVersionUID(Ver)
object DefaultValue {
	@inline def default[T](implicit value :DefaultValue[T]) :T = value.get

	//todo: in Scala3, drop the empty param list
	@inline def apply[T]()(implicit value :DefaultValue[T]) :value.type = value

	@inline def apply[T](value :T) :DefaultValue[T] = new ConstValue(value)
	@inline def delay[T](value: => T) :DefaultValue[T] = new LazyValue[T](() => value)
	@inline def eval[T](value: => T) :DefaultValue[T] = new EvalValue[T](() => value)

	implicit val DefaultByte    :NullValue[Byte]    = new NullValue(0)
	implicit val DefaultShort   :NullValue[Short]   = new NullValue(0)
	implicit val DefaultChar    :NullValue[Char]    = new NullValue(0)
	implicit val DefaultInt     :NullValue[Int]     = new NullValue(0)
	implicit val DefaultLong    :NullValue[Long]    = new NullValue(0)
	implicit val DefaultFloat   :NullValue[Float]   = new NullValue(0)
	implicit val DefaultDouble  :NullValue[Double]  = new NullValue(0)
	implicit val DefaultBoolean :NullValue[Boolean] = new NullValue(false)
	implicit val DefaultUnit    :NullValue[Unit]    = new NullValue(())
//	implicit val DefaultByte    :DefaultValue[Byte]    = NullValue.ZeroByte
//	implicit val DefaultShort   :DefaultValue[Short]   = NullValue.ZeroShort
//	implicit val DefaultChar    :DefaultValue[Char]    = NullValue.ZeroChar
//	implicit val DefaultInt     :DefaultValue[Int]     = NullValue.ZeroInt
//	implicit val DefaultLong    :DefaultValue[Long]    = NullValue.ZeroLong
//	implicit val DefaultFloat   :DefaultValue[Float]   = NullValue.ZeroFloat
//	implicit val DefaultDouble  :DefaultValue[Double]  = NullValue.ZeroDouble
//	implicit val DefaultBoolean :DefaultValue[Boolean] = NullValue.FalseBoolean
//	implicit val DefaultUnit    :DefaultValue[Unit]    = NullValue.UnitValue

	implicit def defaultOption[T] :DefaultValue[Option[T]] = DefaultNone.asInstanceOf[DefaultValue[Option[T]]]
	implicit def defaultOpt[T]    :DefaultValue[Opt[T]]    = DefaultLack.asInstanceOf[DefaultValue[Opt[T]]]

	private[this] final val DefaultNone = new ConstValue(None)
	private[this] final val DefaultLack = new ConstValue(Lack)


	private class ConstValue[T](override val get :T) extends DefaultValue[T] {
		override val supplier    :Supplier[_ <: T] = () => get
		override val toFunction0 :() => T          = () => get
		override def toString :String = "DefaultValue(" + get + ")"
	}
	private class EvalValue[T](override val toFunction0 :() => T) extends DefaultValue[T] {
		override def get = toFunction0()
		override val supplier :Supplier[_ <: T] = () => toFunction0()
		override def toString :String = "DefaultValue(_)"
	}
	private class LazyValue[T](init :() => T) extends DefaultValue[T] {
		override lazy val get :T = init()
		override val toFunction0 :() => T = () => get
		override val supplier :Supplier[_ <: T] = () => get
	}
}




/** A type class providing the closest equivalent of 'zero' or 'null' value for a type.
  * It is the default Scala/Java value of this type, the one to which `Array[T]` is initialized.
  * The difference from `DefaultValue` is an addition of `NullValue[Null] == null` for true reference types.
  */ //Not specialized anymore, to avoid boxing in get on generic calls.
@SerialVersionUID(Ver)
final class NullValue[T] private[witness] (override val get :T) extends DefaultValue[T] {
	override val supplier    :Supplier[_ <: T] = () => get
	override val toFunction0 :() => T          = () => get
}


@SerialVersionUID(Ver)
object NullValue {
	@inline def apply[T](implicit value :NullValue[T]) :NullValue[T] = value

	implicit val NullRef      :NullValue[Null]    = new NullValue[Null](null)
//	implicit val ZeroByte     :NullValue[Byte]    = new NullValue[Byte](0)
//	implicit val ZeroShort    :NullValue[Short]   = new NullValue[Short](0)
//	implicit val ZeroChar     :NullValue[Char]    = new NullValue[Char](0)
//	implicit val ZeroInt      :NullValue[Int]     = new NullValue[Int](0)
//	implicit val ZeroLong     :NullValue[Long]    = new NullValue[Long](0L)
//	implicit val ZeroFloat    :NullValue[Float]   = new NullValue[Float](0.0f)
//	implicit val ZeroDouble   :NullValue[Double]  = new NullValue[Double](0.0d)
//	implicit val FalseBoolean :NullValue[Boolean] = new NullValue[Boolean](false)
//	implicit val UnitValue    :NullValue[Unit]    = new NullValue[Unit](())
}


