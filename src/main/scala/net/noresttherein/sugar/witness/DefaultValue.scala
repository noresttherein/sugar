package net.noresttherein.sugar.witness

import java.util.function.Supplier

import scala.reflect.ClassTag

import net.noresttherein.sugar.vars.Opt.Lack
import net.noresttherein.sugar.vars.Opt


/** A type class providing the default value for standard types. It is the value to which `Array[T]` is initialized,
  * that is some sort of zero for numeric values, `false`, and `null` for reference values.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
final class DefaultValue[@specialized +T](val get :T) extends Serializable {
	private[sugar] val supplier :Supplier[_ <: T] = () => get
}


@SerialVersionUID(Ver)
object DefaultValue {
	@inline def default[T](implicit value :DefaultValue[T]) :T = value.get

	//todo: in Scala3, drop the empty param list
	@inline def apply[T]()(implicit value :DefaultValue[T]) :value.type = value

	@inline def apply[@specialized T](value :T) = new DefaultValue(value)


	implicit val DefaultNull    :DefaultValue[Null]    = new DefaultValue[Null](null)
	implicit val DefaultByte    :DefaultValue[Byte]    = new DefaultValue[Byte](0)
	implicit val DefaultShort   :DefaultValue[Short]   = new DefaultValue[Short](0)
	implicit val DefaultChar    :DefaultValue[Char]    = new DefaultValue[Char](0)
	implicit val DefaultInt     :DefaultValue[Int]     = new DefaultValue[Int](0)
	implicit val DefaultLong    :DefaultValue[Long]    = new DefaultValue[Long](0L)
	implicit val DefaultFloat   :DefaultValue[Float]   = new DefaultValue[Float](0.0f)
	implicit val DefaultDouble  :DefaultValue[Double]  = new DefaultValue[Double](0.0d)
	implicit val DefaultBoolean :DefaultValue[Boolean] = new DefaultValue[Boolean](false)
	implicit val DefaultUnit    :DefaultValue[Unit]    = new DefaultValue[Unit](())

	implicit def defaultOption[T] :DefaultValue[Option[T]] = DefaultNone.asInstanceOf[DefaultValue[Option[T]]]
	implicit def defaultOpt[T]    :DefaultValue[Opt[T]]    = DefaultLack.asInstanceOf[DefaultValue[Opt[T]]]

	private[this] final val DefaultNone = new DefaultValue(None)
	private[this] final val DefaultLack = new DefaultValue(Lack)
}
