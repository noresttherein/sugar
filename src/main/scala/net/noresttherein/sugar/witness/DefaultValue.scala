package net.noresttherein.sugar.witness

import java.util.function.Supplier

import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.{Const, Eval, Lazy, Maybe, Opt, Ref, SyncLazyRef}




/** A type class providing the default value for a given type. Implicit values exist for all standard value types,
  * as well as some monadic types. There are however no constraints on what the value should be,
  * and applications are free to both define values for custom types, and override the provided values.
  * This makes it different from [[net.noresttherein.sugar.witness.NullValue NullValue]]`[T]`, which describes
  * specifically the default value for the type, as defined by the language (`_` in statement `var x :X = _`),
  * and includes `null` value for reference types, which this type class does not for increased safety.
  * @author Marcin Mościcki
  */
//This class is specialized, because Ref subclass factory methods accepting only it should be specialized.
// It would be bad, because generic calls to get would box the value each time. However, the actual instances
// are always created as non-specialized, so they'll store the boxed value and unbox it on specialized calls.
@SerialVersionUID(Ver)
sealed trait DefaultValue[@specialized +T] extends Ref[T] with Serializable {
	def map[O](f :T => O) :DefaultValue[O]
	def flatMap[O](f :T => DefaultValue[O]) :DefaultValue[O]

	def supplier  :Supplier[_ <: T]
	def toFunction0 :() => T
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[DefaultValue[_]]
	override def mkString :String = mkString("DefaultValue")
}


@SerialVersionUID(Ver)
object DefaultValue {
	@inline def default[T](implicit value :DefaultValue[T]) :T = value.get

	//todo: in Scala3, drop the empty param list
	@inline def apply[T]()(implicit value :DefaultValue[T]) :value.type = value

	@inline def apply[T](value :T) :DefaultValue[T] = new ConstDefault(value)
	@inline def delay[T](value: => T) :DefaultValue[T] = new LazyDefault[T](() => value)
	@inline def eval[T](value: => T) :DefaultValue[T] = new EvalDefault[T](() => value)

	implicit val DefaultByte    :NullValue[Byte]    = new NullValue(0)
	implicit val DefaultShort   :NullValue[Short]   = new NullValue(0)
	implicit val DefaultChar    :NullValue[Char]    = new NullValue(0)
	implicit val DefaultInt     :NullValue[Int]     = new NullValue(0)
	implicit val DefaultLong    :NullValue[Long]    = new NullValue(0)
	implicit val DefaultFloat   :NullValue[Float]   = new NullValue(0)
	implicit val DefaultDouble  :NullValue[Double]  = new NullValue(0)
	implicit val DefaultBoolean :NullValue[Boolean] = new NullValue(false)
	implicit val DefaultUnit    :NullValue[Unit]    = new NullValue(())

	implicit def defaultOption[T] :DefaultValue[Option[T]] = DefaultNone.asInstanceOf[DefaultValue[Option[T]]]
	implicit def defaultMaybe[T]  :DefaultValue[Maybe[T]]  = DefaultNo.asInstanceOf[DefaultValue[Maybe[T]]]
	implicit def defaultOpt[T]    :DefaultValue[Opt[T]]    = DefaultNone.asInstanceOf[DefaultValue[Opt[T]]]

	private[this] final val DefaultNone = new ConstDefault(None)
	private[this] final val DefaultNo   = new ConstDefault(No)


	@SerialVersionUID(Ver)
	private class ConstDefault[T](override val get :T) extends Const[T] with Lazy[T] with DefaultValue[T] {
		override val supplier    :Supplier[_ <: T] = () => get
		override val toFunction0 :() => T          = () => get

		override def map[O](f :T => O) :ConstDefault[O] = new ConstDefault(f(get))
		override def flatMap[O](f :T => DefaultValue[O]) :DefaultValue[O] = f(get)

		override def toString :String = "DefaultValue(" + get + ")"
	}
	@SerialVersionUID(Ver)
	private class EvalDefault[T](override val toFunction0 :() => T) extends Eval[T] with DefaultValue[T] {
		override val supplier :Supplier[_ <: T] = () => toFunction0()
		override def apply() :T = get
		override def get =
			try toFunction0() catch {
				case e :Exception =>
					throw new NoSuchElementException("Could not calculate the default: " + e.getMessage, e)
			}

		override def map[O](f :T => O) :DefaultValue[O] = new EvalDefault(() => f(toFunction0()))
		override def flatMap[O](f :T => DefaultValue[O]) :DefaultValue[O] = new EvalDefault(() => f(toFunction0()).get)

		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this
		override def toString :String = "DefaultValue(_)"
	}
	@SerialVersionUID(Ver)
	private class LazyDefault[T](init :() => T) extends SyncLazyRef[T](init) with DefaultValue[T] {
		override val toFunction0 :() => T = () => get
		override val supplier :Supplier[_ <: T] = () => get

		override def map[O](f :T => O) :DefaultValue[O] with Lazy[O] = maybe match {
			case Yes(v) => new ConstDefault(f(v))
			case _      => new LazyDefault(() => f(get))
		}
		override def flatMap[O](f :T => DefaultValue[O]) :DefaultValue[O] = maybe match {
			case Yes(v) => f(v)
			case _      => new LazyDefault(() => f(get).get)
		}
		override def toString :String = "DefaultValue(" + get + ")"
		private def writeReplace :AnyRef = new ConstDefault(get)
	}
}




/** A type class providing the closest equivalent of 'zero' or 'null' value for a type.
  * It is the default Scala/Java value of this type, the one to which `Array[T]` is initialized.
  * The difference from `DefaultValue` is an addition of `NullValue[Null] == null` for true reference types.
  */ //Not specialized anymore, to avoid boxing in get on generic calls.
@SerialVersionUID(Ver)
final class NullValue[+T] private[witness] (override val get :T) extends DefaultValue[T] {
	override val supplier    :Supplier[_ <: T] = () => get
	override val toFunction0 :() => T          = () => get

	override def apply()  :T = get
	override def value    :T = get
	override def const    :T = get
	override def opt      :Opt[T] = One(get)
	override def toOpt    :Opt[T] = One(get)
	override def constOpt :Opt[T] = One(get)

	override def isFinal       :Boolean = true
	override def isFinalizable :Boolean = true
	override def isConst       :Boolean = true
	override def isDefined     :Boolean = true
	override def isDefinite    :Boolean = true

	override def map[O](f :T => O) :DefaultValue[O] = DefaultValue(f(get))
	override def flatMap[O](f :T => DefaultValue[O]) :DefaultValue[O] = f(get)

	override def mkString :String = mkString("NullValue")
}


@SerialVersionUID(Ver)
object NullValue {
	@inline def apply[T](implicit value :NullValue[T]) :NullValue[T] = value

	implicit val NullRef :NullValue[Null]    = new NullValue[Null](null)
}
