package net.turambar

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

/**
  * @author Marcin Mościcki
  */
package object slang {
//	object ValidatedBlockExpression {
//		@inline final def apply[T](block: =>T) :T = block
//	}

	private[slang] final def raise[E<:Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure", ex)
		}).get

	private[slang] def raise[E<:Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.newInstance().asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure", ex)
		}).get


	/** A box/reference for a mutable value. Allows for in/out paramters to functions. */
	final class Var[@specialized(Var.SpecializedTypes) T](private[this] var value :T) extends Serializable {
		@inline final def get :T = value
		@inline final def :=(newValue :T) :Unit = value = newValue

		@inline final def +=(other :T)(implicit num :Numeric[T]) :Unit =
			value = num.plus(value, other)

		@inline final def -=(other :T)(implicit num :Numeric[T]) :Unit =
			value = num.minus(value, other)

		@inline final def ++(implicit num :Integral[T]) :Unit =
			value = num.plus(value, num.one)

		@inline final def --(implicit num :Integral[T]) :Unit =
			value = num.minus(value, num.one)

		@inline final def *=(other :T)(implicit num :Numeric[T]) :Unit =
			value = num.times(value, other)

		@inline final def /=(other :T)(implicit num :Fractional[T]) :Unit =
			value = num.div(value, other)

		@inline final def /=(other :T)(implicit num :Integral[T]) :Unit =
			value = num.quot(value, other)

		@inline final def %=(other :T)(implicit num :Integral[T]) :Unit =
			value = num.rem(value, other)

		@inline final def &&=(other :Boolean)(implicit bool :T=:=Boolean) :Unit =
			value = (bool(value) && other).asInstanceOf[T]

		@inline final def ||=(other :Boolean)(implicit bool :T=:=Boolean) :Unit =
			value = (bool(value) || other).asInstanceOf[T]

		override def equals(that :Any) = that match {
			case v :Var[_] => (v eq this) || v.get == value
			case _ => false
		}

		override def hashCode = value.hashCode

		override def toString = value.toString
	}

	object Var {
		final val SpecializedTypes = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)

		@inline final implicit def unboxVar[@specialized(SpecializedTypes) T](variable :Var[T]) :T = variable.get

		@inline def apply[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)

		object implicits {
			@inline implicit def boxVar[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)
		}
	}
//	def required(condition :Boolean, msg : =>Any)
/*
	@inline final def validate[E<:Throwable :ClassTag](condition :Boolean, msg: =>String) :ValidatedBlockExpression.type =
		if (condition) {
			ValidatedBlockExpression
		} else raise[E](msg)

	@inline final def validate[E<:Throwable :ClassTag](condition :Boolean) :ValidatedBlockExpression.type =
		if (condition)
			ValidatedBlockExpression
		else raise[E]
*/

}
