package net.noresttherein.sugar.collections


import scala.annotation.switch
import scala.collection.{Stepper, StepperShape}
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, Shape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JByte, JChar, JDouble, JFloat, JInt, JIterator, JLong, JShort}
import net.noresttherein.sugar.collections.JavaIteratorShape.IteratorShape
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** An implicit witness providing the most specialized Java Iterator subtype `I` for a given element type `A`.
  * For value types, it specifies one of
  * [[java.util.PrimitiveIterator PrimitiveIterator]]`.{`[[java.util.PrimitiveIterator.OfInt OfInt]]`,`[[java.util.PrimitiveIterator.OfLong OfLong]]`.`[[java.util.PrimitiveIterator.OfDouble OfDouble]]`}`.
  * Very similar to [[scala.collection.StepperShape StepperShape]], it allows to define methods returning
  * a `java.util.Iterator`.
  * @tparam A the Scala type of the iterator elements.
  * @tparam I a subtype of `JavaIterator[_]`, compatible with element type `A`.
  * @author Marcin Mościcki
  */
sealed trait JavaIteratorShape[-A, I] extends Equals with Serializable {
	import JteratorShape._

	def shape :StepperShape.Shape

	def stepperShape :StepperShape[A, _ <: Stepper[_]] =
		(shape match {
			case IntShape    => StepperShape.intStepperShape
			case LongShape   => StepperShape.longStepperShape
			case DoubleShape => StepperShape.doubleStepperShape
			case CharShape   => StepperShape.charStepperShape
			case ByteShape   => StepperShape.byteStepperShape
			case FloatShape  => StepperShape.floatStepperShape
			case ShortShape  => StepperShape.shortStepperShape
			case _           => StepperShape.ReferenceShape
		}).asInstanceOf[StepperShape[A, _ <: Stepper[_]]]

	def jteratorShape :JteratorShape[A, _ <: Jterator[_]] =
		(shape match {
			case ReferenceShape => anyJteratorShape
			case IntShape       => intJteratorShape
			case LongShape      => longJteratorShape
			case DoubleShape    => doubleJteratorShape
			case CharShape      => charJteratorShape
			case ByteShape      => byteJteratorShape
			case FloatShape     => floatJteratorShape
			case ShortShape     => shortJteratorShape
			case _              => anyJteratorShape
		}).asInstanceOf[JteratorShape[A, _ <: Jterator[_]]]

	def empty :I

	override def equals(that :Any) :Boolean = that match {
		case other :JavaIteratorShape[_, _] if other canEqual this => (this eq other) || shape == other.shape
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JavaIteratorShape[_, _]]
	override def hashCode :Int = shape.hashCode

	override def toString :String = this match {
		case JavaIteratorShape.intJavaIteratorShape        => "JavaIteratorShape[Int, JIntIterator]"
		case JavaIteratorShape.longJavaIteratorShape       => "JavaIteratorShape[Long, JLongIterator]"
		case JavaIteratorShape.doubleJavaIteratorShape     => "JavaIteratorShape[Double, JDoubleIterator]"
		case JavaIteratorShape.charJavaIteratorShape       => "JavaIteratorShape[Char, JIntIterator]"
		case JavaIteratorShape.byteJavaIteratorShape       => "JavaIteratorShape[Byte, JIntIterator]"
		case JavaIteratorShape.floatJavaIteratorShape      => "JavaIteratorShape[Double, JDoubleIterator]"
		case JavaIteratorShape.shortJavaIteratorShape      => "JavaIteratorShape[Short, JIntIterator]"
		case JavaIteratorShape.javaIntJavaIteratorShape    => "JavaIteratorShape[JInt, JIntIterator]"
		case JavaIteratorShape.javaLongJavaIteratorShape   => "JavaIteratorShape[JLong, JLongIterator]"
		case JavaIteratorShape.javaDoubleJavaIteratorShape => "JavaIteratorShape[JDouble, JDoubleIterator]"
		case JavaIteratorShape.javaCharJavaIteratorShape   => "JavaIteratorShape[JChar, JIntIterator]"
		case JavaIteratorShape.javaByteJavaIteratorShape   => "JavaIteratorShape[JByte, JIntIterator]"
		case JavaIteratorShape.javaFloatJavaIteratorShape  => "JavaIteratorShape[JDouble, JDoubleIterator]"
		case JavaIteratorShape.javaShortJavaIteratorShape  => "JavaIteratorShape[JShort, JIntIterator]"
		case _                                             => "JavaIteratorShape[_, JIterator[_]]"
	}
}


private[collections] sealed abstract class Rank3JavaIteratorShapes {
	implicit def anyJavaIteratorShape[A] :JavaIteratorShape[A, JavaIterator[A]] =
		refJavaIteratorShape.asInstanceOf[JavaIteratorShape[A, JavaIterator[A]]]

	private[this] val refJavaIteratorShape = new IteratorShape[Any, JavaIterator[Any]](ReferenceShape, JavaIterator.empty)
}

private[collections] sealed abstract class Rank2JavaIteratorShapes extends Rank3JavaIteratorShapes {
	implicit def stepperJavaIteratorShape[A, S <: Stepper[_]](implicit shape :StepperShape[A, S])
			:JavaIteratorShape[A, JavaIterator[A]] =
		(shape.shape match {
			case IntShape    => JavaIteratorShape.intJavaIteratorShape
			case LongShape   => JavaIteratorShape.longJavaIteratorShape
			case DoubleShape => JavaIteratorShape.doubleJavaIteratorShape
			case CharShape   => JavaIteratorShape.charJavaIteratorShape
			case ByteShape   => JavaIteratorShape.byteJavaIteratorShape
			case FloatShape  => JavaIteratorShape.floatJavaIteratorShape
			case ShortShape  => JavaIteratorShape.floatJavaIteratorShape
			case _           => anyJavaIteratorShape
		}).asInstanceOf[JavaIteratorShape[A, JIterator[A]]]
}

private[collections] sealed abstract class Rank1JavaIteratorShapes extends Rank2JavaIteratorShapes {
	implicit val byteJavaIteratorShape   :JavaIteratorShape[Byte, JavaIntIterator] =
		new IteratorShape(ByteShape, JavaIterator.ofInt())
	implicit val shortJavaIteratorShape  :JavaIteratorShape[Short, JavaIntIterator] =
		new IteratorShape(ShortShape, JavaIterator.ofInt())
	implicit val charJavaIteratorShape   :JavaIteratorShape[Char, JavaIntIterator] =
		new IteratorShape(CharShape, JavaIterator.ofInt())
	implicit val floatJavaIteratorShape  :JavaIteratorShape[Float, JavaDoubleIterator] =
		new IteratorShape(FloatShape, JavaIterator.ofDouble())

	implicit val javaByteJavaIteratorShape :JavaIteratorShape[JByte, JavaIntIterator] =
		byteJavaIteratorShape.asInstanceOf[JavaIteratorShape[JByte, JavaIntIterator]]
	implicit val javaShortJavaIteratorShape :JavaIteratorShape[JShort, JavaIntIterator] =
		shortJavaIteratorShape.asInstanceOf[JavaIteratorShape[JShort, JavaIntIterator]]
	implicit val javaCharJavaIteratorShape :JavaIteratorShape[JChar, JavaIntIterator] =
		charJavaIteratorShape.asInstanceOf[JavaIteratorShape[JChar, JavaIntIterator]]
	implicit val javaIntJavaIteratorShape :JavaIteratorShape[JInt, JavaIntIterator] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShape[JInt, JavaIntIterator]]
	implicit val javaLongJavaIteratorShape :JavaIteratorShape[JLong, JavaLongIterator] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShape[JLong, JavaLongIterator]]
	implicit val javaFloatJavaIteratorShape :JavaIteratorShape[JFloat, JavaDoubleIterator] =
		floatJavaIteratorShape.asInstanceOf[JavaIteratorShape[JFloat, JavaDoubleIterator]]
	implicit val javaDoubleJavaIteratorShape :JavaIteratorShape[JDouble, JavaDoubleIterator] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShape[JDouble, JavaDoubleIterator]]
}

@SerialVersionUID(Ver)
object JavaIteratorShape extends Rank1JavaIteratorShapes {
	@SerialVersionUID(Ver)
	private[collections] class IteratorShape[A, I <: JIterator[_]](override val shape :Shape, override val empty :I)
		extends JavaIteratorShape[A, I]

	implicit val intJavaIteratorShape    :JavaIteratorShape[Int, JavaIntIterator] =
		new IteratorShape(IntShape, JavaIterator.ofInt())
	implicit val longJavaIteratorShape   :JavaIteratorShape[Long, JavaLongIterator] =
		new IteratorShape(LongShape, JavaIterator.ofLong())
	implicit val doubleJavaIteratorShape :JavaIteratorShape[Double, JavaDoubleIterator] =
		new IteratorShape(DoubleShape, JavaIterator.ofDouble())
}


/** A proof that [[net.noresttherein.sugar.collections.JavaIterator JavaIterator]] `I` has elements of type `E`.
  * It allows generic use of manually specialized types for all value types, such as
  * [[net.noresttherein.sugar.collections.IntJterator IntJterator]],
  * [[net.noresttherein.sugar.collections.CharJterator CharJterator]], etc.
  * @param shape A similar evidence of this category for standard `Stepper`, essentially enumerating value types.
  *              It is empty only for `Boolean`, which allows to correctly identify the latter, too.
  * @tparam E the Scala type of the jterator elements.
  * @tparam I a subtype of `Jterator[_]`, compatible with element type `A`.
  */
@SerialVersionUID(Ver)
final class JteratorShape[-E, I] private (val shape :Opt[StepperShape.Shape]) extends Equals with Serializable {
	import JavaIteratorShape._

	def javaIteratorShape :JavaIteratorShape[E, _ <: JavaIterator[_]] = (
		if (!shape.isDefined)
			anyJavaIteratorShape
		else (shape.get : @switch) match {
			case ReferenceShape => anyJavaIteratorShape
			case IntShape       => intJavaIteratorShape
			case LongShape      => longJavaIteratorShape
			case DoubleShape    => doubleJavaIteratorShape
			case CharShape      => charJavaIteratorShape
			case ByteShape      => byteJavaIteratorShape
			case FloatShape     => floatJavaIteratorShape
			case ShortShape     => shortJavaIteratorShape
			case _              => anyJavaIteratorShape
		}
	).asInstanceOf[JavaIteratorShape[E, _ <: JavaIterator[_]]]

	def stepperShape :StepperShape[E, _ <: Stepper[_]] = javaIteratorShape.stepperShape

	override def toString :String = shape match {
		case Got(ReferenceShape) => "JteratorShape[_<:AnyRef, RefJterator[_]]"
		case Got(IntShape)       => "JteratorShape[Int, IntJterator]"
		case Got(LongShape)      => "JteratorShape[Long, LongJterator]"
		case Got(DoubleShape)    => "JteratorShape[Double, DoubleJterator]"
		case Got(FloatShape)     => "JteratorShape[Float, FloatJterator]"
		case Got(ShortShape)     => "JteratorShape[Short, ShortJterator]"
		case Got(CharShape)      => "JteratorShape[Char, CharJterator]"
		case Got(ByteShape)      => "JteratorShape[Byte, ByteJterator]"
		case _                   => "Jterator[Boolean, BooleanJterator]"
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JteratorShape[_, _]]
	override def equals(that :Any) :Boolean = that match {
		case shape :JteratorShape[_, _] => this.shape == shape.shape
		case _ => false
	}
	override def hashCode :Int = shape.hashCode
}


@SerialVersionUID(Ver)
object JteratorShape extends JteratorShapeForAny {
	implicit val intJteratorShape     :JteratorShape[Int, IntJterator]   = new JteratorShape(Got(IntShape))
	implicit val longJteratorShape    :JteratorShape[Long, LongJterator] = new JteratorShape(Got(LongShape))
	implicit val doubleJteratorShape  :JteratorShape[Double, DoubleJterator] = new JteratorShape(Got(DoubleShape))
	implicit val floatJteratorShape   :JteratorShape[Float, FloatJterator] = new JteratorShape(Got(FloatShape))
	implicit val shortJteratorShape   :JteratorShape[Short, ShortJterator] = new JteratorShape(Got(ShortShape))
	implicit val charJteratorShape    :JteratorShape[Char, CharJterator] = new JteratorShape(Got(CharShape))
	implicit val byteJteratorShape    :JteratorShape[Byte, ByteJterator] = new JteratorShape(Got(ByteShape))
	implicit val booleanJteratorShape :JteratorShape[Boolean, BooleanJterator] = new JteratorShape(Lack)
	implicit def refJteratorShape[T <: AnyRef] :JteratorShape[T, RefJterator[T]] =
		anyRefJteratorShape.asInstanceOf[JteratorShape[T, RefJterator[T]]]

	private val anyRefJteratorShape = new JteratorShape[AnyRef, RefJterator[AnyRef]](Got(ReferenceShape))
}

sealed abstract class JteratorShapeForAny {
	@inline implicit def anyJteratorShape[T] :JteratorShape[T, JavaIterator[T]] =
		JteratorShape.refJteratorShape.asInstanceOf[JteratorShape[T, JavaIterator[T]]]
}

