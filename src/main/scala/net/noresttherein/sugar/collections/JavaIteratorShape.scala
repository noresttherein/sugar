package net.noresttherein.sugar.collections


import scala.annotation.switch
import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, Stepper, StepperShape}
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, Shape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JByte, JChar, JDouble, JFloat, JInt, JIterator, JLong, JShort}
import net.noresttherein.sugar.collections
import net.noresttherein.sugar.collections.JavaIteratorShape.IteratorShape
import net.noresttherein.sugar.extensions.castingMethods
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
//	type Jterator <: collections.Jterator[_]
	type Stepper  <: collection.Stepper[_]
	import JteratorShape._

	def shape :StepperShape.Shape

	def stepperShape :StepperShape[A, Stepper] =
		(shape match {
			case IntShape    => StepperShape.intStepperShape
			case LongShape   => StepperShape.longStepperShape
			case DoubleShape => StepperShape.doubleStepperShape
			case CharShape   => StepperShape.charStepperShape
			case ByteShape   => StepperShape.byteStepperShape
			case FloatShape  => StepperShape.floatStepperShape
			case ShortShape  => StepperShape.shortStepperShape
			case _           => StepperShape.ReferenceShape
		}).asInstanceOf[StepperShape[A, Stepper]]

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




@SerialVersionUID(Ver)
object JavaIteratorShape extends Rank1JavaIteratorShapes {
	@SerialVersionUID(Ver)
	private[collections] class IteratorShape[A, I <: JIterator[_]](override val shape :Shape, override val empty :I)
		extends JavaIteratorShape[A, I]

	implicit val intJavaIteratorShape    :JavaIteratorShapeWithStepper[Int, JavaIntIterator, IntStepper] =
		new IteratorShape(IntShape, JavaIterator.ofInt())
			.asInstanceOf[JavaIteratorShapeWithStepper[Int, JavaIntIterator, IntStepper]]
	implicit val longJavaIteratorShape   :JavaIteratorShapeWithStepper[Long, JavaLongIterator, LongStepper] =
		new IteratorShape(LongShape, JavaIterator.ofLong())
			.asInstanceOf[JavaIteratorShapeWithStepper[Long, JavaLongIterator, LongStepper]]
	implicit val doubleJavaIteratorShape :JavaIteratorShapeWithStepper[Double, JavaDoubleIterator, DoubleStepper] =
		new IteratorShape(DoubleShape, JavaIterator.ofDouble())
			.asInstanceOf[JavaIteratorShapeWithStepper[Double, JavaDoubleIterator, DoubleStepper]]
}

private[collections] sealed abstract class Rank1JavaIteratorShapes extends Rank2JavaIteratorShapes {
	implicit val byteJavaIteratorShape   :JavaIteratorShapeWithStepper[Byte, JavaIntIterator, IntStepper] =
		new IteratorShape(ByteShape, JavaIterator.ofInt())
			.asInstanceOf[JavaIteratorShapeWithStepper[Byte, JavaIntIterator, IntStepper]]
	implicit val shortJavaIteratorShape  :JavaIteratorShapeWithStepper[Short, JavaIntIterator, IntStepper] =
		new IteratorShape(ShortShape, JavaIterator.ofInt())
			.asInstanceOf[JavaIteratorShapeWithStepper[Short, JavaIntIterator, IntStepper]]
	implicit val charJavaIteratorShape   :JavaIteratorShapeWithStepper[Char, JavaIntIterator, IntStepper] =
		new IteratorShape(CharShape, JavaIterator.ofInt())
			.asInstanceOf[JavaIteratorShapeWithStepper[Char, JavaIntIterator, IntStepper]]
	implicit val floatJavaIteratorShape  :JavaIteratorShapeWithStepper[Float, JavaDoubleIterator, DoubleStepper] =
		new IteratorShape(FloatShape, JavaIterator.ofDouble())
			.asInstanceOf[JavaIteratorShapeWithStepper[Float, JavaDoubleIterator, DoubleStepper]]

	implicit val javaByteJavaIteratorShape :JavaIteratorShapeWithStepper[JByte, JavaIntIterator, IntStepper] =
		byteJavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JByte, JavaIntIterator, IntStepper]]
	implicit val javaShortJavaIteratorShape :JavaIteratorShapeWithStepper [JShort, JavaIntIterator, IntStepper] =
		shortJavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JShort, JavaIntIterator, IntStepper]]
	implicit val javaCharJavaIteratorShape :JavaIteratorShapeWithStepper[JChar, JavaIntIterator, IntStepper] =
		charJavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JChar, JavaIntIterator, IntStepper]]
	implicit val javaIntJavaIteratorShape :JavaIteratorShapeWithStepper[JInt, JavaIntIterator, IntStepper] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JInt, JavaIntIterator, IntStepper]]
	implicit val javaLongJavaIteratorShape :JavaIteratorShapeWithStepper[JLong, JavaLongIterator, LongStepper] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JLong, JavaLongIterator, LongStepper]]
	implicit val javaFloatJavaIteratorShape :JavaIteratorShapeWithStepper[JFloat, JavaDoubleIterator, DoubleStepper] =
		floatJavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JFloat, JavaDoubleIterator, DoubleStepper]]
	implicit val javaDoubleJavaIteratorShape :JavaIteratorShapeWithStepper[JDouble, JavaDoubleIterator, DoubleStepper] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[JDouble, JavaDoubleIterator, DoubleStepper]]
}

private[collections] sealed abstract class Rank2JavaIteratorShapes extends Rank3JavaIteratorShapes {
	implicit def stepperJavaIteratorShape[A, S <: Stepper[_]](implicit shape :StepperShape[A, S])
	:JavaIteratorShapeWithStepper[A, JavaIterator[A], S] =
		(shape.shape match {
			case IntShape    => JavaIteratorShape.intJavaIteratorShape
			case LongShape   => JavaIteratorShape.longJavaIteratorShape
			case DoubleShape => JavaIteratorShape.doubleJavaIteratorShape
			case CharShape   => JavaIteratorShape.charJavaIteratorShape
			case ByteShape   => JavaIteratorShape.byteJavaIteratorShape
			case FloatShape  => JavaIteratorShape.floatJavaIteratorShape
			case ShortShape  => JavaIteratorShape.floatJavaIteratorShape
			case _           => anyJavaIteratorShape
		}).asInstanceOf[JavaIteratorShapeWithStepper[A, JavaIterator[A], S]]
}

private[collections] sealed abstract class Rank3JavaIteratorShapes {
	type JavaIteratorShapeWithStepper[-A, I <: JavaIterator[_], S <: Stepper[_]] =
		JavaIteratorShape[A, I] { type Stepper = S }

	implicit def anyJavaIteratorShape[A] :JavaIteratorShape[A, JavaIterator[A]] =
		refJavaIteratorShape.asInstanceOf[JavaIteratorShapeWithStepper[A, JavaIterator[A], Stepper[A]]]

	private[this] val refJavaIteratorShape =
		new IteratorShape[Any, JavaIterator[Any]](ReferenceShape, JavaIterator.empty)
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
	type JavaIterator <: collections.JavaIterator[_]
	type Stepper      <: collection.Stepper[_]

	import JavaIteratorShape._

	def javaIteratorShape :JavaIteratorShapeWithStepper[E, JavaIterator, Stepper] = (
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
	).asInstanceOf[JavaIteratorShapeWithStepper[E, JavaIterator, Stepper]]

	def stepperShape :StepperShape[E, Stepper] =
		javaIteratorShape.stepperShape.castFrom[StepperShape[E, _ <: collection.Stepper[_]], StepperShape[E, Stepper]]

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
	type JteratorShapeAndTypes[-E, J <: Jterator[E], I <: JavaIterator[_], S <: Stepper[_]] =
		JteratorShape[E, J] { type JavaIterator = J; type Stepper = S }

	private def apply[E, J <: Jterator[E], I <: JavaIterator[_], S <: Stepper[_]]
	                 (shape :Opt[Shape]) :JteratorShapeAndTypes[E, J, I, S] =
		new JteratorShape[E, J](shape).asInstanceOf[JteratorShapeAndTypes[E, J, I, S]]

	implicit val intJteratorShape     :JteratorShapeAndTypes[Int, IntJterator, JavaIntIterator, IntStepper] =
		apply(Got(IntShape))
	implicit val longJteratorShape    :JteratorShapeAndTypes[Long, LongJterator, JavaLongIterator, LongStepper] =
		apply(Got(LongShape))
	implicit val doubleJteratorShape  :JteratorShapeAndTypes[Double, DoubleJterator, JavaDoubleIterator, DoubleStepper] =
		apply(Got(DoubleShape))
	implicit val floatJteratorShape   :JteratorShapeAndTypes[Float, FloatJterator, JavaDoubleIterator, DoubleStepper] =
		apply(Got(FloatShape))
	implicit val shortJteratorShape   :JteratorShapeAndTypes[Short, ShortJterator, JavaIntIterator, IntStepper] =
		apply(Got(ShortShape))
	implicit val charJteratorShape    :JteratorShapeAndTypes[Char, CharJterator, JavaIntIterator, IntStepper] =
		apply(Got(CharShape))
	implicit val byteJteratorShape    :JteratorShapeAndTypes[Byte, ByteJterator, JavaIntIterator, IntStepper] =
		apply(Got(ByteShape))
	implicit val booleanJteratorShape :JteratorShape[Boolean, BooleanJterator] = new JteratorShape(Lack)

	implicit def refJteratorShape[T <: AnyRef] :JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]] =
		anyRefJteratorShape.asInstanceOf[JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]]]

	private val anyRefJteratorShape = new JteratorShape[AnyRef, RefJterator[AnyRef]](Got(ReferenceShape))
}

private[collections] sealed abstract class JteratorShapeForAny {
	@inline implicit def anyJteratorShape[T] :JteratorShape[T, JavaIterator[T]] =
		JteratorShape.refJteratorShape.asInstanceOf[JteratorShape[T, JavaIterator[T]]]
}

