package net.noresttherein.sugar.collections


import scala.annotation.{implicitNotFound, switch}
import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, Stepper, StepperShape}
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, Shape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JByte, JChar, JDouble, JFloat, JInt, JIterator, JLong, JShort}
import net.noresttherein.sugar.collections
import net.noresttherein.sugar.collections.JavaIteratorShape.IteratorShape
import net.noresttherein.sugar.collections.extensions.StepType
import net.noresttherein.sugar.extensions.castingMethods
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




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
	/** The specific Scala [[scala.collection.Stepper Stepper]] type corresponding to this Java iterator type. */
	type Stepper  <: collection.Stepper[_]

	/** The specific [[net.noresttherein.sugar.collections.Jterator Jterator]] type corresponding
	  * to this Java iterator type. While this type is never definite, because there is no bijection between
	  * [[net.noresttherein.sugar.collections.JavaIterator JavaIterator]] and `Jterator`, it enables to reference
	  * this particular type and its tracking by the compiler.
	  * @example
	  * {{{
	  *     jterator(shape.jteratorShape).asJava(shape.jteratorShape)
	  * }}}
	  */
	type Jterator <: collections.Jterator[_]

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

	def jteratorShape :JteratorShape[A, Jterator] { type JavaIterator = I } =
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
		}).asInstanceOf[JteratorShape[A, Jterator] { type JavaIterator = I }]

	/** The type of the elements returned by this `JavaIterator` type.
	  * @return a `StepType` for one of `IntShape`, `LongShape`, `DoubleShape` and `ReferenceShape`.
	  */
	def stepType :StepType[_ >: A, Stepper] = StepType.fromStepperShape(stepperShape)


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
	private[collections] class IteratorShape[A, I <: JavaIterator[_]](override val shape :Shape, override val empty :I)
		extends JavaIteratorShape[A, I]

	implicit def javaIteratorShapeToStepperShape[A, I](shape :JavaIteratorShape[A, I]) :StepperShape[A, shape.Stepper] =
		shape.stepperShape

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
final class JteratorShape[-E, I] private (val shape :Maybe[StepperShape.Shape]) extends Equals with Serializable {
	/** The specific Java [[net.noresttherein.sugar.collections.JavaIterator iterator]] type backing this jterator type. */
	type JavaIterator <: collections.JavaIterator[_]

	/** The Scala [[scala.collection.Stepper Stepper]] type corresponding to the element type of this jterator type. */
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
		case Yes(ReferenceShape) => "JteratorShape[_<:AnyRef, RefJterator[_]]"
		case Yes(IntShape)       => "JteratorShape[Int, IntJterator]"
		case Yes(LongShape)      => "JteratorShape[Long, LongJterator]"
		case Yes(DoubleShape)    => "JteratorShape[Double, DoubleJterator]"
		case Yes(FloatShape)     => "JteratorShape[Float, FloatJterator]"
		case Yes(ShortShape)     => "JteratorShape[Short, ShortJterator]"
		case Yes(CharShape)      => "JteratorShape[Char, CharJterator]"
		case Yes(ByteShape)      => "JteratorShape[Byte, ByteJterator]"
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
	                 (shape :Maybe[Shape]) :JteratorShapeAndTypes[E, J, I, S] =
		new JteratorShape[E, J](shape).asInstanceOf[JteratorShapeAndTypes[E, J, I, S]]

	
	implicit val intJteratorShape     :JteratorShapeAndTypes[Int, IntJterator, JavaIntIterator, IntStepper] =
		apply(Yes(IntShape))
	implicit val longJteratorShape    :JteratorShapeAndTypes[Long, LongJterator, JavaLongIterator, LongStepper] =
		apply(Yes(LongShape))
	implicit val doubleJteratorShape  :JteratorShapeAndTypes[Double, DoubleJterator, JavaDoubleIterator, DoubleStepper] =
		apply(Yes(DoubleShape))
	implicit val floatJteratorShape   :JteratorShapeAndTypes[Float, FloatJterator, JavaDoubleIterator, DoubleStepper] =
		apply(Yes(FloatShape))
	implicit val shortJteratorShape   :JteratorShapeAndTypes[Short, ShortJterator, JavaIntIterator, IntStepper] =
		apply(Yes(ShortShape))
	implicit val charJteratorShape    :JteratorShapeAndTypes[Char, CharJterator, JavaIntIterator, IntStepper] =
		apply(Yes(CharShape))
	implicit val byteJteratorShape    :JteratorShapeAndTypes[Byte, ByteJterator, JavaIntIterator, IntStepper] =
		apply(Yes(ByteShape))
	implicit val booleanJteratorShape :JteratorShape[Boolean, BooleanJterator] = new JteratorShape(No)

	implicit def refJteratorShape[T <: AnyRef] :JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]] =
		anyRefJteratorShape.asInstanceOf[JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]]]

	private val anyRefJteratorShape = new JteratorShape[AnyRef, RefJterator[AnyRef]](Yes(ReferenceShape))
	
	@inline implicit def intJteratorShapeFromStepperShape(stepperShape :StepperShape[Int, IntStepper]) 
			:intJteratorShape.type =
		intJteratorShape
	
	@inline implicit def longJteratorShapeFromStepperShape(stepperShape :StepperShape[Long, LongStepper]) 
			:longJteratorShape.type =
		longJteratorShape
	
	@inline implicit def doubleJteratorShapeFromStepperShape(stepperShape :StepperShape[Double, DoubleStepper]) 
			:doubleJteratorShape.type =
		doubleJteratorShape
	
	@inline implicit def byteJteratorShapeFromStepperShape(stepperShape :StepperShape[Byte, IntStepper])
			:byteJteratorShape.type =
		byteJteratorShape
	
	@inline implicit def charJteratorShapeFromStepperShape(stepperShape :StepperShape[Char, IntStepper])
			:charJteratorShape.type =
		charJteratorShape
	
	@inline implicit def shortJteratorShapeFromStepperShape(stepperShape :StepperShape[Short, IntStepper])
			:shortJteratorShape.type =
		shortJteratorShape
	
	@inline implicit def floatJteratorShapeFromStepperShape(stepperShape :StepperShape[Float, DoubleStepper])
			:floatJteratorShape.type =
		floatJteratorShape

	implicit def refJteratorShapeFromStepperShape[T <: AnyRef](stepperShape :StepperShape[T, AnyStepper[T]])
			:JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]] =
		anyRefJteratorShape.asInstanceOf[JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]]]

	
	@inline implicit def intJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Int, JavaIntIterator])
			:intJteratorShape.type =
		intJteratorShape
	
	@inline implicit def longJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Long, JavaLongIterator])
			:longJteratorShape.type =
		longJteratorShape
	
	@inline implicit def doubleJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Double, JavaDoubleIterator])
			:doubleJteratorShape.type =
		doubleJteratorShape
	
	@inline implicit def byteJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Byte, JavaIntIterator])
			:byteJteratorShape.type =
		byteJteratorShape
	
	@inline implicit def charJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Char, JavaIntIterator])
			:charJteratorShape.type =
		charJteratorShape
	
	@inline implicit def shortJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Short, JavaIntIterator])
			:shortJteratorShape.type =
		shortJteratorShape
	
	@inline implicit def floatJteratorShapeFromJavaIteratorShape(__ :JavaIteratorShape[Float, JavaDoubleIterator])
			:floatJteratorShape.type =
		floatJteratorShape

	implicit def refJteratorShapeFromJavaIteratorShape[T <: AnyRef](stepperShape :JavaIteratorShape[T, AnyStepper[T]])
			:JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]] =
		anyRefJteratorShape.asInstanceOf[JteratorShapeAndTypes[T, RefJterator[T], JavaIterator[T], AnyStepper[T]]]

}


private[collections] sealed abstract class JteratorShapeForAny {
	@inline implicit def anyJteratorShape[T] :JteratorShape[T, Jterator[T]] =
		JteratorShape.refJteratorShape.asInstanceOf[JteratorShape[T, Jterator[T]]]
}

