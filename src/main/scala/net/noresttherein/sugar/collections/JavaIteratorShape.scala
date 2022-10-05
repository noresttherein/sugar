package net.noresttherein.sugar.collections

import java.util.PrimitiveIterator

import scala.collection.{Stepper, StepperShape}
import scala.collection.StepperShape.{ByteShape, CharShape, DoubleShape, FloatShape, IntShape, LongShape, ReferenceShape, Shape, ShortShape}

import net.noresttherein.sugar.JavaTypes.{JByte, JChar, JDouble, JDoubleIterator, JFloat, JInt, JIntIterator, JIterator, JLong, JLongIterator, JShort}
import net.noresttherein.sugar.collections.JavaIteratorShape.IteratorShape
import net.noresttherein.sugar.extensions.cast2TypeParams




/** An implicit witness providing the most specialized Java Iterator subtype `I` for a given element type `A`.
  * For value types, it specifies one of
  * [[java.util.PrimitiveIterator PrimitiveIterator]]`.{`[[java.util.PrimitiveIterator.OfInt OfInt]]`,`[[java.util.PrimitiveIterator.OfLong OfLong]]`.`[[java.util.PrimitiveIterator.OfDouble OfDouble]]`}`.
  * Very similar to [[scala.collection.StepperShape StepperShape]], it allows to define methods returning
  * a `java.util.Iterator`.
  * @author Marcin Mościcki
  */
trait JavaIteratorShape[-A, I <: JIterator[_]] extends Equals with Serializable {
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

	def empty :I

	override def equals(that :Any) :Boolean = that match {
		case other :JavaIteratorShape[_, _] if other canEqual this => (this eq other) || shape == other.shape
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JavaIteratorShape[_, _]]
	override def hashCode :Int = shape.hashCode
}


private[collections] sealed abstract class Rank3JavaIteratorShapes {
	implicit def anyJavaIteratorShape[A] :JavaIteratorShape[A, JIterator[A]] =
		refJavaIteratorShape.asInstanceOf[JavaIteratorShape[A, JIterator[A]]]

	private[this] val refJavaIteratorShape = new IteratorShape[Any, JIterator[Any]](ReferenceShape, JavaIterator.empty)
}

private[collections] sealed abstract class Rank2JavaIteratorShapes extends Rank3JavaIteratorShapes {
	implicit def stepperJavaIteratorShape[A, S <: Stepper[_]](implicit shape :StepperShape[A, S])
			:JavaIteratorShape[A, JIterator[A]] =
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
	implicit val byteJavaIteratorShape   :JavaIteratorShape[Byte, JIntIterator] =
		new IteratorShape(ByteShape, JavaIterator.ofInt())
	implicit val shortJavaIteratorShape  :JavaIteratorShape[Short, JIntIterator] =
		new IteratorShape(ShortShape, JavaIterator.ofInt())
	implicit val charJavaIteratorShape   :JavaIteratorShape[Char, JIntIterator] =
		new IteratorShape(CharShape, JavaIterator.ofInt())
	implicit val floatJavaIteratorShape  :JavaIteratorShape[Float, JDoubleIterator] =
		new IteratorShape(FloatShape, JavaIterator.ofDouble())

	implicit val javaByteJavaIteratorShape :JavaIteratorShape[JByte, JIntIterator] =
		byteJavaIteratorShape.asInstanceOf[JavaIteratorShape[JByte, JIntIterator]]
	implicit val javaShortJavaIteratorShape :JavaIteratorShape[JShort, JIntIterator] =
		shortJavaIteratorShape.asInstanceOf[JavaIteratorShape[JShort, JIntIterator]]
	implicit val javaCharJavaIteratorShape :JavaIteratorShape[JChar, JIntIterator] =
		charJavaIteratorShape.asInstanceOf[JavaIteratorShape[JChar, JIntIterator]]
	implicit val javaIntJavaIteratorShape :JavaIteratorShape[JInt, JIntIterator] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShape[JInt, JIntIterator]]
	implicit val javaLongJavaIteratorShape :JavaIteratorShape[JLong, JLongIterator] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShape[JLong, JLongIterator]]
	implicit val javaFloatJavaIteratorShape :JavaIteratorShape[JFloat, JDoubleIterator] =
		floatJavaIteratorShape.asInstanceOf[JavaIteratorShape[JFloat, JDoubleIterator]]
	implicit val javaDoubleJavaIteratorShape :JavaIteratorShape[JDouble, JDoubleIterator] =
		JavaIteratorShape.asInstanceOf[JavaIteratorShape[JDouble, JDoubleIterator]]
}

@SerialVersionUID(ver)
object JavaIteratorShape extends Rank1JavaIteratorShapes {
	@SerialVersionUID(ver)
	private[collections] class IteratorShape[A, I <: JIterator[_]](override val shape :Shape, override val empty :I)
		extends JavaIteratorShape[A, I]

	implicit val intJavaIteratorShape    :JavaIteratorShape[Int, JIntIterator] =
		new IteratorShape(IntShape, JavaIterator.ofInt())
	implicit val longJavaIteratorShape   :JavaIteratorShape[Long, JLongIterator] =
		new IteratorShape(LongShape, JavaIterator.ofLong())
	implicit val doubleJavaIteratorShape :JavaIteratorShape[Double, JDoubleIterator] =
		new IteratorShape(DoubleShape, JavaIterator.ofDouble())
}
