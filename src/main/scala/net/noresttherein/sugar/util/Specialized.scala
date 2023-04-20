package net.noresttherein.sugar.util

import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.util.SpecTag.Default




/** An enumeration of all primitive types (standard value types) as well as default, erased context.
  * It is a replacement of Scala 2 `@specialized` tags, allowing to pick between manually specialized subclasses
  * based on type parameter `X`. It works in a very similar fashion to [[scala.reflect.ClassTag ClassTag]],
  * but, unlike the latter, there are no instances for arbitrary classes, and an implicit instance for `Any` is always
  * present (superseded by tags for individual value types if type `X` is known).
  * @see [[net.noresttherein.sugar.util.Specialized]]
  */
sealed trait SpecTag[X] {
	/** A type selector equal to one of its type parameters, or `T[X]`, corresponding to this tag. */
	type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char],
	          OfFloat <: T[Float], OfDouble <: T[Double], OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		<: T[X]

	/** Invokes the parameterless method of `specialized` corresponding to this tag. */
	def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char],
	          OfFloat <: T[Float], OfDouble <: T[Double], OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
	         (specialized :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit])
			:Pick[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]

	/** The class tag representing the specialization context. If `X` is abstract or a reference type,
	  * then `classTag.runtimeClass == classOf[Any]`.
	  */
	def classTag :ClassTag[X]
}




private[util] sealed abstract class Rank2SpecTags {
	implicit def default[X] :Default[X] = prototype.asInstanceOf[Default[X]]

	private[this] val prototype = new Default[Any]
}


private[util] sealed abstract class Rank1SpecTags extends Rank2SpecTags {
	implicit def fromClassTag[X :ClassTag] :SpecTag[X] = {
		val cls = classTag[X].runtimeClass
		(if (classOf[AnyRef] isAssignableFrom cls)
			default[X]
		else if (cls == classOf[Int]) SpecTag.OfInt
		else if (cls == classOf[Long]) SpecTag.OfLong
		else if (cls == classOf[Double]) SpecTag.OfDouble
		else if (cls == classOf[Byte]) SpecTag.OfByte
		else if (cls == classOf[Char]) SpecTag.OfChar
		else if (cls == classOf[Float]) SpecTag.OfFloat
		else if (cls == classOf[Boolean]) SpecTag.OfBoolean
		else if (cls == classOf[scala.Unit]) SpecTag.OfUnit
		else if (cls == classOf[Short]) SpecTag.OfShort
		else default[X]
		).asInstanceOf[SpecTag[X]]
	}
}


object SpecTag extends Rank1SpecTags {
	private[SpecTag] abstract class BaseTag[X](override val toString :String)
	                                          (implicit override val classTag :ClassTag[X]) extends SpecTag[X]

	implicit object OfByte extends BaseTag[Byte]("Byte") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfByte

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfByte =
			specialized.ofByte
	}

	implicit object OfShort extends BaseTag[Short]("Short") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfShort

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfShort =
			specialized.ofShort
	}

	implicit object OfInt extends BaseTag[Int]("Int") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfInt

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfInt =
			specialized.ofInt
	}

	implicit object OfLong extends BaseTag[Long]("Long") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfLong

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfLong =
			specialized.ofLong
	}

	implicit object OfChar extends BaseTag[Char]("Char") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfChar

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfChar =
			specialized.ofChar
	}

	implicit object OfFloat extends BaseTag[Float]("Float") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfFloat

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfFloat =
			specialized.ofFloat
	}

	implicit object OfDouble extends BaseTag[Double]("Double") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfDouble

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfDouble =
			specialized.ofDouble
	}

	implicit object OfBoolean extends BaseTag[Boolean]("Boolean") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			OfBoolean

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :OfBoolean =
			specialized.ofBoolean
	}

	implicit object OfUnit extends BaseTag[scala.Unit]("Unit") {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			ofUnit

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :ofUnit =
			specialized.ofUnit
	}




	sealed class Default[X] private[util] extends BaseTag[X]("_")(ClassTag[X](classOf[Any])) {
		override type Pick[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
			T[X]

		override def apply[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long],
		                   OfChar <: T[Char], OfFloat <: T[Float], OfDouble <: T[Double],
		                   OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		                  (specialized
		                   :Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]
		                  ) :T[X] =
			specialized.default

		private def readResolve = SpecTag.default[X]
	}

}





/** A double dispatch callback allowing to create different, manually specialized subclasses of a base type `T`,
  * depending on its type argument. Each inbuilt value (primitive) type has a dedicated method, allowing to execute
  * distinct code based on the value of an implicit [[net.noresttherein.sugar.util.SpecTag SpecTag]]`[X]`.
  *
  * The companion object contains base classes representing various `Specializable.Group` defined by Scala 2.
  * This allows deriving a callback class from a type with fewer type parameters, which already forwards calls
  * for all omitted types to [[net.noresttherein.sugar.util.Specialized.default default]].
  * @author Marcin Mościcki
  */
trait Specialized[+T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char],
                  OfFloat <: T[Float], OfDouble <: T[Double], OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
{
//trait Specialized[+T[_]] {
//	type OfByte    <: T[Byte]
//	type OfShort   <: T[Short]
//	type OfChar    <: T[Char]
//	type OfInt     <: T[Int]
//	type OfLong    <: T[Long]
//	type OfFloat   <: T[Float]
//	type OfDouble  <: T[Double]
//	type OfBoolean <: T[Boolean]
//	type ofUnit    <: T[scala.Unit]

	/** Calls one of the parameterless methods, depending on if `X` is an inbuilt value type and which one,
	  * or [[net.noresttherein.sugar.util.Specialized.default default]] if `X` is not a Java primitive type
	  * or is abstract.
	  */
	def apply[X](implicit ev :SpecTag[X]) :ev.Pick[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]

	def ofByte    :OfByte
	def ofShort   :OfShort
	def ofInt     :OfInt
	def ofLong    :OfLong
	def ofChar    :OfChar
	def ofFloat   :OfFloat
	def ofDouble  :OfDouble
	def ofBoolean :OfBoolean
	def ofUnit    :ofUnit

	def default[X] :T[X]
}



object Specialized {
	type Primitives[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char],
	                OfFloat <: T[Float], OfDouble <: T[Double], OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
		Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]

	type Everything[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char],
	                OfFloat <: T[Float], OfDouble <: T[Double], OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]] =
		Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, OfBoolean, ofUnit]

	trait Bits32AndUp[T[_], OfInt <: T[Int], OfLong <: T[Long], OfFloat <: T[Float], OfDouble <: T[Double]]
		extends Specialized[T, T[Byte], T[Short], OfInt, OfLong, T[Char], OfFloat, OfDouble, T[Boolean], T[scala.Unit]]
	{
		override def ofByte    :T[Byte] = default
		override def ofShort   :T[Short] = default
		override def ofChar    :T[Char] = default
		override def ofBoolean :T[Boolean] = default
		override def ofUnit    :T[scala.Unit] = default
	}

	trait Integral[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char]]
		extends Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, T[Float], T[Double], T[Boolean], T[scala.Unit]]
	{
		override def ofFloat   :T[Float] = default
		override def ofDouble  :T[Double] = default
		override def ofBoolean :T[Boolean] = default
		override def ofUnit    :T[scala.Unit] = default
	}

	trait AllNumeric[T[_], OfByte <: T[Byte], OfShort <: T[Short], OfInt <: T[Int], OfLong <: T[Long], OfChar <: T[Char],
	                 OfFloat <: T[Float], OfDouble <: T[Double]]
		extends Specialized[T, OfByte, OfShort, OfInt, OfLong, OfChar, OfFloat, OfDouble, T[Boolean], T[scala.Unit]]
	{
		override def ofBoolean :T[Boolean] = default
		override def ofUnit    :T[scala.Unit] = default
	}

	trait BestOfBreed[T[_], OfInt <: T[Int], OfDouble <: T[Double], OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		extends Specialized[T, T[Byte], T[Short], OfInt, T[Long], T[Char], T[Float], OfDouble, OfBoolean, ofUnit]
	{
		override def ofByte    :T[Byte] = default
		override def ofShort   :T[Short] = default
		override def ofLong    :T[Long] = default
		override def ofChar    :T[Char] = default
		override def ofFloat   :T[Float] = default
	}


	trait Unit[T[_], OfUnit <: T[scala.Unit]]
		extends Specialized[T, T[Byte], T[Short], T[Int], T[Long], T[Char], T[Float], T[Double], T[Boolean], OfUnit]
	{
		override def ofByte    :T[Byte] = default
		override def ofShort   :T[Short] = default
		override def ofInt     :T[Int] = default
		override def ofLong    :T[Long] = default
		override def ofFloat   :T[Float] = default
		override def ofDouble  :T[Double] = default
		override def ofChar    :T[Char] = default
		override def ofBoolean :T[Boolean] = default
	}
	
	


	type Arg[T[_], OfInt <: T[Int], OfLong <: T[Long], OfFloat <: T[Float], OfDouble <: T[Double]] =
		Bits32AndUp[T, OfInt, OfLong, OfFloat, OfDouble]

	trait Args[T[_], OfInt <: T[Int], OfLong <: T[Long], OfDouble <: T[Double]]
		extends Specialized[T, T[Byte], T[Short], OfInt, OfLong, T[Char], T[Float], OfDouble, T[Boolean], T[scala.Unit]]
	{
		override def ofByte    :T[Byte] = default
		override def ofShort   :T[Short] = default
		override def ofFloat   :T[Float] = default
		override def ofChar    :T[Char] = default
		override def ofBoolean :T[Boolean] = default
		override def ofUnit    :T[scala.Unit] = default
	}
	type Stepper[T[_], OfInt <: T[Int], OfLong <: T[Long], OfDouble <: T[Double]] = Args[T, OfInt, OfLong, OfDouble]

	trait Return[T[_], OfInt <: T[Int], OfLong <: T[Long], OfFloat <: T[Float], OfDouble <: T[Double],
	             OfBoolean <: T[Boolean], ofUnit <: T[scala.Unit]]
		extends Specialized[T, T[Byte], T[Short], OfInt, OfLong, T[Char], OfFloat, OfDouble, T[Boolean], T[scala.Unit]]
	{
		override def ofByte    :T[Byte] = default
		override def ofShort   :T[Short] = default
		override def ofChar    :T[Char] = default
	}

}
