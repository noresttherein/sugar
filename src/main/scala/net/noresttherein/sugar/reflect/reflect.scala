package net.noresttherein.sugar

import java.util.Arrays

import scala.collection.concurrent.TrieMap
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JLong, JShort}
import net.noresttherein.sugar.extensions.{ClassExtension, castTypeParamMethods, immutableMapExtension}
import net.noresttherein.sugar.matching.BooleanMatchPattern
import net.noresttherein.sugar.vars.Opt




package object reflect {
	private[reflect] final val Ver = 1L

	@SerialVersionUID(Ver)
	object Boxed {
		/** If `clss` is a Java primitive type, return its specific box class. Otherwise return the argument itself. */
		def apply(clss :Class[_]) :Class[_] = if (clss.isPrimitive) Wrappers(clss) else clss

		/** If `clss` is a box class for a Java primitive type, return its unboxed primitive class. */
		def unapply(clss :Class[_]) :Opt[Class[_]] = Opt(PrimitiveClass.getOrElse(clss, null))

		private[this] val Wrappers :Map[Class[_], Class[_]] = BoxClass
	}

	@SerialVersionUID(Ver)
	object Unboxed {
		/** If the argument is a box class for a Java primitive type, return the appropriate primitive type.
		  * Otherwise return the argument itself.
		  */
		def apply(clss :Class[_]) :Class[_] = Unwrapped.getOrElse(clss, clss)

		/** If `clss` is a Java primitive type, return its standard boxed reference class. */
		def unapply(clss :Class[_]) :Opt[Class[_]] = Opt(BoxClass.getOrElse(clss, null))

		private[this] val Unwrapped :Map[Class[_], Class[_]] = PrimitiveClass
	}


	/** Provides the class object for `Array[E]`, given `Class[E]`. */
	@SerialVersionUID(Ver)
	object ArrayClass {
		/** Returns a class `c` such that `c.getComponentType == clss`. */
		def apply[X](clss :Class[X]) :Class[Array[X]] =
			cache.getOrElseApply(clss, java.lang.reflect.Array.newInstance(_, 0).getClass).castParam[Array[X]]

		/** If the argument class is an array class, returns the class of its elements. */
		def unapply(clss :Class[_]) :Opt[Class[_]] = Opt(clss.getComponentType)

		private[this] val cache = Map[Class[_], Class[_]](
			classOf[AnyRef]   -> classOf[Array[AnyRef]],
			classOf[String]   -> classOf[Array[String]],
			classOf[Byte]     -> classOf[Array[Byte]],
			classOf[Short]    -> classOf[Array[Short]],
			classOf[Char]     -> classOf[Array[Char]],
			classOf[Int]      -> classOf[Array[Int]],
			classOf[Long]     -> classOf[Array[Long]],
			classOf[Float]    -> classOf[Array[Float]],
			classOf[Double]   -> classOf[Array[Double]],
			classOf[Boolean]  -> classOf[Array[Boolean]],
			classOf[Unit]     -> classOf[Array[BoxedUnit]],
			classOf[JByte]    -> classOf[Array[JByte]],
			classOf[JShort]   -> classOf[Array[JShort]],
			classOf[JChar]    -> classOf[Array[JChar]],
			classOf[JInt]     -> classOf[Array[JInt]],
			classOf[JLong]    -> classOf[Array[JLong]],
			classOf[JFloat]   -> classOf[Array[JFloat]],
			classOf[JDouble]  -> classOf[Array[JDouble]],
			classOf[JBoolean] -> classOf[Array[JBoolean]],
		)
	}

	/** Maps all classes representable in `JVM` to their boxed representations. */
	final val BoxClass = Map[Class[_], Class[_]](
		classOf[Byte]    -> classOf[java.lang.Byte],
		classOf[Short]   -> classOf[java.lang.Short],
		classOf[Char]    -> classOf[java.lang.Character],
		classOf[Int]     -> classOf[java.lang.Integer],
		classOf[Long]    -> classOf[java.lang.Long],
		classOf[Float]   -> classOf[java.lang.Float],
		classOf[Double]  -> classOf[java.lang.Double],
		classOf[Boolean] -> classOf[java.lang.Boolean],
		classOf[Unit]    -> classOf[BoxedUnit]
	) //withDefault identity[Class[_]]


	/** Reverse map of java primitive boxes containing entries for all java primitives and `Unit`,
	  * mapping the box classes (i.e. `java.lang.Integer`) to synthetic classes representing actual primitive types
	  * (i.e. `Integer.TYPE`).
	  */
	final val PrimitiveClass :Map[Class[_], Class[_]] = BoxClass map { case (primitive, box) => box -> primitive }

	/** Matching pattern for classes representing Java primitive types. */
	final val Primitive = BooleanMatchPattern(BoxClass.contains(_ :Class[_]))

//	/** Maps all classes to their corresponding primitives (for java primitive boxes) or themselves (all other). */
//	final val UnboxedClass = PrimitiveClass withDefault identity[Class[_]]

	/** Mapping from any `jvm` class to their representations in specialized generic methods.
	  * `classOf[V] -> classOf[V]` for all builtin scala value classes,
	  * `classOf[T] -> classOf[Any]` for all others.
	  */
	final val SpecializationClass = Map[Class[_], Class[_]](
		classOf[Byte]    -> classOf[Byte],
		classOf[Short]   -> classOf[Short],
		classOf[Char]    -> classOf[Char],
		classOf[Int]     -> classOf[Int],
		classOf[Long]    -> classOf[Long],
		classOf[Float]   -> classOf[Float],
		classOf[Double]  -> classOf[Double],
		classOf[Boolean] -> classOf[Boolean],
		classOf[Unit]    -> classOf[Unit]
	) withDefaultValue classOf[Any]
}
