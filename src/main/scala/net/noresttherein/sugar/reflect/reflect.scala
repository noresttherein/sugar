package net.noresttherein.sugar

import scala.runtime.BoxedUnit

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JLong, JShort}
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.vars.Opt




package object reflect {
	final val Ver = 1L

	@SerialVersionUID(Ver)
	object Boxed {
		/** If `clss` is a Java primitive type, return its specific box class. Otherwise return the argument itself. */
		def apply(clss :Class[_]) :Class[_] = if (clss.isPrimitive) Wrappers(clss) else clss

		def unapply(clss :Class[_]) :Opt[Class[_]] = Opt(PrimitiveClass.getOrElse(clss, null))

		private[this] val Wrappers :Map[Class[_], Class[_]] = BoxClass
	}

	@SerialVersionUID(Ver)
	object Unboxed {
		/** If the argument is a box class for a Java primitive type, return the appropriate primitive type.
		  * Otherwise return the argument itself.
		  */
		def apply(clss :Class[_]) :Class[_] = Unwrapped.getOrElse(clss, clss)

		def unapply(clss :Class[_]) :Opt[Class[_]] = Opt(BoxClass.getOrElse(clss, null))

		private[this] val Unwrapped :Map[Class[_], Class[_]] = PrimitiveClass
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
	final val PrimitiveClass = BoxClass map { case (primitive, box) => box -> primitive }

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
