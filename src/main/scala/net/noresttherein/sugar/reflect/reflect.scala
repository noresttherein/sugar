package net.noresttherein.sugar

import java.lang.reflect.{ParameterizedType, Type}

import scala.collection.immutable.ArraySeq
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JLong, JShort}
import net.noresttherein.sugar.extensions.{castTypeParamMethods, immutableMapExtension}
import net.noresttherein.sugar.matching.BooleanMatchPattern
import net.noresttherein.sugar.vars.Opt




package reflect {

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


	/** `Class` constants for commonly used types. */
	@SerialVersionUID(Ver)
	object classes { //consider: adding suffix Class to field names.
		val Byte      :Class[Byte]      = classOf[Byte]
		val Short     :Class[Short]     = classOf[Short]
		val Char      :Class[Char]      = classOf[Char]
		val Int       :Class[Int]       = classOf[Int]
		val Long      :Class[Long]      = classOf[Long]
		val Float     :Class[Float]     = classOf[Float]
		val Double    :Class[Double]    = classOf[Double]
		val Boolean   :Class[Boolean]   = classOf[Boolean]
		val Void      :Class[Void]      = java.lang.Void.TYPE
		val Unit      :Class[Unit]      = classOf[Unit]
		val JByte     :Class[JByte]     = classOf[JByte]
		val JShort    :Class[JShort]    = classOf[JShort]
		val JChar     :Class[JChar]     = classOf[JChar]
		val JInt      :Class[JInt]      = classOf[JInt]
		val JLong     :Class[JLong]     = classOf[JLong]
		val JFloat    :Class[JFloat]    = classOf[JFloat]
		val JDouble   :Class[JDouble]   = classOf[JDouble]
		val JBoolean  :Class[JBoolean]  = classOf[JBoolean]
		val AnyRef    :Class[AnyRef]    = classOf[AnyRef]
		val BoxedUnit :Class[BoxedUnit] = classOf[BoxedUnit]

		val ByteArray    :Class[Array[Byte]]    = classOf[Array[Byte]]
		val ShortArray   :Class[Array[Short]]   = classOf[Array[Short]]
		val CharArray    :Class[Array[Char]]    = classOf[Array[Char]]
		val IntArray     :Class[Array[Int]]     = classOf[Array[Int]]
		val LongArray    :Class[Array[Long]]    = classOf[Array[Long]]
		val FloatArray   :Class[Array[Float]]   = classOf[Array[Float]]
		val DoubleArray  :Class[Array[Double]]  = classOf[Array[Double]]
		val BooleanArray :Class[Array[Boolean]] = classOf[Array[Boolean]]
		val AnyRefArray  :Class[Array[AnyRef]]  = classOf[Array[AnyRef]]
	}
}




package object reflect {
	private[reflect] final val Ver = 1L

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


	/** Uses java reflection to determine the type of the `n`-th type argument given to super type `givenTo` of `tpe`
	  * by the concrete class of this object.
	  */
	private[sugar] def typeArgumentOf(tpe :Type, n :Int, givenTo :Class[_]) :Type = tpe match {
		case null => null

		case p :ParameterizedType =>
			if (p.getRawType == givenTo) {
				p.getActualTypeArguments()(n)
			} else p.getRawType match { //recurse down the generic definition
				case cls :Class[_] => typeArgumentOf(cls, n, givenTo) match {
					case res :Class[_] => res //parameterized with a concrete type
					case param =>
						//try to match formal type parameters with actual arguments
						val i = ArraySeq.unsafeWrapArray(cls.getTypeParameters).indexOf(param)
						if (i >= 0)
							p.getActualTypeArguments()(i)
						else //possible if param is declared by an enclosing class, but we won't check the outer scopes
							param
				}
				case _ => typeArgumentOf(p.getRawType, n, givenTo) //never happens IRL
			}

		case cls :Class[_] =>
			var res = typeArgumentOf(cls.getGenericSuperclass, n, givenTo)
			val it  = cls.getGenericInterfaces.iterator
			while (it.hasNext) { //look for the most narrow definition
				val next = typeArgumentOf(it.next(), n, givenTo)
				if (next != null)
					if (res == null || !res.isInstanceOf[Class[_]])
						res = next
					else if (n == 0) next match {
						case c :Class[_] if c.isAssignableFrom(res.asInstanceOf[Class[_]]) =>
							res = c
						case _ =>
					} else next match {
						case c :Class[_] if res.asInstanceOf[Class[_]].isAssignableFrom(c) =>
							res = c
						case _ =>
					}
			}
			res
		case _ => null
	}
}
