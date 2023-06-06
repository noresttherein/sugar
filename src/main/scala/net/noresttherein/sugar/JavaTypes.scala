package net.noresttherein.sugar




/** Type aliases for ubiquitous Java types which have Scala counterparts sharing their name. */
@SerialVersionUID(Ver)
object JavaTypes {
	type JVoid             = java.lang.Void
	type JBoolean          = java.lang.Boolean
	type JByte             = java.lang.Byte
	type JShort            = java.lang.Short
	type JInt              = java.lang.Integer
	type JLong             = java.lang.Long
	type JChar             = java.lang.Character
	type JFloat            = java.lang.Float
	type JDouble           = java.lang.Double

	type JBigDecimal       = java.math.BigDecimal
	type JBigInt           = java.math.BigInteger

	type JStringBuilder    = java.lang.StringBuilder
	type JIterator[T]      = java.util.Iterator[T]
	type JIntIterator      = java.util.PrimitiveIterator.OfInt
	type JLongIterator     = java.util.PrimitiveIterator.OfLong
	type JDoubleIterator   = java.util.PrimitiveIterator.OfDouble
	type JCollection[T]    = java.util.Collection[T]
	type JList[T]          = java.util.List[T]
	type JArrayList[T]     = java.util.ArrayList[T]
	type JLinkedList[T]    = java.util.LinkedList[T]
	type JSet[T]           = java.util.Set[T]
	type JHashSet[T]       = java.util.HashSet[T]
	type JTreeSet[T]       = java.util.TreeSet[T]
	type JBitSet           = java.util.BitSet
	type JMap[K, V]        = java.util.Map[K, V]
	type JHashMap[K, V]    = java.util.HashMap[K, V]
	type JTreeMap[K, V]    = java.util.TreeMap[K, V]
	type JQueue[T]         = java.util.Queue[T]
	type JDeque[T]         = java.util.Deque[T]
	type JPriorityQueue[T] = java.util.PriorityQueue[T]

	/** Implicit unboxing of java wrappers of primitive types (`Integer`, `java.lang.Long`, etc.) to Scala value types. */
	object unboxingConversions {
		@inline implicit def unboxBoolean(x :JBoolean) :Boolean = x.booleanValue
		@inline implicit def unboxByte(x :JByte)       :Byte = x.byteValue
		@inline implicit def unboxShort(x :JShort)     :Short = x.shortValue
		@inline implicit def unboxChar(x :JChar)       :Char = x.charValue
		@inline implicit def unboxInt(x :JInt)         :Int = x.intValue
		@inline implicit def unboxLong(x :JLong)       :Long = x.longValue
		@inline implicit def unboxFloat(x :JFloat)     :Float = x.floatValue
		@inline implicit def unboxDouble(x :JDouble)   :Double = x.doubleValue

		@inline implicit def javaBooleanIsBoolean :JBoolean =:= Boolean = ev.asInstanceOf[JBoolean =:= Boolean]
		@inline implicit def javaByteIsByte       :JByte =:= Byte = ev.asInstanceOf[JByte =:= Byte]
		@inline implicit def javaShortIsShort     :JShort =:= Short = ev.asInstanceOf[JShort =:= Short]
		@inline implicit def javaCharIsChar       :JChar =:= Char = ev.asInstanceOf[JChar =:= Char]
		@inline implicit def javaIntIsInt         :JInt =:= Int = ev.asInstanceOf[JInt =:= Int]
		@inline implicit def javaLongIsLong       :JLong =:= Long = ev.asInstanceOf[JLong =:= Long]
		@inline implicit def javaFloatIsFloat     :JFloat =:= Float = ev.asInstanceOf[JFloat =:= Float]
		@inline implicit def javaDoubleIsDouble   :JDouble =:= Double = ev.asInstanceOf[JDouble =:= Double]

		private[this] val ev :Any =:= Any = implicitly[Any=:=Any]
	}

	object boxingConversions {
		import java.{lang => j}
		@inline implicit def boxBoolean(x :Boolean) :JBoolean = j.Boolean.valueOf(x)
		@inline implicit def boxByte(x :Byte)       :JByte    = j.Byte.valueOf(x)
		@inline implicit def boxShort(x :Short)     :JShort   = j.Short.valueOf(x)
		@inline implicit def boxChar(x :Char)       :JChar    = j.Character.valueOf(x)
		@inline implicit def boxInt(x :Int)         :JInt     = j.Integer.valueOf(x)
		@inline implicit def boxLong(x :Long)       :JLong    = j.Long.valueOf(x)
		@inline implicit def boxFloat(x :Float)     :JFloat   = j.Float.valueOf(x)
		@inline implicit def boxDouble(x :Double)   :JDouble  = j.Double.valueOf(x)

		@inline implicit def booleanIsJavaBoolean :Boolean =:= JBoolean = ev.asInstanceOf[Boolean =:= JBoolean]
		@inline implicit def byteIsJavaByte       :Byte =:= JByte = ev.asInstanceOf[Byte =:= JByte]
		@inline implicit def shortIsJavaShort     :Short =:= JShort = ev.asInstanceOf[Short =:= JShort]
		@inline implicit def charIsJavaChar       :Char =:= JChar = ev.asInstanceOf[Char =:= JChar]
		@inline implicit def intIsJavaInt         :Int =:= JInt = ev.asInstanceOf[Int =:= JInt]
		@inline implicit def longIsJavaLong       :Long =:= JLong = ev.asInstanceOf[Long =:= JLong]
		@inline implicit def floatIsJavaFloat     :Float =:= JFloat = ev.asInstanceOf[Float =:= JFloat]
		@inline implicit def doubleIsJavaDouble   :Double =:= JDouble = ev.asInstanceOf[Double =:= JDouble]

		private[this] val ev :Any =:= Any = implicitly[Any=:=Any]
	}
}
