package net.noresttherein.sugar




/** Type aliases for ubiquitous Java types which have Scala counterparts sharing their name. */
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
	type JList[T]          = java.util.List[T]
	type JArrayList[T]     = java.util.ArrayList[T]
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
}
