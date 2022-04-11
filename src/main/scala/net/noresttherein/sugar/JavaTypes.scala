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

	type JBigDecimal      = java.math.BigDecimal
	type JBigInt          = java.math.BigInteger

	type JStringBuilder   = java.lang.StringBuilder
	type JIterator[T]     = java.util.Iterator[T]
	type JSpliterator[T]  = java.util.Spliterator[T]
	type JList[T]         = java.util.List[T]
	type JArrayList[T]    = java.util.ArrayList[T]

//	val JVoid            = java.lang.Void.TYPE
//	val JBoolean         = java.lang.Boolean.TYPE
//	val JByte            = java.lang.Byte.TYPE
//	val JShort           = java.lang.Short.TYPE
//	val JInt             = java.lang.Integer.TYPE
//	val JLong            = java.lang.Long.TYPE
//	val JChar            = java.lang.Character.TYPE
//	val JFloat           = java.lang.Float.TYPE
//	val JDouble          = java.lang.Double.TYPE

}
