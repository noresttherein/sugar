package net.noresttherein.sugar.numeric

object UByteSpec {

	UByte(1.toByte) :UInt
	UByte(1.toByte) :Int
	UByte(1.toByte) :ULong
	UByte(1.toByte) :Long
	UByte(1.toByte) + 1.toByte :Int
	UByte(1.toByte) + UByte(1.toByte) :UByte
	UByte(1.toByte) + 1.toShort :Int
	UByte(1.toByte) + UShort(1.toShort) :UShort
	UByte(1.toByte) + 1 :Int
	UByte(1.toByte) + UInt(1) :UInt
	UByte(1.toByte) + 1L :Long
	UByte(1.toByte) + ULong(1) :ULong
	UByte(1.toByte) /% 1.toByte :(Int, Int)
	UByte(1.toByte) /% UByte(1.toByte) :(UByte, UByte)
	UByte(1.toByte) /% 1.toShort :(Int, Int)
	UByte(1.toByte) /% UShort(1.toShort) :(UShort, UShort)
	UByte(1.toByte) /% 1 :(Int, Int)
	UByte(1.toByte) /% UInt(1) :(UInt, UInt)
	UByte(1.toByte) /% 1L :(Long, Long)
	UByte(1.toByte) /% ULong(1) :(ULong, ULong)
	UByte(1.toByte) ** UByte(1.toByte)
	UByte(1.toByte) ** UShort(1.toShort)
	UByte(1.toByte) ** UInt(1)
	UByte(1.toByte) ** 1.toByte
	UByte(1.toByte) ** 1.toShort
	UByte(1.toByte) ** 1
	UByte(1.toByte) %/ 1.toByte
	UByte(1.toByte) %/ UByte(1.toByte)
	UByte(1.toByte) %/ 1.toShort
	UByte(1.toByte) %/ UShort(1.toShort)
	UByte(1.toByte) %/ 1
	UByte(1.toByte) %/ UInt(1)
	UByte(1.toByte) %/ 1L
}



object UShortSpec {

	UShort(1.toShort) :UInt
	UShort(1.toShort) :Int
	UShort(1.toShort) :ULong
	UShort(1.toShort) :Long
	UShort(1.toShort) + 1.toByte :Int
	UShort(1.toShort) + UByte(1.toByte) :UShort
	UShort(1.toShort) + 1.toShort :Int
	UShort(1.toShort) + UShort(1.toShort) :UShort
	UShort(1.toShort) + 1 :Int
	UShort(1.toShort) + UInt(1) :UInt
	UShort(1.toShort) + 1L :Long
	UShort(1.toShort) + ULong(1) :ULong
	UShort(1.toShort) /% 1.toByte :(Int, Int)
	UShort(1.toShort) /% UByte(1.toByte) :(UShort, UShort)
	UShort(1.toShort) /% 1.toShort :(Int, Int)
	UShort(1.toShort) /% UShort(1.toShort) :(UShort, UShort)
	UShort(1.toShort) /% 1 :(Int, Int)
	UShort(1.toShort) /% UInt(1) :(UInt, UInt)
	UShort(1.toShort) /% 1L :(Long, Long)
	UShort(1.toShort) /% ULong(1) :(ULong, ULong)
	UShort(1.toShort) ** UByte(1.toByte)
	UShort(1.toShort) ** UShort(1.toShort)
	UShort(1.toShort) ** UInt(1)
	UShort(1.toShort) ** 1.toByte
	UShort(1.toShort) ** 1.toShort
	UShort(1.toShort) ** 1
	UShort(1.toShort) %/ 1.toByte
	UShort(1.toShort) %/ UByte(1.toByte)
	UShort(1.toShort) %/ 1.toShort
	UShort(1.toShort) %/ UShort(1.toShort)
	UShort(1.toShort) %/ 1
	UShort(1.toShort) %/ UInt(1)
	UShort(1.toShort) %/ 1L
}



object UIntSpec {

	UInt(1) :ULong
	UInt(1) :Long
	UInt(1) + UByte(1.toByte) :UInt
	UInt(1) + 1.toByte :Long
	UInt(1) + UShort(1.toShort) :UInt
	UInt(1) + 1.toShort :Long
	UInt(1) + UInt(1) :UInt
	UInt(1) + 1 :Long
	UInt(1) + ULong(1) :ULong
	UInt(1) + 1L :Long
	UInt(1) /% UByte(1.toByte) :(UInt, UInt)
	UInt(1) /% 1.toByte :(Long, Long)
	UInt(1) /% UShort(1.toShort) :(UInt, UInt)
	UInt(1) /% 1.toShort :(Long, Long)
	UInt(1) /% UInt(1) :(UInt, UInt)
	UInt(1) /% 1 :(Long, Long)
	UInt(1) /% ULong(1) :(ULong, ULong)
	UInt(1) /% 1L :(Long, Long)
	UInt(1) ** UByte(1.toByte)
	UInt(1) ** UShort(1.toShort)
	UInt(1) ** UInt(1)
	UInt(1) ** 1.toByte
	UInt(1) ** 1.toShort
	UInt(1) ** 1
	UInt(1) %/ UByte(1.toByte)
	UInt(1) %/ 1.toByte
	UInt(1) %/ UShort(1.toShort)
	UInt(1) %/ 1.toShort
	UInt(1) %/ UInt(1)
	UInt(1) %/ 1
	//	UInt(1) %/ULong(1)
	UInt(1) %/ 1L
}



object ULongSpec {
	ULong.ULongUnsignedOps(ULong(1)) + UByte(1.toByte) :ULong
	ULong(1) + 1.toByte :ULong
	ULong(1) + UShort(1.toShort) :ULong
	ULong(1) + 1.toShort :ULong
	ULong(1) + UInt(1) :ULong
	ULong(1) + 1 :ULong
	ULong(1) + ULong(1) :ULong
	ULong(1) + 1L :ULong
	ULong(1) /% UByte(1.toByte) :(ULong, ULong)
	ULong(1) /% 1.toByte :(ULong, ULong)
	ULong(1) /% UShort(1.toShort) :(ULong, ULong)
	ULong(1) /% 1.toShort :(ULong, ULong)
	ULong(1) /% UInt(1) :(ULong, ULong)
	ULong(1) /% 1 :(ULong, ULong)
	ULong(1) /% ULong(1) :(ULong, ULong)
	ULong(1) /% 1L :(ULong, ULong)
	ULong(1) ** UByte(1.toByte)
	ULong(1) ** UShort(1.toShort)
	ULong(1) ** UInt(1)
	ULong(1) ** 1.toByte
	ULong(1) ** 1.toShort
	ULong(1) ** 1
	ULong(1) %/ UByte(1.toByte)
	ULong(1) %/ 1.toByte
	ULong(1) %/ UShort(1.toShort)
	ULong(1) %/ 1.toShort
	ULong(1) %/ UInt(1)
	ULong(1) %/ 1
	ULong(1) %/ 1L
}
