package net.noresttherein.sugar.collections

import scala.collection.SeqFactory
import scala.collection.mutable.{Buffer, Builder}

import net.noresttherein.sugar.collections.IterableProps.Dummy
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.util.Buildable
import org.scalacheck.{Properties, Test}




private object AliasingArrayBufferSpec
	extends Properties("AliasingArrayBuffer") with BufferProps[AliasingArrayBuffer, Dummy]
{
	override val parameters = overrideParameters(Test.Parameters.default)
	val Buff = AliasingArrayBuffer

	implicit override def buildableChecked[T :Dummy] :Buildable[T, Buff[T]] = new Buildable[T, Buff[T]] {
		override def builder :Builder[T, Buff[T]] = Buff.newBuilder[T]
	}
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

	val coll = Seq(11, 1, 1981)

	property("mutations") = new BufferCommands[Int](Buff.from(_ :Seq[Int])).property()

	property("toSeq") =
		("update" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			val expectBuffer =
				if (buffer.length > 1) {
					buffer(1) = 42
					expect.take(1) :+ 42 :++ expect.drop(2)
				} else if (buffer.length > 0) {
					 buffer(0) = 42
					42 +: expect.tail
				} else
					expect
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expectBuffer =? buffer))
		}) &&
		("remove(Int)" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			val expectBuffer =
				if (buffer.length > 1) {
					buffer.remove(1)
					expect.take(1) :++ expect.drop(2)
				} else if (buffer.length > 0) {
					buffer.remove(0)
					expect.tail
				} else
					expect
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expectBuffer =? buffer))
		}) &&
		("remove(Int, Int)" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			val expectBuffer =
				if (buffer.length > 1) {
					buffer.remove(1, buffer.length - 2)
					expect.take(1) :++ expect.slice(expect.length - 1, expect.length)
				} else if (buffer.length > 0) {
					buffer.remove(0, 1)
					expect.tail
				} else {
					buffer.remove(0, 0)
					expect
				}
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expectBuffer =? buffer))
		}) &&
		("add" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			buffer += 42
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expect :+ 42 =? buffer))
		}) &&
		("prepend" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			42 +=: buffer
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (42 +: expect =? buffer))
		}) &&
		("insert" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			val expectBuffer =
				if (buffer.length > 1) {
					buffer.insert(1, 42)
					expect.take(1) :+ 42 :++ expect.drop(1)
				} else {
					buffer.insert(0, 42)
					42 +: expect
				}
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expectBuffer =? buffer))
		}) &&
		("addAll" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			buffer ++= coll
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expect :++ coll =? buffer))
		}) &&
		("prependAll" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			coll ++=: buffer
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (coll ++: expect =? buffer))
		}) &&
		("insertAll" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			val expectBuffer =
				if (buffer.length > 1) {
					buffer.insertAll(1, coll)
					expect.take(1) :++ coll :++ expect.drop(1)
				} else {
					buffer.insertAll(0, coll)
					coll ++: expect
				}
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq)) && ("result" |: (expectBuffer =? buffer))
		}) &&
		("clear" |: forAll { buffer :AliasingArrayBuffer[Int] =>
			val expect = buffer.iterator.toVector :collection.Seq[Int]
			val seq    = buffer.toSeq
			buffer.clear()
			expect.mkString("Buffer(", ", ", ")") lbl_:
				("unmodified" |: (expect =? seq) && ("result" |: collection.Seq[Int]() =? buffer))
		})
}


//grant access to Playground
object AliasingArrayBufferFactory extends SeqFactory.Delegate[Buffer](AliasingArrayBuffer)
