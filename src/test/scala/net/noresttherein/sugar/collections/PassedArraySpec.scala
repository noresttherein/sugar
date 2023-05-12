package net.noresttherein.sugar.collections

import org.scalacheck.{Arbitrary, Prop, Properties}
import scala.collection.{SeqFactory, mutable}
import scala.collection.immutable.ArraySeq
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.collections.IterableProps.{Filter, filter}
import net.noresttherein.sugar.extensions.{castTypeParamMethods, ClassExtension}
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension
import org.scalacheck.Prop.{AnyOperators, all, forAll}




object PassedArraySpec extends SeqProps[PassedArray]("PassedArray") {
//	override def referenceFactory = Seq
	override def checkedFactory :SeqFactory[PassedArray] = PassedArray

	//todo: tests of appends on non-owning instances
	//todo: tests of section, including appends
	//todo: tests of various builders
//	property(":++") = forAll { elems :List[List[Int]] =>
//		validate(elems.flatten, elems.foldLeft(PassedArray.empty[Int])(_ :++ _))
//	}
//	property("++:") = forAll { elems :List[List[Int]] =>
//		validate(elems.flatten, elems.foldRight(PassedArray.empty[Int])(_ ++: _))
//	}
	include(new ArrayTypeProps[Byte])
	include(new ArrayTypeProps[Short])
	include(new ArrayTypeProps[Char])
	include(new ArrayTypeProps[Int])
	include(new ArrayTypeProps[Long])
	include(new ArrayTypeProps[Float])
	include(new ArrayTypeProps[Double])


	private implicit class PassedArrayExtension[T](private val self :PassedArray[T]) extends AnyVal {
		def elemType :Class[T] = self.elementType.castParam[T]
	}

	class ArrayTypeProps[T :ClassTag :Arbitrary :Filter]
		extends Properties(s"(elementType=classOf[${classTag[T].runtimeClass.name}])")
	{
		private def get = Arbitrary.arbitrary[T].sample.get
		private val elem = classTag[T].runtimeClass.name
		private def elemClass = classTag[T].runtimeClass.castParam[T]

//		property(s"PassedArray.empty[$elem]") = PassedArray.empty[T].elemType =? elemClass
		property(s"PassedArray.one[$elem]")   = elemClass =? PassedArray.one(get).elemType
		property(s"PassedArray.two[$elem]")   = elemClass =? PassedArray.two(get, get).elemType
		//excluded because Scala creates an ArraySeq[Any] for var args.
//		property(s"PassedArray($elem*)")      = elemClass =? PassedArray(get, get, get, get, get).elemType
		property(s"PassedArray.from[$elem]")  = all(
			forAll { list :List[T] => Prop(list.isEmpty) || elemClass =? PassedArray.from(list).elemType } lbl "List",
			forAll { list :ArraySeq[T] => Prop(list.isEmpty) || elemClass =? PassedArray.from(list).elemType } lbl "ArraySeq",
			forAll { list :mutable.ArraySeq[T] => Prop(list.isEmpty) || elemClass =? PassedArray.from(list).elemType } lbl "ArraySeq"
		)
		//PassedArray.from(Seq(get, get, get, get)).elemType =? elemClass

		private def include(name :String, subject: => PassedArray[T]) :Unit = {
			property(s"$name.filter") = {
				val filtered = subject.filter(filter)
				filtered.isEmpty || elemClass =? filtered.elemType
			}

			property(s"($name :+ $elem)") = elemClass =? (subject :+ get).elemType
			property(s"($name :++ PassedArray($elem))") =
				elemClass =? (subject :++ PassedArray.one(get)).elemType
			property(s"($name :++ PassedArray($elem, $elem))") =
				elemClass =? (subject :++ PassedArray.two(get, get)).elemType
//			property(s"($name :++ PassedArray($elem*))") = //add an array fitting in the capacity
//				subject.elemType =? (subject :++ PassedArray(get, get, get, get)).elemType
			property(s"($name :++ PassedArray.from(List($elem*)))") = //add an array fitting in the capacity
				elemClass =? (subject :++ PassedArray.from(List(get, get, get, get))).elemType
			property(s"($name :++ ArraySeq.fill[$elem](128))") =
				elemClass =? (subject :++ ArraySeq.fill(128)(get)).elemType
			property(s"($name :++ ArraySeq[$elem])") =    //add an array requiring reallocation
				elemClass =? (subject :++ ArraySeq.from(List(get, get, get))).elemType
			property(s"($name :++ ArraySeq[Any]($elem*))") =
				subject.elemType.castParam[Any] =? (subject :++ ArraySeq.from[Any](List(get, get, get))).elemType

			property(s"($elem +: $name)") = elemClass =? (get +: subject).elemType
			property(s"(PassedArray($elem) ++: $name)") =
				elemClass =? (PassedArray.one(get) ++ subject).elemType
			property(s"(PassedArray($elem, $elem) ++: $name)") =
				elemClass =? (PassedArray.two(get, get) ++: subject).elemType
//			property(s"(PassedArray($elem*) ++: $name)") = //add an array fitting in the capacity
//				subject.elemType =? (PassedArray(get, get, get, get) ++: subject).elemType
			property(s"(PassedArray.from(List($elem*)) ++: $name)") = //add an array fitting in the capacity
				elemClass =? (PassedArray.from(List(get, get, get, get)) ++: subject).elemType
			property(s"(ArraySeq.fill[$elem](128) ++: $name)") = //add an array requiring reallocation
				elemClass =? (ArraySeq.fill(128)(get) ++: subject).elemType
			property(s"(ArraySeq[$elem] ++: $name)") =
				elemClass =? (ArraySeq.from(List(get, get, get)) ++: subject).elemType
			property(s"(ArraySeq[Any]($elem*) ++: $name)") =
				subject.elemType.castParam[Any] =? (ArraySeq.from[Any](List(get, get, get)) ++: subject).elemType

//			property(s"$name.toArray") =
//				elemClass =? subject.toArray.getClass.getComponentType.castParam[T]
		}

		//apply(E*) factory method gets passed
		include(s"PassedArray.empty[$elem]", PassedArray.empty[T])
		include("PassedArray.empty[Nothing]", PassedArray.empty[Nothing])
		include(s"PassedArray.one[$elem]", PassedArray.one(get))
		include(s"PassedArray.two[$elem]", PassedArray.two(get, get))
		include(s"PassedArray.from(List($elem*))", PassedArray.from(List(get, get, get, get)))
//		include(s"PassedArray.($elem*)", PassedArray(get, get, get, get))
	}


	//test concatenating instances without ownership flags
	property("chunks.scanLeft(empty)(_ :++ _)") = forAll { input :List[List[Int]] =>
		val expected = input.scanLeft(Vector.empty[Int])(_ :++ _).map(seq => seq ++: (seq :++ seq))
		val results  = input.scanLeft(PassedArray.empty[Int])(_ :++ _).map(seq => seq ++: (seq :++ seq))
		all(expected.zip(results).map { case (expect, result) => compare(expect, result) } :_*)
	}
	property("chunks.scanRight(empty)(_ ++: _)") = forAll { input :List[List[Int]] =>
		val expected = input.scanRight(Vector.empty[Int])(_ ++: _).map(seq => seq :++ (seq ++: seq))
		val results  = input.scanRight(PassedArray.empty[Int])(_ ++: _).map(seq => seq :++ (seq ++: seq))
		all(expected.zip(results).map { case (expect, result) => compare(expect, result) } :_*)
	}
}
