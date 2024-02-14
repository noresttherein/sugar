package net.noresttherein.sugar.collections

import org.scalacheck.{Arbitrary, Prop, Properties}
import scala.collection.{SeqFactory, mutable}
import scala.collection.immutable.ArraySeq
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, filter}
import net.noresttherein.sugar.extensions.{ClassExtension, castTypeParamMethods, classNameMethods}
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension
import org.scalacheck.Prop.{AnyOperators, all, forAll}




object RelayArraySpec
	extends UntaggedSeqProps[RelayArray]("RelayArray", RelayArray) with SugaredSeqProps[RelayArray, Dummy]
{
	//todo: tests of section, including appends
	//todo: tests of various builders
	include(new ArrayTypeProps[Byte])
	include(new ArrayTypeProps[Short])
	include(new ArrayTypeProps[Char])
	include(new ArrayTypeProps[Int])
	include(new ArrayTypeProps[Long])
	include(new ArrayTypeProps[Float])
	include(new ArrayTypeProps[Double])


	private implicit class RelayArrayExtension[T](private val self :RelayArray[T]) extends AnyVal {
		def elemType :Class[T] = self.elementType.castParam[T]
	}

	/** Properties verifying that various operations on an `RelayArray` create/preserve a specific element type
	  * of the underlying array.
	  */
	class ArrayTypeProps[T :ClassTag :Arbitrary :Filter]
		extends Properties(s"[${classTag[T].runtimeClass.name}]")
	{
		private def get :T = Arbitrary.arbitrary[T].sample.get
		private val elem = classTag[T].runtimeClass.name
		private def elemClass = classTag[T].runtimeClass.castParam[T]

		property(s"RelayArray.one[$elem]")   = elemClass =? RelayArray.one(get).elemType
		property(s"RelayArray.two[$elem]")   = elemClass =? RelayArray.two(get, get).elemType
		//excluded because Scala creates an ArraySeq[Any] for var args.
//		property(s"RelayArray($elem*)")      = elemClass =? RelayArray(get, get, get, get, get).elemType
		property(s"RelayArray.from[$elem]")  = all(
			forAll { list :List[T] => Prop(list.isEmpty) || elemClass =? RelayArray.from(list).elemType } lbl "List",
			//Excluded because, currently, anything backed by an Array[Any] creates a RelayArray using Array[Any].
//			forAll { list :Vector[T] => Prop(list.isEmpty) || elemClass =? RelayArray.from(list).elemType } lbl "Vector",
			forAll { list :ArraySeq[T] => Prop(list.isEmpty) || elemClass =? RelayArray.from(list).elemType } lbl "ArraySeq",
			forAll { list :mutable.ArraySeq[T] => Prop(list.isEmpty) || elemClass =? RelayArray.from(list).elemType } lbl "ArraySeq"
		)

		private def include(name :String, subject: => RelayArray[T]) :Unit = {
			property(s"$name.filter") = {
				val filtered = subject.filter(filter)
				filtered.isEmpty || elemClass =? filtered.elemType
			}

			property(s"$name :+ $elem") = elemClass =? (subject :+ get).elemType
			property(s"$name :++ RelayArray($elem)") =
				elemClass =? (subject :++ RelayArray.one(get)).elemType
			property(s"$name :++ RelayArray($elem, $elem)") =
				elemClass =? (subject :++ RelayArray.two(get, get)).elemType
			property(s"$name :++ RelayArray.from(List($elem*))") = //add an array fitting in the capacity
				elemClass =? (subject :++ RelayArray.from(List(get, get, get, get))).elemType
			property(s"$name :++ ArraySeq.fill[$elem](128)") =
				elemClass =? (subject :++ ArraySeq.fill(128)(get)).elemType
			property(s"$name :++ ArraySeq[$elem]") =    //add an array requiring reallocation
				elemClass =? (subject :++ ArraySeq.unsafeWrapArray(Array(get, get, get))).elemType
			property(s"$name :++ ArraySeq[Any]($elem*)") =
				subject.elemType.castParam[Any] =? (subject :++ ArraySeq.from[Any](List(get, get, get))).elemType

			property(s"$elem +: $name)") = elemClass =? (get +: subject).elemType
			property(s"RelayArray($elem) ++: $name") =
				elemClass =? (RelayArray.one(get) ++ subject).elemType
			property(s"RelayArray($elem, $elem) ++: $name") =
				elemClass =? (RelayArray.two(get, get) ++: subject).elemType
			property(s"RelayArray.from(List($elem*)) ++: $name") = //add an array fitting in the capacity
				elemClass =? (RelayArray.from(List(get, get, get, get)) ++: subject).elemType
			property(s"ArraySeq.fill[$elem](128) ++: $name") = //add an array requiring reallocation
				elemClass =? (ArraySeq.fill(128)(get) ++: subject).elemType
			property(s"ArraySeq[$elem] ++: $name") =
				elemClass =? (ArraySeq.from(List(get, get, get)) ++: subject).elemType
			property(s"ArraySeq[Any]($elem*) ++: $name") =
				subject.elemType.castParam[Any] =? (ArraySeq.from[Any](List(get, get, get)) ++: subject).elemType
		}

		//apply(E*) factory method gets passed
		include(s"RelayArray.empty[$elem]", RelayArray.empty[T])
		include("RelayArray.empty[Nothing]", RelayArray.empty[Nothing])
		include(s"RelayArray.one[$elem]", RelayArray.one(get))
		include(s"RelayArray.two[$elem]", RelayArray.two(get, get))
		include(s"RelayArray.from(List($elem*))", RelayArray.from(List(get, get, get, get)))
//		include(s"RelayArray.apply($elem*)", RelayArray(get, get, get, get))
	}


	//test concatenating instances without ownership flags
	property("scanLeft(empty)(_ :++ _)") = forAll { input :List[List[Int]] =>
		val expected = input.scanLeft(Vector.empty[Int])(_ :++ _).map(seq => seq ++: (seq :++ seq))
		val results  = input.scanLeft(RelayArray.empty[Int])(_ :++ _).map(seq => seq ++: (seq :++ seq))
		all(expected.zip(results).map { case (expect, result) => compare(expect, result) } :_*)
	}
	property("scanRight(empty)(_ ++: _)") = forAll { input :List[List[Int]] =>
		val expected = input.scanRight(Vector.empty[Int])(_ ++: _).map(seq => seq :++ (seq ++: seq))
		val results  = input.scanRight(RelayArray.empty[Int])(_ ++: _).map(seq => seq :++ (seq ++: seq))
		all(expected.zip(results).map { case (expect, result) => compare(expect, result) } :_*)
	}

}
