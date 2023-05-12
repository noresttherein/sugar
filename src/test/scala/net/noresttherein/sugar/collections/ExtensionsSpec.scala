package net.noresttherein.sugar.collections

import scala.collection.{AnyStepper, BuildFrom, DoubleStepper, Factory, IntStepper, LongStepper, Stepper}
import scala.collection.immutable.{ArraySeq, HashMap, HashSet, SortedMap, TreeMap}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.jdk.CollectionConverters.IteratorHasAsScala

import org.scalacheck.{Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter
import net.noresttherein.sugar.extensions.{immutableMapExtension, immutableSeqFactoryExtension, immutableSetFactoryExtension, ArrayExtension, ArrayObjectExtension, BuilderExtension, castTypeParamMethods, classNameMethods, ClassExtension, FactoryExtension, IndexedSeqExtension, IterableFactoryExtension, IterableOnceExtension, IteratorObjectExtension, JavaIteratorExtension, StepperExtension, StepperObjectExtension}
import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.??!
import net.noresttherein.sugar.testing.scalacheck.extensions._




object ExtensionsSpec extends Properties("extensions") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140))

	include(IndexedSeqProps)
	include(ArrayProps)
	include(MapProps)
	include(StepperProps)
	include(BuilderProps)
	include(FactoryProps)
	include(JavaIteratorProps)
	include(IterableFactoryProps)
	include(SetFactoryProps)
	include(SeqFactoryProps)
	include(ArrayObjectProps)
	include(IteratorFactoryProps)
	include(StepperFactoryProps)

//	import JavaIterator.conversions._
//	Console.err.println(List(1, 2).jiterator)
//	Console.err.println("JIntIterator ++ JIntIterator")
//	Console.err.println(List(1, 2).jiterator ++ List(3, 4).jiterator)
////	println("JIntIterator ++ JIterator[Int]")
////	println(List(1, 2).jiterator ++ (List(3, 4).jiterator :JIterator[Int]))
//	Console.err.println("JIterator[Int] ++ JIntIterator")
//	Console.err.println((List(1, 2).jiterator :JIterator[Int]) ++ List(3, 4).jiterator )
//	Console.err.println("JIterator[Int] ++ JIterator[Int]")
//	Console.err.println((List(1, 2).jiterator :JIterator[Int]) ++ (List(3, 4).jiterator :JIterator[Int]))


	object IndexedSeqProps extends Properties("IndexedSeqExtension") {
		property("takeRightWhile") = forAll { seq :IndexedSeq[Int] =>
			seq.takeRightWhile(_ % 5 < 4) ?= seq.reverseIterator.takeWhile(_ % 5 < 4).toIndexedSeq.reverse
		}
		property("dropRightWhile") = forAll { seq :IndexedSeq[Int] =>
			seq.dropRightWhile(_ % 5 < 4) ?= seq.reverseIterator.dropWhile(_ % 5 < 4).toIndexedSeq.reverse
		}
		property("binarySearch") = forAll { seq :IndexedSeq[Int] =>
			val sorted = seq.distinct.sorted

			all(sorted.indices.map { i => sorted.binarySearch(sorted(i)) ?= i } :_*) &&
				forAll { x :Int =>
					val i = sorted.binarySearch(x)
					sorted.indexOf(x) match {
						case -1 =>
							sorted.length == 0 && i == 0 ||
								i == sorted.length && sorted.last < x ||
								sorted(i) > x && (i == 0 || sorted(i - 1) < x) lbl "#" + i
						case n => i ?= n
					}
				} lbl seq.toString
		}
	}


	object ArrayProps extends Properties("ArrayExtension") {
		property("binarySearch") = forAll { array :Array[Int] =>
			val sorted = array.distinct.sorted
			all(
				(0 until sorted.length).map {
					i => sorted.binarySearch(sorted(i)) ?= i
				}
					:_*) && forAll { x :Int =>
				val i = sorted.binarySearch(x)
				sorted.indexOf(x) match {
					case -1 =>
						sorted.length == 0 && i == 0 ||
							i == sorted.length && sorted.last < x ||
							sorted(i) > x && (i == 0 || sorted(i - 1) < x) lbl "#" + i
					case n => i ?= n
				}
			} lbl ArrayAsSeq(array).toString
		}
	}


	object MapProps extends Properties("MapExtension") {
		property("updatedIfAbsent") = forAll { (map :Map[Int, String], key :Int, value :String) =>
			if (map.contains(key))
				(map =? map.updatedIfAbsent(key, value)) :| "present"
			else
				(map.updated(key, value) =? map.updatedIfAbsent(key, value)) :| "absent"
		}
		property("?=") = forAll { (map :Map[Int, String], key :Int, value :String) =>
			if (map.contains(key))
				(map =? (map ?= ((key, value)))) :| "present"
			else
				(map.updated(key, value) =? (map ?= ((key, value)))) :| "absent"
		}
	}


	object StepperProps extends Properties("StepperExtension") {
		property("++") =
			forAll { (prefix :String, suffix :String) =>
				(prefix.stepper ++ suffix.stepper).iterator
					.foldLeft(new StringBuilder)((sb, int) => sb += int.toChar).toString ?=
						prefix + suffix
			} &&
			forAll { (prefix :List[String], suffix :List[String]) =>
				(prefix.stepper ++ suffix.stepper).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Int]) ++ suffix.stepper).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(prefix.stepper ++ (suffix.stepper :Stepper[Int])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Int]) ++ (suffix.stepper :Stepper[Int])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Any]) ++ suffix.stepper).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(prefix.stepper ++ (suffix.stepper :Stepper[Any])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Any]) ++ (suffix.stepper :Stepper[Any])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix :List[Any]).stepper ++ (suffix.stepper :Stepper[Int])).iterator.toList ?= prefix:::suffix
			}
	}


	object BuilderProps extends Properties("BuilderExtension") {
		property("mapInput") = forAll { ints :List[Int] =>
			(IndexedSeq.newBuilder[Char].mapInput((_ :Int).toChar) ++= ints).result() ?= ints.toIndexedSeq.map(_.toChar)
		}
	}


	object FactoryProps extends Properties("FactoryExtension") {
		property("source") =
			(List.iterableFactory.source ?= List) && (Iterable.iterableFactory.source ?= Iterable) &&
				(Set.iterableFactory.source ?= Set) && (ArraySeq.evidenceIterableFactory[Int].source ?= ArraySeq) &&
				(HashMap.mapFactory.source ?= HashMap) && (Map.mapFactory.source ?= Map) &&
				(TreeMap.sortedMapFactory[Int, Int].source ?= TreeMap) &&
				(SortedMap.sortedMapFactory[Int, Int].source ?= SortedMap) && {
					val bf = implicitly[BuildFrom[List[Int], Int, List[Int]]]
					bf.toFactory(Nil).source ?= bf
				} && {
					new ComparableFactory[Int, List[Int]] {
						override def factory :Any = Set
						override def fromSpecific(it :IterableOnce[Int]) :List[Int] = ??!
						override def newBuilder :Builder[Int, List[Int]] = ??!
					}.source ?= Set
				} && {
					val factory = new Factory[Int, List[Int]] {
						override def fromSpecific(it :IterableOnce[Int]) :List[Int] = ??!
						override def newBuilder :Builder[Int, List[Int]] = ??!
					}
					factory.source ?= factory
				}
		property("iterableFactory") =
			("List" |: List.iterableFactory[Int].iterableFactory.contains(List)) &&
				("Set" |: Set.iterableFactory[Int].iterableFactory.contains(Set))

		property("evidenceIterableFactory") =
			ArraySeq.evidenceIterableFactory[Int].evidenceIterableFactory.contains(ArraySeq)

		property("mapFactory") =
			("Map" |: Map.mapFactory[Int, Int].mapFactory.contains(Map)) &&
				("HashMap" |: HashMap.mapFactory[Int, Int].mapFactory.contains(HashMap))

		property("sortedMapFactory") =
			("SortedMap" |: SortedMap.sortedMapFactory[Int, Int].sortedMapFactory.contains(SortedMap)) &&
				("TreeMap" |: TreeMap.sortedMapFactory[Int, Int].sortedMapFactory.contains(TreeMap))
	}


	object JavaIteratorProps extends Properties("JavaIteratorExtension") {
		import JavaIterator.conversions._
		property("++") =
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(prefix.jiterator ++ suffix.jiterator).asScala.toList ?= (prefix:::suffix).map(Integer.valueOf)
			}  :| "JIntIterator ++ JIntIterator" &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(((prefix.jiterator :JIterator[Int]) ++ suffix.jiterator).asScala.toSeq ?= prefix:::suffix)
			} :| "JIterator[Int] ++ JIntIterator" &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.jiterator :JIterator[Int]) ++ (suffix.jiterator :JIterator[Int])).asScala.toSeq ?= prefix:::suffix
			} :| "JIterator[Int] ++ JIterator[Int]"
	}


	object IterableFactoryProps extends Properties("IterableFactoryExtension") {
		property("generate") =
			List.generate(1) { case i if i < 1000 => i * 2 } ?= List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)

		property("expand") =
			Vector.expand(2) { i => Option(i * i).filter(_ < 1000) } ?= Vector(2, 4, 16, 256)

		property("iterateWithIndex") =
			Seq.iterateWithIndex(1, 10)((acc, i) => acc + i) ?= Seq(1, 2, 4, 7, 11, 16, 22, 29, 37, 46)
	}

	object SetFactoryProps extends Properties("SetFactoryExtension") {
		property("one")    = Set.one(42) ?= Set(42)
		property("single") = Set.single(42) ?= Set(42)
	}

	object SeqFactoryProps extends Properties("SeqFactoryExtension") {
		property("one")    = Vector.one(42) ?= Vector(42)
		property("single") = List.single(42)  ?= 42::Nil
	}


	object ArrayObjectProps extends Properties("ArrayObjectExtension") {
		property("generate") = {
			val res = Array.generate(1) { case i if i < 1000 => i * 2 }
			(res.getClass.castParam[Array[Int]] ?= classOf[Array[Int]]) &&
				(res.toList ?= List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024))

		}
		property("expand") = {
			val res = Array.expand(2L) { i => Option(i * i).filter(_ < 1000) }
			(res.getClass.castParam[Array[Long]] ?= classOf[Array[Long]]) && (res.toList ?= List(2, 4, 16, 256))
		}
		property("iterateWithIndex") = {
			val res = Array.iterateWithIndex("0", 10)((acc, i) => acc + "," + i)
			(res.getClass.castParam[Array[String]] == classOf[Array[String]]) &&
				(res.toSeq ?= Seq(
					"0", "0,1", "0,1,2", "0,1,2,3", "0,1,2,3,4", "0,1,2,3,4,5", "0,1,2,3,4,5,6", "0,1,2,3,4,5,6,7",
					"0,1,2,3,4,5,6,7,8", "0,1,2,3,4,5,6,7,8,9"
				))
		}
		property("unfold") = {
			val res = Array.unfold(1)(i => Some((i % 10, i * 2)).filter(_._2 < 1000))
			(res.getClass.castParam[Array[Int]] ?= classOf[Array[Int]]) && (res.toSeq ?=
				Seq(1, 2, 4, 8, 6, 2, 4, 8, 6)
			)
		}
	}


	object IteratorFactoryProps extends Properties("IteratorFactoryExtension") {
		property("double") = Iterator.double(1, 2).toSeq ?= Seq(1, 2)
	}


	object StepperFactoryProps extends Properties("StepperFactoryExtension") {
		private def stepperProperty[A, S <: Stepper[A]](name :String, stepper: => S, expect :Seq[A], stepperClass :Class[_]) :Prop =
			(Prop(stepperClass.isInstance(stepper)) :|
				"(" + name + ": " + stepper.className + ").isInstanceOf[" + stepperClass.name + "]"
			) &&
				((stepper.iterator.toSeq ?= expect) :| name + ".iterator.toSeq") &&
				(((stepper.javaIterator.asScala.toSeq :Seq[Any]) ?= expect) :| name + ".javaIterator.asScala.toSeq") &&
				((stepper.spliterator.estimateSize ?= expect.length) :| name + ".spliterator.estimateSize") && {
					val buf = new ArrayBuffer[A]
					val s = stepper
					while (s.hasStep)
						buf += s.nextStep()
					((buf :collection.Seq[A]) ?= expect) :| name + ".hasStep/nextStep()"
				} && {
					val buf = new ArrayBuffer[Any]
					stepper.spliterator.forEachRemaining(buf += (_ :Any))
					((buf :collection.Seq[Any]) ?= expect) :| name + ".spliterator.forEachRemaining"
				} && {
					val s1 = stepper.spliterator
					val s2 = s1.trySplit
					val buf = new ArrayBuffer[Any]
					if (s2 != null)
						while (s2.tryAdvance(buf += (_ :Any)))
							{}
					while (s1.tryAdvance(buf += (_ :Any)))
						{}
					((buf :collection.Seq[Any]) ?= expect) :| name + ".spliterator.trySplit"
				} lbl "slice: " + expect.toString lbl "stepper: " + stepper

//		property("empty") =
//			stepperProperty("Stepper.empty[Byte]", Stepper.empty )
//			(Prop(Stepper.empty[Byte].isInstanceOf[IntStepper]) :| "empty[Byte].isInstanceOf[IntStepper]") &&
//			(Prop(Stepper.empty[Short].isInstanceOf[IntStepper]) :| "empty[Short].isInstanceOf[IntStepper]") &&
//			(Prop(Stepper.empty[Char].isInstanceOf[IntStepper]) :| "empty[Char].isInstanceOf[IntStepper]") &&
//			(Prop(Stepper.empty[Int].isInstanceOf[IntStepper]) :| "empty[Int].isInstanceOf[IntStepper]") &&
//			(Prop(Stepper.empty[Long].isInstanceOf[LongStepper]) :| "empty[Long].isInstanceOf[IntStepper]") &&
//			(Prop(Stepper.empty[Float].isInstanceOf[DoubleStepper]) :| "empty[Float].isInstanceOf[IntStepper]") &&
//			(Prop(Stepper.empty[Double].isInstanceOf[DoubleStepper]) :| "empty[Double].isInstanceOf[DoubleStepper]") &&
//			(Prop(Stepper.empty[String].isInstanceOf[AnyStepper[_]]) :| "empty[String].isInstanceOf[AnyStepper[_]]") &&
//			(Prop(!Stepper.empty[String].hasStep) :| "!empty[String].hasStep")

		property("apply()") =
			stepperProperty("Stepper[Byte]()", Stepper[Byte](), Seq[Int](), classOf[IntStepper]) &&
			stepperProperty("Stepper[Short]()", Stepper[Short](), Seq[Int](), classOf[IntStepper]) &&
			stepperProperty("Stepper[Char]()", Stepper[Char](), Seq[Int](), classOf[IntStepper]) &&
			stepperProperty("Stepper[Int]()", Stepper[Int](), Seq[Int](), classOf[IntStepper]) &&
			stepperProperty("Stepper[Long]()", Stepper[Long](), Seq[Long](), classOf[LongStepper]) &&
			stepperProperty("Stepper[Float]()", Stepper[Float](), Seq[Double](), classOf[DoubleStepper]) &&
			stepperProperty("Stepper[Double]()", Stepper[Double](), Seq[Double](), classOf[DoubleStepper]) &&
			stepperProperty("Stepper[String]()", Stepper[String](), Seq[String](), classOf[AnyStepper[_]])
		
		property("apply(elem:T)") =
			stepperProperty("Stepper(1.toByte)", Stepper.apply(1.toByte), Seq(1), classOf[IntStepper]) &&
			stepperProperty("Stepper(1.toShort)", Stepper(1.toShort), Seq(1), classOf[IntStepper]) &&
			stepperProperty("Stepper('1')", Stepper('1'), Seq('1'.toInt), classOf[IntStepper]) &&
			stepperProperty("Stepper(1)", Stepper(1), Seq(1), classOf[IntStepper]) &&
			stepperProperty("Stepper(1L)", Stepper(1L), Seq(1L), classOf[LongStepper]) &&
			stepperProperty("Stepper(1.0f)", Stepper(1.0f), Seq(1.0), classOf[DoubleStepper]) &&
			stepperProperty("Stepper(1.0)", Stepper(1.0), Seq(1.0), classOf[DoubleStepper]) &&
			stepperProperty("Stepper(\"1\")", Stepper("1"), Seq("1"), classOf[AnyStepper[_]])

		property("apply(first:T,second:T)") =
			stepperProperty("Stepper(1.toByte, 2.toByte)", Stepper(1.toByte, 2.toByte), Seq(1, 2), classOf[IntStepper]) &&
			stepperProperty("Stepper(1.toShort, 2.toShort)", Stepper(1.toShort, 2.toShort), Seq(1, 2), classOf[IntStepper]) &&
			stepperProperty("Stepper('1', '2')", Stepper('1', '2'), Seq('1'.toInt, '2'.toInt), classOf[IntStepper]) &&
			stepperProperty("Stepper(1, 2)", Stepper(1, 2), Seq(1, 2), classOf[IntStepper]) &&
			stepperProperty("Stepper(1L, 2L)", Stepper(1L, 2L), Seq(1L, 2L), classOf[LongStepper]) &&
			stepperProperty("Stepper(1.0f, 2.0f)", Stepper(1.0f, 2.0f), Seq(1.0, 2.0), classOf[DoubleStepper]) &&
			stepperProperty("Stepper(1.0, 2.0)", Stepper(1.0, 2.0), Seq(1.0, 2.0), classOf[DoubleStepper]) 

		property("apply(array:Array[T],from:Int,until:Int)") = all(
			forAll { (array :Array[Byte], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Byte]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toInt), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Short], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Short]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toInt), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Char], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Char]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toInt), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Int], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Int]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Long], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Long]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[LongStepper]
				)
			},
			forAll { (array :Array[Float], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Float]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toDouble), classOf[DoubleStepper]
				)
			},
			forAll { (array :Array[Double], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[Double]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[DoubleStepper]
				)
			},
			forAll { (array :Array[String], from :Int, until :Int) =>
				stepperProperty("Stepper(Array[String]," + from + "," + until +")", Stepper(array, from, until),
					ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[AnyStepper[String]]
				)
			},
		)

		property("ofAny[String]") =
			stepperProperty("", Stepper.ofAny[String], Seq[String](), classOf[AnyStepper[_]])
		property("ofAny[String](\"1\")") = 
			stepperProperty("", Stepper.ofAny("1"), Seq("1"), classOf[AnyStepper[_]])
		property("ofAny[String](\"1\", \"2\")") =
			stepperProperty("", Stepper.ofAny("1", "2"), Seq("1", "2"), classOf[AnyStepper[_]])

		property("ofInt") =
			stepperProperty("", Stepper.ofInt, Seq[Int](), classOf[IntStepper])
		property("ofInt(1)") = 
			stepperProperty("", Stepper.ofInt(1), Seq(1), classOf[IntStepper])
		property("ofInt(1, 2)") =
			stepperProperty("", Stepper.ofInt(1, 2), Seq(1, 2), classOf[IntStepper])

		property("ofLong") =
			stepperProperty("", Stepper.ofLong, Seq[Long](), classOf[LongStepper])
		property("ofLong(1)") = 
			stepperProperty("", Stepper.ofLong(1), Seq(1L), classOf[LongStepper])
		property("ofLong(1, 2)") =
			stepperProperty("", Stepper.ofLong(1, 2), Seq(1L, 2L), classOf[LongStepper])

		property("ofDouble") =
			stepperProperty("", Stepper.ofDouble, Seq[Double](), classOf[DoubleStepper])
		property("ofDouble(1.0)") =
			stepperProperty("", Stepper.ofDouble(1.0), Seq(1.0), classOf[DoubleStepper])
		property("ofDouble(1.0, 2.0)") =
			stepperProperty("", Stepper.ofDouble(1.0, 2.0), Seq(1.0, 2.0), classOf[DoubleStepper])
		
	}



}
