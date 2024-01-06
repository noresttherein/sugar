package net.noresttherein.sugar.collections

import scala.annotation.nowarn
import scala.collection.{AnyStepper, BuildFrom, DoubleStepper, Factory, IntStepper, LongStepper, SeqFactory, Stepper, mutable}
import scala.collection.immutable.{ArraySeq, HashMap, HashSet, LinearSeq, SortedMap, TreeMap}
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder, IndexedBuffer, ListBuffer}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.{ClassTag, classTag}

import org.scalacheck.{Arbitrary, Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter
import net.noresttherein.sugar.extensions.{ArrayExtension, ArrayCompanionExtension, BuilderExtension, ClassExtension, FactoryExtension, IndexedSeqExtension, IterableFactoryExtension, IterableOnceExtension, IteratorCompanionExtension, JavaIteratorExtension, JavaStringBuilderExtension, SeqFactoryExtension, StepperExtension, StepperCompanionExtension, castTypeParamMethods, classNameMethods, immutableIndexedSeqCompanionExtension, immutableMapExtension, immutableSetFactoryExtension}
import net.noresttherein.sugar.JavaTypes.{JIterator, JStringBuilder}
import net.noresttherein.sugar.??!
import net.noresttherein.sugar.testing.scalacheck.extensions._




object ExtensionsSpec extends Properties("extensions") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140))

	include(MapProps)
	include(StepperProps)
	include(BuilderProps)
	include(JavaStringBuilderProps)
	include(FactoryProps)
	include(JavaIteratorProps)
	include(IterableFactoryProps)
	include(SetFactoryProps)
	include(SeqFactoryProps)
	include(InfiniteSeqProps)
	include(StepperObjectProps)



	object MapProps extends Properties("MapExtension") {
		property("getOrElseApply") = forAll { (map :Map[Int, String], key :Int, value :String) =>
			map.getOrElseApply(key, (_ :Int).toString) ?= map.applyOrElse(key, (_ :Int).toString)
		}
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
			forAll { strings :Seq[String] =>
				val concat = (Stepper[Char]() /: strings)(_ ++ _.stepper)
				val result = concat.iterator.foldLeft(new StringBuilder)((sb, int) => sb += int.toChar).toString
				result ?= strings.foldLeft(new StringBuilder)((sb, string) => sb ++= string).toString
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

		property(":++") =
			forAll { strings :Seq[String] =>
				val concat = (Stepper[Char]() /: strings)(_ :++ _.stepper)
				val result = concat.iterator.foldLeft(new StringBuilder)((sb, int) => sb += int.toChar).toString
				result ?= strings.foldLeft(new StringBuilder)((sb, string) => sb ++= string).toString
			} &&
			forAll { (prefix :List[String], suffix :List[String]) =>
				(prefix.stepper :++ suffix.stepper).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Int]) :++ suffix.stepper).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(prefix.stepper :++ (suffix.stepper :Stepper[Int])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Int]) :++ (suffix.stepper :Stepper[Int])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Any]) :++ suffix.stepper).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(prefix.stepper :++ (suffix.stepper :Stepper[Any])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.stepper :Stepper[Any]) :++ (suffix.stepper :Stepper[Any])).iterator.toList ?= prefix:::suffix
			} &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix :List[Any]).stepper :++ (suffix.stepper :Stepper[Int])).iterator.toList ?= prefix:::suffix
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


	object BuilderProps extends Properties("BuilderExtension") {
		property("mapInput") = forAll { ints :List[Int] =>
			(IndexedSeq.newBuilder[Char].mapInput((_ :Int).toChar) ++= ints).result() ?= ints.toIndexedSeq.map(_.toChar)
		}
	}


	object JavaStringBuilderProps extends Properties("JavaStringBuilderExtension") {
		property("apply") = forAll { s :String =>
			all(s.indices.map(i => new StringBuilder(s).apply(i) =? s.charAt(i)) :_*)
		}
		property("length") = forAll { s :String => new StringBuilder(s).length ?= s.length }
		property("length_=") = {
			val colours = new StringBuilder("red, black, blue")
			colours.length = "red, black".length
			colours.result() ?= "red, black"
		}
		property("+=(Char)") = (new JStringBuilder("hello, ") += 'X').result() ?= "hello, X"
		property("++=(String)") = (new JStringBuilder("hello, ") ++= "stranger").result() ?= "hello, stranger"
		property("++=(Seq)") =
			(new JStringBuilder("hello, ") ++= Seq('p', 'r', 'o', 'f', 'e', 's', 's', 'o', 'r')).result() ?=
				"hello, professor"
		property("++=(CharSequence)") =
			(new JStringBuilder("hello, ") ++= ("stranger" :CharSequence)).result() ?= "hello, stranger"

		property("iterator") = forAll { s :String => new StringBuilder(s).iterator.toSeq ?= s.toSeq }
	}


	object JavaIteratorProps extends Properties("JavaIteratorExtension") {
		import JavaIterator.conversions._
		property("++") =
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(prefix.javaIterator ++ suffix.javaIterator).asScala.toList ?= (prefix:::suffix).map(Integer.valueOf)
			}  :| "JIntIterator ++ JIntIterator" &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				(((prefix.javaIterator :JIterator[Int]) ++ suffix.javaIterator).asScala.toSeq ?= prefix:::suffix)
			} :| "JIterator[Int] ++ JIntIterator" &&
			forAll { (prefix :List[Int], suffix :List[Int]) =>
				((prefix.javaIterator :JIterator[Int]) ++ (suffix.javaIterator :JIterator[Int])).asScala.toSeq ?= prefix:::suffix
			} :| "JIterator[Int] ++ JIterator[Int]"
	}



	@nowarn("cat=deprecation")
	object IterableFactoryProps extends Properties("IterableFactoryExtension") {
		property("generate") = {
			val expect = List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
			(List.generate(1) { case i if i < 1000 => i * 2 } ?= expect) :| "List" && {
				var x = 0
				val lzy = LazyList.generate(1) { case i if i < 1000 => x += 1; i * 2 }
				(x ?= 0) && ((lzy :Seq[Int]) ?= expect) :| "LazyList"
			} && {
				var x = 0
				val lzy = Stream.generate(1) { case i if i < 1000 => x += 1; i * 2 }
				(x ?= 0) && ((lzy :Seq[Int]) ?= expect) :| "Stream"
			}
		}
		property("expand") = {
			val expect = Vector(2, 4, 16, 256)
			(Vector.expand(2) { i => Option(i * i).filter(_ < 1000) } ?= expect) :| "Vector" && {
				var x = 0
				val lzy :Seq[Int] = LazyList.expand(2) { case i => x += 1; Option(i * i).filter(_ < 1000) }
				(x ?= 0) && (lzy ?= expect) :| "LazyList"
			} && {
				var x = 0
				val lzy :Seq[Int] = Stream.expand(2) { case i => x += 1; Option(i * i).filter(_ < 1000) }
				(x ?= 0) && (lzy ?= expect) :| "Stream"
			}
		}
		property("iterateWithIndex") =
			Seq.iterateWithIndex(1, 10)((acc, i) => acc + i) ?= Seq(1, 2, 4, 7, 11, 16, 22, 29, 37, 46)
	}


	object SetFactoryProps extends Properties("SetFactoryExtension") {
		property("one")    = Set.one(42) ?= Set(42)
		property("single") = Set.single(42) ?= Set(42)
	}


	object SeqFactoryProps extends Properties("SeqFactoryExtension") {
		property("one")     =
			((Vector.one(42) ?= Vector(42)) lbl "Vector") &&
				((List.one(42) ?= 42 :: Nil) lbl "List") &&
				((Seq.one(42) ?= Seq(42)) lbl "Seq") &&
				((mutable.Seq.one(42) ?= mutable.Seq(42)) lbl "mutable.Seq") &&
				((mutable.IndexedSeq.one(42) ?= mutable.IndexedSeq(42)) lbl "mutable.IndexedSeq") &&
				((mutable.Buffer.one(42) ?= mutable.Buffer(42)) lbl "Buffer") &&
				((ArrayBuffer.one(42) ?= ArrayBuffer(42)) lbl "ArrayBuffer") &&
				((ListBuffer.one(42) ?= ListBuffer(42)) lbl "ListBuffer")

		property("single")  =
			((Vector.single(42) ?= Vector(42)) lbl "Vector") &&
				((List.single(42) ?= 42 :: Nil) lbl "List") &&
				((Seq.single(42) ?= Seq(42)) lbl "Seq") &&
				((mutable.Seq.single(42) ?= mutable.Seq(42)) lbl "mutable.Seq") &&
				((mutable.IndexedSeq.single(42) ?= mutable.IndexedSeq(42)) lbl "mutable.IndexedSeq") &&
				((mutable.Buffer.single(42) ?= mutable.Buffer(42)) lbl "Buffer") &&
				((ArrayBuffer.single(42) ?= ArrayBuffer(42)) lbl "ArrayBuffer") &&
				((ListBuffer.single(42) ?= ListBuffer(42)) lbl "ListBuffer")

		property("two") = {
			def prop[C[A] <: collection.Seq[A] with collection.SeqOps[A, C, C[A]]](factory :SeqFactory[C]) = {
				val two = factory.two("Tom", "Jerry") :collection.Seq[String]
				val expect = factory("Tom", "Jerry") :collection.Seq[String]
				seqProps(two, expect)
			} :| factory.localClassName
			prop(Seq) && prop(IndexedSeq) && prop(LinearSeq) && prop(List) && prop(Vector) &&
				prop(mutable.Seq) && prop(Buffer) && prop(mutable.IndexedSeq) && prop(IndexedBuffer) &&
				prop(ArrayBuffer) && prop(ListBuffer)
		}

		property("const") = {
			def prop[C[A] <: collection.Seq[A] with collection.SeqOps[A, C, C[A]]](factory :SeqFactory[C]) =
				forAll { (value :Int, length :Byte) =>
					seqProps(factory.const(length)(value), factory.fill(length)(value))
				} :| factory.localClassName
			prop(Seq) && prop(IndexedSeq) && prop(LinearSeq) && prop(List) && prop(Vector)
		}

		private def seqProps[A](seq :collection.Seq[A], expect :collection.Seq[A]) :Prop =
			(seq ?= expect) &&
				((seq.iterator.to(collection.Seq)) ?= expect) :| "iterator" &&
				forAll((n :Int) => seq.drop(n) ?= expect.drop(n)) :| "drop" &&
				forAll((n :Int) => seq.take(n) ?= expect.take(n)) :| "take" &&
				forAll { (from :Int, until :Int) =>
					seq.slice(from, until) ?= expect.slice(from, until)
				} :| "slice" && (
				if (expect.length == 0)
					seq.head.throws[NoSuchElementException] :| "head" &&
						seq.last.throws[NoSuchElementException] :| "last" &&
						seq.tail.throws[UnsupportedOperationException] :| "tail" &&
						seq.init.throws[UnsupportedOperationException] :| "init"
				else
					(seq.tail ?= expect.tail) :| "tail" &&
						(seq.head ?= expect.head) :| "head" &&
						(seq.last ?= expect.last) :| "last" &&
						(seq.init ?= expect.init) :| "init"
				)
		}


	object InfiniteSeqProps extends Properties("IndexedSeq.infinite") {
		val infinite = IndexedSeq.infinite(42)
		property("knownSize") = infinite.knownSize < 0
		property("apply") = forAll { i :Int =>
			if (i < 0) infinite(i).throws[IndexOutOfBoundsException]
			else infinite(i) ?= 42
		}
		property("head") = infinite.head ?= 42
		property("tail") = infinite.tail eq infinite
		property("drop") = forAll { n :Int => infinite.drop(n) eq infinite }
		property("take") = forAll { n :Byte => infinite.take(n) ?= IndexedSeq.fill(n)(42) }
		property("slice") = forAll { (from :Byte, until :Byte) =>
			val from0 = math.max(from, 0)
			val until0 = math.max(until, from0)
			infinite.slice(from, until) ?= IndexedSeq.fill(until0 - from0)(42)
		}
	}


	object StepperObjectProps extends Properties("StepperCompanionExtension") {
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

		private def spliteratorProperty[A :Arbitrary, S <: Stepper[A] :ClassTag](stepper :Seq[A] => S) :Prop =
			forAll { expect :Vector[A] => stepperProperty("", stepper(expect), expect, classTag[S].runtimeClass) }

		property("apply(Spliterator[String])") =
			spliteratorProperty[String, AnyStepper[String]]((expect :Seq[String]) => Stepper(expect.stepper.spliterator))
		property("apply(Spliterator.OfInt)") =
			spliteratorProperty[Int, IntStepper]((expect :Seq[Int]) => Stepper(expect.stepper.spliterator))
		property("apply(Spliterator.OfLong)") =
			spliteratorProperty[Long, LongStepper]((expect :Seq[Long]) => Stepper(expect.stepper.spliterator))
		property("apply(Spliterator.OfDouble)") =
			spliteratorProperty[Double, DoubleStepper]((expect :Seq[Double]) => Stepper(expect.stepper.spliterator))

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
		
		property("one") =
			stepperProperty("Stepper.one(1.toByte)", Stepper.one(1.toByte), Seq(1), classOf[IntStepper]) &&
			stepperProperty("Stepper.one(1.toShort)", Stepper.one(1.toShort), Seq(1), classOf[IntStepper]) &&
			stepperProperty("Stepper.one('1')", Stepper.one('1'), Seq('1'.toInt), classOf[IntStepper]) &&
			stepperProperty("Stepper.one(1)", Stepper.one(1), Seq(1), classOf[IntStepper]) &&
			stepperProperty("Stepper.one(1L)", Stepper.one(1L), Seq(1L), classOf[LongStepper]) &&
			stepperProperty("Stepper.one(1.0f)", Stepper.one(1.0f), Seq(1.0), classOf[DoubleStepper]) &&
			stepperProperty("Stepper.one(1.0)", Stepper.one(1.0), Seq(1.0), classOf[DoubleStepper]) &&
			stepperProperty("Stepper.one(\"1\")", Stepper.one("1"), Seq("1"), classOf[AnyStepper[_]])

		property("two") =
			stepperProperty(
				"Stepper.two(1.toByte, 2.toByte)", Stepper.two(1.toByte, 2.toByte), Seq(1, 2), classOf[IntStepper]
			) && stepperProperty(
				"Stepper.two(1.toShort, 2.toShort)", Stepper.two(1.toShort, 2.toShort), Seq(1, 2), classOf[IntStepper]
			) && stepperProperty(
				"Stepper.two('1', '2')", Stepper.two('1', '2'), Seq('1'.toInt, '2'.toInt), classOf[IntStepper]
			) && stepperProperty(
				"Stepper.two(1, 2)", Stepper.two(1, 2), Seq(1, 2), classOf[IntStepper]
			) && stepperProperty(
				"Stepper.two(1L, 2L)", Stepper.two(1L, 2L), Seq(1L, 2L), classOf[LongStepper]
			) && stepperProperty(
				"Stepper.two(1.0f, 2.0f)", Stepper.two(1.0f, 2.0f), Seq(1.0, 2.0), classOf[DoubleStepper]
			) && stepperProperty(
				"Stepper.two(1.0, 2.0)", Stepper.two(1.0, 2.0), Seq(1.0, 2.0), classOf[DoubleStepper]
			)

		property("over(array:Array[T],from:Int,until:Int)") = all(
			forAll { (array :Array[Byte], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Byte]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toInt), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Short], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Short]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toInt), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Char], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Char]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toInt), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Int], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Int]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[IntStepper]
				)
			},
			forAll { (array :Array[Long], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Long]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[LongStepper]
				)
			},
			forAll { (array :Array[Float], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Float]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until).map(_.toDouble), classOf[DoubleStepper]
				)
			},
			forAll { (array :Array[Double], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[Double]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[DoubleStepper]
				)
			},
			forAll { (array :Array[String], from :Int, until :Int) =>
				stepperProperty(
					"Stepper.over(Array[String]," + from + "," + until +")", Stepper.slice(array, from, until),
						ArraySeq.unsafeWrapArray(array).slice(from, until), classOf[AnyStepper[String]]
				)
			},
		)

		property("ofAny[String]()") =
			stepperProperty("", Stepper.ofAny(), Seq[String](), classOf[AnyStepper[_]])
		property("ofAny[String](\"1\")") = 
			stepperProperty("", Stepper.ofAny("1"), Seq("1"), classOf[AnyStepper[_]])
		property("ofAny[String](\"1\", \"2\")") =
			stepperProperty("", Stepper.ofAny("1", "2"), Seq("1", "2"), classOf[AnyStepper[_]])

		property("ofInt") =
			stepperProperty("", Stepper.ofInt(), Seq[Int](), classOf[IntStepper])
		property("ofInt(1)") = 
			stepperProperty("", Stepper.ofInt(1), Seq(1), classOf[IntStepper])
		property("ofInt(1, 2)") =
			stepperProperty("", Stepper.ofInt(1, 2), Seq(1, 2), classOf[IntStepper])

		property("ofInt(String, from, until)") = forAll { (string :String, from :Int, until :Int) =>
			stepperProperty("",
				Stepper.ofInt(string, from, until),
				string.map(_.toInt).slice(from, until),
				classOf[IntStepper]
			)
		}
		property("ofLong") =
			stepperProperty("", Stepper.ofLong(), Seq[Long](), classOf[LongStepper])
		property("ofLong(1)") =
			stepperProperty("", Stepper.ofLong(1), Seq(1L), classOf[LongStepper])
		property("ofLong(1, 2)") =
			stepperProperty("", Stepper.ofLong(1, 2), Seq(1L, 2L), classOf[LongStepper])

		property("ofDouble") =
			stepperProperty("", Stepper.ofDouble(), Seq[Double](), classOf[DoubleStepper])
		property("ofDouble(1.0)") =
			stepperProperty("", Stepper.ofDouble(1.0), Seq(1.0), classOf[DoubleStepper])
		property("ofDouble(1.0, 2.0)") =
			stepperProperty("", Stepper.ofDouble(1.0, 2.0), Seq(1.0, 2.0), classOf[DoubleStepper])

	}

}
