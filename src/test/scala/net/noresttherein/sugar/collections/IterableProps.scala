package net.noresttherein.sugar.collections

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{EvidenceIterableFactory, Factory, IterableFactory, IterableOps, Stepper, View, immutable, mutable}
import scala.collection.immutable.{ArraySeq, TreeSet}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.ClassTag
import scala.util.{Success, Try}

import net.noresttherein.sugar.arrays.ArrayCompanionExtension
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop.{AnyOperators, all, forAll}
import org.scalacheck.commands.Commands
import org.scalacheck.util.{Buildable, ConsoleReporter, Pretty}
import net.noresttherein.sugar.casting.{castTypeConstructorMethods, castTypeParamMethods}
import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, FlatMap, Fold, FoldSide, Map, collect, filter, flatMap, fold, foldLeft, foldRight, foldZero, map, value}
import net.noresttherein.sugar.collections.extensions.{FactoryExtension, IterableExtension, IterableOnceExtension}
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.testing.scalacheck.buildable
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, PropExtension}
import net.noresttherein.sugar.testing.scalacheck.typeClasses._




/** A framework base class for specifications of `Iterable` collections, not defining any tested properties itself.
  * Extracted also to impose least restrictive upper bounds on the tested type.
  *
  * @tparam C the type constructor of the tested collection, assumed to be a subtype of `Iterable`.
  * @tparam S a more generic, standard (with an existing implementation) type constructor for the reference collection.
  *           It should be a subtype of `Iterable`.
  * @tparam E type constructor for evidence needed to create an instance of `C[X]` (and, potentially, `S[X]`).
  */
trait GenericIterableProps[C[A] <: IterableOps[A, Any1, Any], S[A] >: C[A] @uncheckedVariance <: IterableOps[A, Any1, Any], E[_]]
	extends Properties
{
	val parameters = overrideParameters(Test.Parameters.default)

	/** A factory of the reference collection type (assumed to work correctly),
	  * used to generate arbitrary collection instances for testing.
	  */
	protected def S[T :E] :Factory[T, S[T]] //= referenceFactory[T]

	/** A factory of the tested collection type, used primarily to convert an arbitrary instance
	  * of reference collection type `S[T]` to a tested collection instance, but also - mainly for formality -
	  * to convert `Iterable` returned by tested methods of `C` back to `C`. For this reason, its `from` method
	  * should always return the argument, if it is already a `C`.
	  */
	protected def C[T :E] :Factory[T, C[T]] //= checkedFactory[T]

	@inline implicit protected final def SAsIterable[T](coll :S[T] @uncheckedVariance) :Iterable[T] = coll.asIterable
	@inline implicit protected final def CAsIterable[T](coll :C[T] @uncheckedVariance) :Iterable[T] = coll.asIterable

	protected implicit def buildableReference[T :E] :Buildable[T, S[T] @uncheckedVariance] = new Buildable[T, S[T]] {
		override def builder :Builder[T, S[T]] = S[T].newBuilder
	}
	protected implicit def buildableChecked[T :E] :Buildable[T, C[T] @uncheckedVariance] = new Buildable[T, C[T]] {
		override def builder :Builder[T, C[T]] = C[T].newBuilder
	}
	protected implicit def arbitraryReference[T :E :Arbitrary] :Arbitrary[S[T] @uncheckedVariance] = Arbitrary(
		for {
			size <- Gen.choose(0, parameters.maxSize)
			col  <- Gen.buildableOfN[S[T], T](size, Arbitrary.arbitrary[T])
		} yield col
	)
	protected implicit def arbitraryChecked[T :E :Arbitrary] :Arbitrary[C[T] @uncheckedVariance] = Arbitrary(
		for {
			size <- Gen.choose(0, parameters.maxSize)
			col  <- Gen.buildableOfN[C[T], T](size, Arbitrary.arbitrary[T])
		} yield col
	)
	protected implicit def arbitraryIterable[X :Arbitrary :ClassTag] :Arbitrary[Iterable[X]] = Arbitrary(
		for {
			length <- Gen.choose(0, parameters.maxSize)
			gen     = Arbitrary.arbitrary[X]
			items  <- Gen.oneOf(
				Gen.listOfN(length, gen),
				Gen.containerOfN[Vector, X](length, gen),
				Gen.containerOfN[ArraySeq, X](length, gen),
				Gen.containerOfN[mutable.ArraySeq, X](length, gen),
				Gen.containerOfN[Set, X](length, gen),
				Gen.containerOfN[UnorderedItems, X](length, gen),
				Gen.containerOfN[StrictView, X](length, gen)
			)
		} yield items
	)
	implicit def arbitrarySeq[X :Arbitrary :ClassTag] :Arbitrary[Seq[X]] = Arbitrary(
		for {
			length <- Gen.choose(0, parameters.maxSize)
			gen     = Arbitrary.arbitrary[X]
			items  <- Gen.oneOf(
				Gen.listOfN(length, gen),
				Gen.containerOfN[Vector, X](length, gen),
				Gen.containerOfN[ArraySeq, X](length, gen),
//				Gen.containerOfN[mutable.ArraySeq, X](length, gen),
			)
		} yield items
	)

	protected implicit def intEvidence    :E[Int]
	protected implicit def longEvidence   :E[Long]
	protected implicit def stringEvidence :E[String]
	protected implicit def intSEvidence   :E[S[Int]] @uncheckedVariance
	protected implicit def intCEvidence   :E[C[Int]] @uncheckedVariance

	protected implicit def pairEvidence[A :E, B :E] :E[(A, B)]

	implicit val intFilter             :Filter[Int]             =
		new Filter[Int]((i :Int) => i % 10 >= 7)
	implicit val intMapToLong          :Map[Int, Long]          =
		new Map[Int, Long](i => i.toLong << 32 | i & 0xffffffffL)
	implicit val intFlatMapToLong      :FlatMap[Int, Long]      =
		new FlatMap[Int, Long]((i :Int) => Seq.iterate(i.toLong, 10)(_ * i))
	implicit val intFold               :Fold[Int]               =
		new Fold[Int](Int.MinValue, (i :Int, j :Int) => i max j)
	implicit val intFoldLongSide       :FoldSide[Long, Int]     =
		new FoldSide[Long, Int](0L, (acc :Long, i :Int) => acc + i)
	implicit val longFilter            :Filter[Long]            =
		new Filter[Long]((i :Long) => i % 10 >= 7)
	implicit val longMapToString       :Map[Long, String]       =
		new Map[Long, String](String.valueOf(_:Long))
	implicit val longFlatMapToLong     :FlatMap[Long, Long]     =
		new FlatMap[Long, Long]((i :Long) => Seq.iterate(i, 10)(_ + i))
	implicit val longFold              :Fold[Long]              =
		new Fold[Long](Long.MinValue, (i :Long, j :Long) => i max j)
	implicit val longFoldLongSide      :FoldSide[Long, Long]    =
		new FoldSide[Long, Long](0L, (acc :Long, i :Long) => acc + i)
	implicit val stringFilter          :Filter[String]          =
		new Filter[String](_.toIntOption.exists(_ % 2 == 0))
	implicit val stringMapToString     :Map[String, String]     =
		new Map[String, String](_ + ":)")
	implicit val stringFlatMapToString :FlatMap[String, String] =
		new FlatMap[String, String](_.split(""))
	implicit val stringFold            :Fold[String]            =
		new Fold[String]("", _ + _)
	implicit val stringFoldIntSide     :FoldSide[Int, String]   =
		new FoldSide[Int, String](0, _ + _.length)

	implicit val byteFilter   :Filter[Byte] = new Filter[Byte]((i :Byte) => i % 10 >= 7)
	implicit val shortFilter  :Filter[Short] = new Filter[Short]((i :Short) => i % 10 >= 7)
	implicit val charFilter   :Filter[Char] = new Filter[Char]((i :Char) => i.isLetter)
//	implicit val longFilter     = new Filter[Long]((i :Long) => i % 10 >= 7)
	implicit val floatFilter  :Filter[Float] = new Filter[Float]((i :Float) => i <= 0)
	implicit val doubleFilter :Filter[Double] = new Filter[Double]((i :Double) => i <= 0)



	/** A higher order method for testing properties accepting an index of an element in the collection.
	  * Tests `prop` `forAll { (sut :C[A], index :Int) => }`, as well as `Int.MinValue` and `Int.MaxValue`
	  * for the index. The range of values otherwise used for `index` is `Short.MinValue` to `Short.MaxValue`,
	  * to reduce the number of fail path tests. Note that it means that larger collections may not be suitable
	  * tested in the result.
	  */
	protected def forAllIndices[A](prop :(C[A], Int) => Prop)
	                              (implicit ev :E[A], a :Arbitrary[A], s :Shrink[A], p :A => Pretty) :Prop =
		forAll { (subject :C[A]) => s"Input: $subject :${subject.localClassName}" lbl_:
			forAll { (index :Short) => prop(subject, index) } &&
				("Int.MinValue" lbl_: prop(subject, Int.MinValue)) &&
				("Int.MaxValue" lbl_: prop(subject, Int.MaxValue))
		}

	protected def forAllSubsets[A, I](col :I)(prop :I => Prop)
	                                 (implicit isIterable :I <:< IterableOps[A, Any1, I],
	                                  select :Arbitrary[Boolean], ev :E[A])
			:Prop =
	{
		val subset = col.filter(_ => select.arbitrary.sample.get)
		prop(subset) lbl "Subset: " + subset
	}


	/** Applies `check` to arbitrary instances of the reference collection `S[A]`,
	  * as wel as instances of the tested collection produced by factory
	  * [[net.noresttherein.sugar.collections.SpecificIterableProps.C C]] from said reference collections.
	  */
	protected def forAllChecked[A :Arbitrary :E](check :(S[A], C[A]) => Prop) :Prop =
		forAll { elems :C[A] => check(elems to ArraySeq.untagged to S, elems) }

	protected def compare[A](expect :S[A] @uncheckedVariance, result :C[A] @uncheckedVariance) :Prop =
		(expect =? result) && ((expect ?= result) lbl s"EXPECTED.equals") lbl
			"EXPECTED: " + expect + " (size=" + expect.size + ");\nGOT:      " + result + " (size=" + result.size + ")"

	/** Compares two collection results of executing some tested method for a reference collection `S`,
	  * as well as an equal (assuming correctness of its builder) collection `C`. Assumes the runtime types
	  * of the arguments to be `S[E]` and `C[E]`, respectively (not tested and not strictly required for practicality
	  * of not having to use bounds of `IterableOps` and the like to enforce the upper bound `C[E]` for method results.
	  * Takes into account the fact if the collection type has an inherent order (that is, `expect` and `result`
	  * must have the elements in the same order). Tests for equality, as well as `sameElements` for ordered collections.
	  */
//	@deprecated("use compare[A](S[A], C[A])", "")
//	protected def compare[A](expect :Iterable[A], result :Iterable[A]) :Prop =
//		Seq(
//			Option.when(hasOrder)(Prop(expect.iterator sameElements result) :| "iterator.sameElements" ),
//			Option.when(hasEquals)(expect =? result label "equals"),
//			Option.when(symmetricEquals)(expect == result lbl s"$expect == $result")
//		).flatten.reduceOption(_ && _) getOrElse
//		(expect =? result) && (result =? expect lbl s"EXPECTED.equals") lbl
//			"EXPECTED: " + expect + " (size=" + expect.size + ");\nGOT:      " + result + " (size=" + result.size + ")"
	protected def shouldEqual[A :Arbitrary :E, X](expect :Iterable[A], subject :C[A])(f :S[A] => X) :Prop =
		f(expect to S) =? f(subject)

	/** The primary function for testing methods which do not return another collection, or more specifically,
	  * need only be tested for equality. Applies `f` to multiple arbitrary instances of reference collection `S`,
	  * together with instances of the tested collection type produced by its factory
	  * [[net.noresttherein.sugar.collections.SpecificIterableProps.C C]] from the former, and compares the results
	  * for equality.
	  */
	protected def shouldEqual[A :Arbitrary :E, X](f :S[A] => X) :Prop =
		forAll { elems :C[A] => shouldEqual[A, X](elems to Vector, elems)(f) }


	/** A factory for comparing the results of executing a method for both a collection of the reference type
	  * and an equal one under test in the same way as
	  * [[net.noresttherein.sugar.collections.SpecificIterableProps.shouldEqual compare]], but executes the function
	  * passed as an argument to the returned object's `apply` only for non empty collections, while at the same time
	  * checking that executing it on an empty collection throws an exception of type `T`.
	  */
	protected def nonEmptyShouldEqual[T <: Throwable] = new NonEmptyShouldEqual[T]

	protected class NonEmptyShouldEqual[T <: Throwable] {
		def apply[A :Arbitrary :E, X](f :S[A] => X)(implicit tag :ClassTag[T]) :Prop =
			forAll { elems :C[A] =>
				if (elems.isEmpty) f(elems).throws[T]
				else shouldEqual[A, X](elems to Vector, elems)(f)
			}
	}

//	protected def orderedCompare[A, X](expect :S[A], result :C[A])(f :Iterable[A] => Iterable[X]) :Prop =
//		compare(if (hasOrder) f(expect.asIterable) else f(result to Vector), f(result.asIterable))

	/** @return `compare(f(expect) to S, f(result))`. */
	protected def orderedCompare[A, X :E](expect :Iterable[A], result :C[A] @uncheckedVariance)
	                                     (f :Iterable[A] => Iterable[X]) :Prop =
		compare(f(expect).to(S), f(result.asIterable).castCons[C])

	protected def orderedCompare[A :Arbitrary :E, X :E](f :Iterable[A] => Iterable[X]) :Prop =
		forAll { elems :C[A] => orderedCompare[A, X](elems to Vector, elems)(f) }


	protected def nonEmptyOrderedCompare[T <: Throwable] = new NonEmptyOrderedCompare[T]

	protected class NonEmptyOrderedCompare[T <: Throwable] {
		def apply[A, X :E](expect :Iterable[A], elems :C[A] @uncheckedVariance)
		                  (f :Iterable[A] => Iterable[X])(implicit tag :ClassTag[T]) :Prop =
			if (expect.isEmpty) f(elems.asIterable).throws[T]
			else orderedCompare(expect, elems)(f)
	}

	/** A property comparing `f(expect)` and `f(result)`. */
	protected def orderedShouldEqual[A, X](expect :Iterable[A], result :C[A])(f :Iterable[A] => X) :Prop =
		f(expect) =? f(result.asIterable)

	/** Compares the results of executing an order-dependent method like `tail` on an instance of the tested collection
	  * `coll` with an expect result obtained by the same method on a known working implementation obtained from
	  * `f(coll to Vector)`. This method is used in tests of relevant methods when the tested collection doesn't
	  * have an inherent order, and, in particular, an equal instance of the reference collection may contain
	  * the elements in a different order, even if one collection was directly converted into the second one.
	  */
	protected def orderedShouldEqual[A :Arbitrary :E, X](f :Iterable[A] => X) :Prop =
		forAll { subject :C[A] => orderedShouldEqual[A, X](subject to Vector, subject)(f) }

	/** Like [[net.noresttherein.sugar.collections.SpecificIterableProps.orderedShouldEqual orderedShouldEqual]],
	  * applies the function given as the argument to the `apply` method of the returned object to a non-empty instance
	  * of the tested collection type and a `Seq` obtained by converting the former, and tests the results
	  * for equality. Additionally,
	  * similarly to [[net.noresttherein.sugar.collections.SpecificIterableProps.nonEmptyShouldEqual nonEmptyShouldEqual]],
	  * it tests that the given function throws an exception of type `T` when executed for an empty instance.
	  */
	protected def nonEmptyOrderedShouldEqual[T <: Throwable] = new NonEmptyOrderedShouldEqual[T]

	protected class NonEmptyOrderedShouldEqual[T <: Throwable] {
		def apply[A :Arbitrary :E, X](f :Iterable[A] => X)(implicit tag :ClassTag[T]) :Prop =
			forAll { elems :C[A] =>
				if (elems.isEmpty) f(elems.asIterable).throws[T]
				else orderedShouldEqual[A, X](elems to Vector, elems)(f)
			}
	}

	/** Same as two argument validate, but additionally attaches a lazy label to the created property. */
	protected def test[A, F, M, FM](label: => String, expect :S[A] @uncheckedVariance, result :C[A] @uncheckedVariance)
	                               (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A], filt :Filter[A],
	                                         fldA :FoldSide[F, A], evf :E[F], fld :Fold[A], mp :Map[A, M], evm :E[M],
	                                         fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
		compare(expect, result) && all(props(expect, result) :_*) lbl label

	/** Compares `expect` with `result` using [[net.noresttherein.sugar.collections.SpecificIterableProps.compare compare]],
	  * and combines properties for testing most of their methods obtained from
	  * `this.`[[net.noresttherein.sugar.collections.SpecificIterableProps.props props]] into a single property object.
	  */
	protected def test[A, F, M, FM](expect :S[A] @uncheckedVariance, result :C[A] @uncheckedVariance)
	                               (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A], filt :Filter[A],
	                                         fldA :FoldSide[F, A], evf :E[F], fld :Fold[A], mp :Map[A, M], evm :E[M],
	                                         fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
		test(
			s"REFERENCE: $expect (size=${expect.size});\nTESTING:   $result :${result.localClassName} (size=${result.size})",
			expect, result
		)

	/** The primary testing method for properties which return another collection (typically of the same type).
	  * Applies function `f` to arbitrary instances of the reference collection type and an instance of the tested type
	  * like [[net.noresttherein.sugar.collections.SpecificIterableProps.shouldEqual compare]], but, in addition
	  * to comparing the results for equality, executes a full test suite for most standard methods for both results,
	  * by passing both results to lower level [[net.noresttherein.sugar.collections.SpecificIterableProps.test test]].
	  * @see [[net.noresttherein.sugar.collections.SpecificIterableProps.props props]].
	  */
	protected def testSpecific[A, F, M, FM]
	                          (f :S[A] => S[A] @uncheckedVariance)
	                          (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A],
	                                    filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
	                                    mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
		forAll { elems :C[A] => test(f(elems to ArraySeq.untagged to S), f(elems).castCons[C]) }

	/** An object applicable to a function `S[A] => S[A]`, which creates a property executing that function
	  * for a non empty `C[A]` instance, and derived instance `S[A]` (as created by factory `S`).
	  * It additionally checks that the function throws exception `T` for an empty `C[A]`.
	  * @see [[net.noresttherein.sugar.collections.SpecificIterableProps.testSpecific]]
	  */
	protected def testSpecificNonEmpty[T <: Throwable] :TestSpecificNonEmpty[T] = new TestSpecificNonEmpty[T]

	protected class TestSpecificNonEmpty[T <: Throwable] {
		def apply[A, F, M, FM](f :S[A] => S[A] @uncheckedVariance)
		                      (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A], exTag :ClassTag[T],
		                                filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
		                                mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
			forAll { elems :C[A] =>
				if (elems.isEmpty) f(elems).throws[T]
				else test(f(elems to ArraySeq.untagged to S), f(elems).castCons[C])
			}
	}

	/** Creates a property testing an order-dependent property/method of `Iterable`.
	  * Applies function `f` to arbitrary instance of the tested type `col` and `col.toSeq`
	  * like [[net.noresttherein.sugar.collections.SpecificIterableProps.orderedShouldEqual orderedCompare]], but,
	  * in addition to comparing the results for equality, also executes a full test suite for most standard methods
	  * by passing both results to lower level
	  * [[net.noresttherein.sugar.collections.SpecificIterableProps.test test]].
	  * @param f a function executing a method of the argument `Iterable` and returning an `Iterable` of the same type:
	  *          `f(c :C[A])` must conform to `C[A]`.
	  * @see [[net.noresttherein.sugar.collections.SpecificIterableProps.props props]].
	  */
	protected def orderedTestSpecific[A, F, M, FM]
	                                 (f :Iterable[A] => Iterable[A])
	                                 (implicit arbitrary :Arbitrary[A], ev :E[A],
	                                  tag :ClassTag[A], filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
	                                  mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
		forAll { elems :C[A] => test(f(elems to ArraySeq.untagged) to S, f(elems.asIterable).castCons[C]) }

	/** An object applicable to a function `Iterable[A] => Iterable[A]`, which creates a property
	  * executing that function for a non empty `C[A]` instance, and derived instance `S[A]` (as created by factory `S`),
	  * similarly to [[net.noresttherein.sugar.collections.SpecificIterableProps.orderedTestSpecific orderedTestSpecific]].
	  * It additionally checks that the function throws exception `T` for an empty `C[A]`.
	  */
	protected def orderedTestSpecificNonEmpty[T <: Throwable] = new OrderedTestSpecificNonEmpty[T]

	protected class OrderedTestSpecificNonEmpty[T <: Throwable] {
		def apply[A, F, M, FM](f :Iterable[A] => Iterable[A])
		                      (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A], exTag :ClassTag[T],
		                                filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
		                                mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
			forAll { elems :C[A] =>
				if (elems.isEmpty) f(elems.asIterable).throws[T]
				else test(f(elems to ArraySeq.untagged) to S, f(elems.asIterable).castCons[C])
			}
	}


	protected def props[T, F, M, FM](expect :S[T] @uncheckedVariance, result :C[T] @uncheckedVariance)
	                                (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T], filt :Filter[T],
	                                 fldA :FoldSide[F, T], evf :E[F], fld :Fold[T], mp :Map[T, M], evm :E[M],
	                                 fmap :FlatMap[T, FM], evfm :E[FM]) :Seq[Prop]


	protected def hasOrder  = false

}




/** Tests methods of `IterableOps[E, CC, C]` which return `E`, `C`, or other type different than `CC[_]`.
  * Subclasses need to implement factory methods returning `Factory[X, C[X]]` and `Factory[X, S[X]]`,
  * for some supertype `S` of `C`, from the standard library and with the same semantics.
  * Defined properties execute the same code for an arbitrary `S` and `C` created from the latter,
  * and compare the results.
  * @tparam C the type constructor of the tested collection, assumed to be a subtype of `Iterable`.
  * @tparam S a more generic, standard (with an existing implementation) type constructor for the reference collection.
  *           It should be a subtype of `Iterable`.
  * @tparam E type constructor for evidence needed to create an instance of `C[X]` (and, potentially, `S[X]`).
  */
abstract class SpecificIterableProps[C[T] <: IterableOps[T, Any1, C[T]],
                                     S[T] >: C[T] <: IterableOps[T, Any1, S[T]], E[_]](name :String)
	extends Properties(name) with GenericIterableProps[C, S, E]
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(3, 140)).withMinSuccessfulTests(500).withMaxSize(64)

	/** The name (or its best guess) of the reference collection type, for the use in property names and labels. */
	protected def typeS :String = S[Int].source.localClassName

	if (isSerializable)
		property("Serializable")  = forAll { col :C[Int] => //This takes too long
		                                val out = new ByteArrayOutputStream()
		                                val obj = new ObjectOutputStream(out)
		                                obj.writeObject(col)
		                                obj.close()
		                                val in   = new ObjectInputStream(new ByteArrayInputStream(out.toByteArray))
		                                val copy = in.readObject().asInstanceOf[C[Int]]
		                                test(col, copy) lbl "Deserialized: " + copy + ": " + copy.localClassName
		                            }
	property("knownSize")         = Prop(!knowsSize) || forAll { col :C[Int] => col.knownSize =? col.size }
	property("size")              = shouldEqual((_ :S[Int]).size)
	property("compareSize")       = forAll { i :Int => shouldEqual((_ :S[Int]).sizeCompare(i).sign) }
	property("isEmpty")           = Prop(C[Int].fromSpecific(Nil).isEmpty) :| "empty is empty" && shouldEqual((_ :S[Int]).isEmpty)

	property("foreach")           = shouldEqual { s :S[Int] => var sum = 0; s.foreach { sum += _ }; sum }
	property("forall")            = shouldEqual((_ :S[Int]).forall(filter))
	property("exists")            = shouldEqual((_ :S[Int]).exists(filter))
	property("count")             = shouldEqual((_ :S[Int]).count(filter))
	property("fold")              = shouldEqual((_ :S[Int]).fold(value[Int])(fold))
	property("foldLeft")          = shouldEqual((_ :S[Int]).foldLeft(foldZero[Long, Int])(foldLeft))
	property("foldRight")         = shouldEqual((_ :S[Int]).foldRight(foldZero[Long, Int])(foldRight))
	property("reduce")            = nonEmptyShouldEqual[UnsupportedOperationException]((_ :S[Int]).reduce(fold[Int]))
	property("reduceLeft")        = nonEmptyShouldEqual[UnsupportedOperationException]((_ :S[Int]).reduceLeft(fold[Int]))
	property("reduceRight")       = nonEmptyShouldEqual[UnsupportedOperationException]((_ :S[Int]).reduceRight(fold[Int]))
	property("reduceLeftOption")  = shouldEqual((_ :S[Int]).reduceLeftOption(fold[Int]))
	property("reduceRightOption") = shouldEqual((_ :S[Int]).reduceRightOption(fold[Int]))
	property("copyToArray")       = forAll { (s :C[Int]) => copyToArrayProp(Vector.from(s), s) }

	property("empty")             = forAll { (col :C[Int]) => compare(S[Int].fromSpecific(Nil), col.empty) }
	property("filter")            = testSpecific((_ :S[Int]).filter(filter))
	property("filterNot")         = testSpecific((_ :S[Int]).filterNot(filter))
	property("partition")         = forAllChecked { (expect :S[Int], result :C[Int]) => //A bit too slow
	                                    val (expect_1, expect_2) = expect.partition(filter)
	                                    val (result_1, result_2) = result.partition(filter)
	                                	("_1" |: test(expect_1.to(S), result_1)) &&
			                                ("_2" |: test(expect_2.to(S), result_2))
	                                }
	property("iterator")          = forAllChecked { (expect :S[Int], result :C[Int]) => checkIterator(expect, result.iterator) }
	property("stepper")           = forAllChecked { (expect :S[Int], result :C[Int]) => checkStepper(expect, result.stepper) }

	property("head")              = nonEmptyOrderedShouldEqual[NoSuchElementException]((_ :Iterable[Int]).head)
	property("last")              = nonEmptyOrderedShouldEqual[NoSuchElementException]((_ :Iterable[Int]).last)
	property("headOption")        = orderedShouldEqual((_ :Iterable[Int]).headOption)
	property("lastOption")        = orderedShouldEqual((_ :Iterable[Int]).lastOption)
	property("find")              = orderedShouldEqual((_ :Iterable[Int]).find(filter))
	property("collectFirst")      = orderedShouldEqual((_ :Iterable[Int]).collectFirst(collect))

	property("init")              = orderedTestSpecificNonEmpty[UnsupportedOperationException]((_ :Iterable[Int]).init)
	property("tail")              = orderedTestSpecificNonEmpty[UnsupportedOperationException]((_ :Iterable[Int]).tail)
	property("take")              = forAll { i :Int => orderedTestSpecific((_:Iterable[Int]).take(i)) }
	property("drop")              = forAll { i :Int => orderedTestSpecific((_:Iterable[Int]).drop(i)) }
	property("takeRight")         = forAll { i :Int => orderedTestSpecific((_:Iterable[Int]).takeRight(i)) }
	property("dropRight")         = forAll { i :Int => orderedTestSpecific((_:Iterable[Int]).dropRight(i)) }
	property("slice")             = forAll { (from :Int, until :Int) =>
	                                    orderedTestSpecific((_:Iterable[Int]).slice(from, until))
	                                }


	/** A set of properties checked for every result of a method returning a `C[T]`.
	  * @param expect the result of executing the tested method on an equal instance of a reference collection.
	  * @param result the result of executing the tested method on an instance of the tested collection type.
	  * @tparam T  the element type of the collection.
	  * @tparam F  the argument and return type of `foldLeft` and `foldRight`.
	  * @tparam M  the element type to which the implicit `Map` type class maps `T` for the purpose of testing `map`.
	  * @tparam FM the element type to which the implicit `FlatMap` type class maps `T` for testing `flatMap`.
	  */
	protected override def props[T, F, M, FM](expect :S[T], result :C[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T], filt :Filter[T],
	                                          fldA :FoldSide[F, T], evf :E[F], fld :Fold[T], mp :Map[T, M], evm :E[M],
	                                           fmap :FlatMap[T, FM], evfm :E[FM]) :Seq[Prop] =
	{
		val ordered = if (hasOrder) expect.asIterable else result.toVector
		Seq(
			"knownSize"         lbl_: !knowsSize || expect.size =? result.knownSize,
			"size"              lbl_: expect.size =? result.size,
			"isEmpty"           lbl_: expect.isEmpty =? result.isEmpty,
			"sizeCompare"       lbl_: forAll { i :Int => expect.sizeCompare(i).sign =? result.sizeCompare(i).sign },

			"foreach"           lbl_: {
			                          	val b = List.newBuilder[T]
			                          	result.foreach(b += _)
			                          	val elems = b.result()
			                            (elems sameElements result) lbl s"!sameElements: $elems"
			                          },
//			"tapEach"
			"forall"            lbl_: expect.forall(filter) =? result.forall(filter),
			"exists"            lbl_: expect.exists(filter) =? result.exists(filter),
			"count"             lbl_: expect.count(filter) =? result.count(filter),
			"fold"              lbl_: expect.fold(value[T])(fold) =? result.fold(value[T])(fold),
			"foldLeft"          lbl_: expect.foldLeft(foldZero[F, T])(foldLeft) =?
			                          result.foldLeft(foldZero[F, T])(foldLeft),
			"foldRight"         lbl_: expect.foldRight(foldZero[F, T])(foldRight) =?
			                          result.foldRight(foldZero[F, T])(foldRight),
			"reduce"            lbl_: (if (expect.isEmpty) result.reduce(fold[T]).throws[UnsupportedOperationException]
			                          else expect.reduce(fold[T]) =? result.reduce(fold[T])),
			"reduceLeft"        lbl_: (if (expect.isEmpty) result.reduceLeft(fold[T]).throws[UnsupportedOperationException]
			                          else expect.reduceLeft(fold[T]) =? result.reduceLeft(fold[T])),
			"reduceRight"       lbl_: (if (expect.isEmpty) result.reduceRight(fold[T]).throws[UnsupportedOperationException]
			                          else expect.reduceRight(fold[T]) =? result.reduceRight(fold[T])),
			"reduceOption"      lbl_: expect.reduceOption(fold[T]) =? result.reduceOption(fold[T]),
			"reduceLeftOption"  lbl_: expect.reduceLeftOption(fold[T]) =? result.reduceLeftOption(fold[T]),
			"reduceRightOption" lbl_: expect.reduceRightOption(fold[T]) =? result.reduceRightOption(fold[T]),
			"copyToArray"       lbl_: copyToArrayProp(if (hasOrder) expect.asIterable else result.toVector, result),

			"iterator"          lbl_: checkIterator(expect, result.iterator),
			"stepper"           lbl_: checkStepper(expect, result.stepper),

			"empty"             lbl_: compare(expect.empty, result.empty),
			"filter"            lbl_: compare(expect.filter(filter), result.filter(filter)),
			"filterNot"         lbl_: compare(expect.filterNot(filter), result.filterNot(filter)),
			"partition"         lbl_: {
			                          	val (expect_1, expect_2) = expect.partition(filter)
			                          	val (result_1, result_2) = result.partition(filter)
			                          	compare(expect_1, result_1) && compare(expect_2, result_2)
			                          },

			"head"              lbl_: (if (expect.isEmpty) result.head.throws[NoSuchElementException]
			                          else ordered.head =? result.head),
			"last"              lbl_: (if (expect.isEmpty) result.last.throws[NoSuchElementException]
			                          else ordered.last =? result.last),
			"headOption"        lbl_: ordered.headOption =? result.headOption,
			"lastOption"        lbl_: ordered.lastOption =? result.lastOption,
			"find"              lbl_: ordered.find(filter) =? result.find(filter),
			"collectFirst"      lbl_: ordered.collectFirst(collect) =? result.collectFirst(collect),

			"init"              lbl_: nonEmptyOrderedCompare[UnsupportedOperationException](ordered, result)(_.init),
			"tail"              lbl_: nonEmptyOrderedCompare[UnsupportedOperationException](ordered, result)(_.tail),
			"take"              lbl_: forAll { n :Int => orderedCompare(ordered, result)(_.take(n)) },
			"drop"              lbl_: forAll { n :Int => orderedCompare(ordered, result)(_.drop(n)) },
			"takeRight"         lbl_: forAll { n :Int => orderedCompare(ordered, result)(_.takeRight(n)) },
			"dropRight"         lbl_: forAll { n :Int => orderedCompare(ordered, result)(_.dropRight(n)) },
			"slice"             lbl_: forAll { (i :Int, j :Int) => orderedCompare(ordered, result)(_.slice(i, j)) },
			"splitAt"           lbl_: forAll { n :Int =>
			                          	val (expect_1, expect_2) = ordered.splitAt(n)
			                          	val (result_1, result_2) = result.splitAt(n)
			                          	compare(expect_1 to S, result_1) :| "_1" &&
				                            compare(expect_2 to S, result_2) :| "_2"
			                          },
			"takeWhile"         lbl_: forAll { n :Int => orderedCompare(ordered, result)(_.takeWhile(filter)) } &&
			                             compare(expect, result.takeWhile(_ => true)) :| "all",
			"dropWhile"         lbl_: forAll { n :Int => orderedCompare(ordered, result)(_.dropWhile(filter)) },
			                             compare(expect.empty, result.dropWhile(_ => true)) :| "all",
			"span"              lbl_: forAll { n :Int =>
			                          	val (expect_1, expect_2) = expect.span(filter)
			                          	val (result_1, result_2) = result.span(filter)
			                          	compare(expect_1 to S, result_1) :| "_1" &&
				                            compare(expect_2 to S, result_2) :| "_2"
			                          },
		)
	}


	private def copyToArrayProp[T :Arbitrary :ClassTag](ordered :Iterable[T], subject :C[T]) :Prop =
		forAll { (capacity :Short, start :Int, len :Int) =>
			//Semantics of copyToArray in the standard implementation are inconsistent
			// and susceptible to arithmetic overflow, so this is an approximation.
			val specific = Array.fill(capacity.toInt.abs)(Arbitrary.arbitrary[T].sample.get)
			val generic  = specific.map[Any](_.toString)
			//For some strange reason the check for zero array size is not made before checking for negative start.
			if (start < 0 && (len max 0 min subject.size) > 0) {
				(subject.copyToArray(specific, start, len).throws[IndexOutOfBoundsException] lbl
					s"copyToArray(${specific.mkString("Array[Int](", ", ", ")")}, $start, $len)"
				) && (
					subject.copyToArray(generic, start, len).throws[IndexOutOfBoundsException] lbl
						s"copyToArray(${generic.mkString("Array[Any](", ", ", ")")}, $start, $len)"
				)
			} else {
				//We impose our own, precise semantics, and do not pass any invalid arguments to the reference collection.
				val copy = len max 0 min subject.size min capacity.toInt.abs - math.max(0, start) max 0
				val clippedStart = if (copy == 0) 0 else start
				val expectSpecific = specific.clone()
				val expectGeneric  = generic.clone()
				ordered.copyToArray(expectSpecific, clippedStart, copy)
				ordered.copyToArray(expectGeneric, clippedStart, copy)
				(
					(copy =? (if (len < 0) 0 else subject.copyToArray(specific, start, len)) &&
						ArraySeq.unsafeWrapArray(expectSpecific) =?
							ArraySeq.unsafeWrapArray(specific)
						) lbl s"copyToArray[Int](_, $start, $len)"
					) && (
					(copy =? (if (len < 0) 0 else subject.copyToArray(generic, start, len)) &&
						ArraySeq.unsafeWrapArray(expectGeneric) =?
							ArraySeq.unsafeWrapArray(generic)
						) lbl s"copyToArray[Any](_, $start, $len)"
					)
			}
		}


	protected def checkIterator[T :E](expect :S[T], iterator: => Iterator[T], prefix :String = "iterator") :Prop =
		(expect.asIterable sameElements iterator) lbl
			"!sameElements: " + iterator.toList.mkString("Iterator(", ", ", ")")

	/** Properties for results of `stepper` method of the tested collection type.
	  * @param expect  a reference collection with the same elements which the tested stepper should return.
	  * @param stepper a lazy expression used to create instances of the tested stepper,
	  *                for the purpose of testing its various methods.
	  * @param prefix  the name of the method used to create the stepper, for use in labels (mainly useful when
	  *                the latter is not `stepper` method itself).
	  */
	protected def checkStepper[T :E](expect :S[T], stepper: => Stepper[T], prefix :String = "stepper") :Prop = {
		val s = stepper
		val b = List.newBuilder[T]
		while (s.hasStep)
			b += s.nextStep()
		val elems = b.result()
		(expect.asIterable sameElements elems lbl "!sameElements:" + elems.mkString("Stepper(", ", ", ")")) &&
			(prefix + ".iterator" lbl_: checkIterator(expect, stepper.iterator, prefix + ".iterator")) &&
			(prefix + ".javaIterator" lbl_: {
				val elems = stepper.javaIterator.castParam[T].asScala.toList
				expect sameElements elems lbl "!sameElements: " + elems.mkString("JIterator(", ", ", ")")
			}) &&
			(prefix + ".spliterator.forEachRemaining" lbl_: {
				val b = List.newBuilder[T]
				stepper.spliterator.castParam[T].forEachRemaining(b += (_:T))
				val elems = b.result()
				expect sameElements elems lbl "!sameElements: " + elems
			}) &&
			(prefix + ".spliterator.tryAdvance" lbl_: {
				val b = List.newBuilder[T]
				val s = stepper.spliterator.castParam[T]
				while (s.tryAdvance(b += (_ :T)))
					{}
				val elems = b.result()
				(expect sameElements b.result()) lbl "!sameElements: " + elems.mkString("Spliterator(", ", ", ")")
			}) &&
			(prefix + ".spliterator.trySplit" lbl_: {
				val s1 = stepper.spliterator.castParam[T]
				val s2 = s1.trySplit.castParam[T]
				val buf = List.newBuilder[T]
				if (s2 != null)
					while (s2.tryAdvance(buf += (_ :T)))
						{}
				while (s1.tryAdvance(buf += (_ :T)))
					{}
				val elems = buf.result()
				expect.castParam[T].asIterable sameElements elems lbl {
					val s1 = stepper.spliterator.castParam[T]
					val s2 = s1.trySplit.castParam[T]
					val prefix = ArrayBuffer.empty[T]
					val suffix = ArrayBuffer.empty[T]
					if (s2 != null)
						s2.forEachRemaining(prefix += _)
					s1.forEachRemaining(suffix += _)
					prefix.mkString("!sameElements: (", ", ", ")") + " + " + suffix.mkString("(", ", ", ")")
				}
			})
	}



	protected def knowsSize = false
	protected def hasEquals = true
	protected def symmetricEquals = true
	protected def isSerializable :Boolean = C[Int].newBuilder.result().isInstanceOf[Serializable]

	/* Evidence instances required to build the tested collection for several element types,
	 * and type classes providing example function arguments to the tested methods.
	 */

}




/** Base class for property specifications of collection type `C`, and its semantically equivalent interface type `S`.
  * It requires that no implicit evidence for `E` is necessary to create a `C[E]`, and,
  * in addition to properties tested by `SpecificIterableProps`, it also tests methods returning instances of `C`
  * with a different element type than the tested collection.
  * @tparam C the type constructor of the tested collection.
  * @tparam S a more generic, standard (with an existing implementation) type constructor for the reference collection;
  *           must be also a subtype of `Iterable[T]`.
  * @see [[net.noresttherein.sugar.collections.EvidenceIterableProps]]
  */ //todo: full IteratorProps testing on this.iterator (take, drop, copyToArray, etc.)
abstract class IterableProps[C[T] <: IterableOps[T, C, C[T]], S[T] >: C[T] <: IterableOps[T, S, S[T]]]
                            (name :String)
                            (val checkedFactory :IterableFactory[C], val referenceFactory :IterableFactory[S])
	extends SpecificIterableProps[C, S, Dummy](name)
{
	def this(checkedFactory :IterableFactory[C], referenceFactory :IterableFactory[S]) =
		this(checkedFactory.toString)(checkedFactory, referenceFactory)

	type E[X] = Dummy[X]

	property("map")               = test((_ :S[Int]).map(map))
	property("flatMap(Seq)")      = test((_ :S[Int]).flatMap(flatMap)) //This is very slow
	property(s"flatMap($name)")   = test((_ :S[Int]).flatMap(i => C[Long].fromSpecific(flatMap(i))))
	property(s"flatten($typeS)")  = test((_ :S[C[Int]]).flatten) //todo: this is extremely slow
	property("collect")           = test((_ :S[Int]).collect { case i if filter(i) => i })
	property("concat")            = forAll { list :List[Int] => test((_ :S[Int]) ++ list) }

	property("scan")              = orderedTest((_ :Iterable[Int]).scan(value[Int])(fold))
	property("scanLeft")          = orderedTest((_ :Iterable[Int]).scanLeft(foldZero[Long, Int])(foldLeft))
	property("scanRight")         = orderedTest((_ :Iterable[Int]).scanRight(foldZero[Long, Int])(foldRight))
	property("zipWithIndex")      = orderedCompare((_ :Iterable[Int]).zipWithIndex)

	protected override def props[T, F, M, FM](expect :S[T], result :C[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
	                                          filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                          mp :Map[T, M], evm :E[M], fmap :FlatMap[T, FM], evfm :E[FM]) :Seq[Prop] =
	{
		val ordered = if (hasOrder) expect.asIterable else result.toVector
		super.props[T, F, M, FM](expect, result) ++ Seq(
			"map"               lbl_: compare(expect.map(map), result.map(map)),
			"flatMap"           lbl_: compare(expect.flatMap(flatMap), result.flatMap(flatMap)),
			"flatten"           lbl_: compare(expect.map(flatMap).flatten, result.map(flatMap).flatten),
			"collect"           lbl_: compare(expect.collect(collect), result.collect(collect)),
			"scan"              lbl_: orderedCompare(ordered, result)(_.scan(value[T])(fold)),
			"scanLeft"          lbl_: orderedCompare(ordered, result)(_.scanLeft(foldZero[F, T])(foldLeft)),
			"scanRight"         lbl_: orderedCompare(ordered, result)(_.scanRight(foldZero[F, T])(foldRight)),
			"zipWithIndex"      lbl_: orderedCompare(ordered, result)(_.zipWithIndex)
		)
	}

	/** The primary testing method for properties which return another collection (typically of the same type).
	  * Applies function `f` to arbitrary instances of the reference collection type and an instance of the tested type
	  * like [[net.noresttherein.sugar.collections.SpecificIterableProps.shouldEqual shouldEqual]], but, in addition
	  * to comparing the results for equality, executes a full test suite for most standard methods for both results,
	  * comparing their outcome with `compare`. This method immediately delegates to the lower-level two argument
	  * `validate`, which in turn returns the property test obtained from method
	  * [[net.noresttherein.sugar.collections.SpecificIterableProps.props props]].
	  */
	protected def test[A, B, F, M, FM]
	                  (f :S[A] => S[B])
	                  (implicit input :Arbitrary[A], output :Arbitrary[B], ev1 :E[A], ev2 :E[B], tag :ClassTag[B],
	                            filt :Filter[B], fldA :FoldSide[F, B], evf :E[F], fld :Fold[B],
	                            mp :Map[B, M], evm :E[M], fmap :FlatMap[B, FM], evfm :E[FM]) :Prop =
		forAll { elems :C[A] => test[B, F, M, FM](f(elems to Vector to S) to S, f(elems).castCons[C]) }

	protected def testNonEmpty[T <: Throwable] = new TestNonEmpty[T]

	protected class TestNonEmpty[T <: Throwable] {
		def apply[A, B, F, M, FM](f :S[A] => S[B])
		                         (implicit input :Arbitrary[A], output :Arbitrary[B], ev1 :E[A], ev2 :E[B],
		                                   tag :ClassTag[B], exTag :ClassTag[T],
		                                   filt :Filter[B], fldA :FoldSide[F, B], evf :E[F], fld :Fold[B],
		                                   mp :Map[B, M], evm :E[M], fmap :FlatMap[B, FM], evfm :E[FM]) :Prop =
			forAll { elems :C[A] =>
				if (elems.isEmpty) f(elems).throws[T]
				else test[B, F, M, FM](f(elems to Vector to S) to S, f(elems).castCons[C])
			}
	}

	protected def orderedTest[A, B, F, M, FM]
	                         (f :Iterable[A] => Iterable[B])
	                         (implicit input :Arbitrary[A], output :Arbitrary[B], ev1 :E[A], ev2 :E[B],
	                          tag :ClassTag[B], filt :Filter[B], fldA :FoldSide[F, B], evf :E[F], fld :Fold[B],
	                          mp :Map[B, M], evm :E[M], fmap :FlatMap[B, FM], evfm :E[FM]) :Prop =
		forAll { elems :C[A] => test[B, F, M, FM](f(elems to Vector) to S, f(elems.asIterable).castCons[C]) }

	protected def orderedTestNonEmpty[T <: Throwable] = new OrderedTestNonEmpty[T]

	protected class OrderedTestNonEmpty[T <: Throwable] {
		def apply[A, B, F, M, FM](f :Iterable[A] => Iterable[B])
		                         (implicit input :Arbitrary[A], output :Arbitrary[B], ev1 :E[A], ev2 :E[B],
		                                   tag :ClassTag[B], exTag :ClassTag[T],
		                                   filt :Filter[B], fldA :FoldSide[F, B], evf :E[F], fld :Fold[B],
		                                   mp :Map[B, M], evm :E[M], fmap :FlatMap[B, FM], evfm :E[FM]) :Prop =
			forAll { elems :C[A] =>
				if (elems.isEmpty) f(elems.asIterable).throws[T]
				else test[B, F, M, FM](f(elems to ArraySeq.untagged) to S, f(elems.asIterable).castCons[C])
			}
	}

	protected override def testSpecific[A, F, M, FM]
	                                   (f :S[A] => S[A])
	                                   (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A],
	                                             filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
	                                             mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
		test(f)

	protected override def testSpecificNonEmpty[T <: Throwable] :TestSpecificNonEmpty[T] =
		new TestSpecificNonEmpty[T] {
			override def apply[A, F, M, FM](f :S[A] => S[A])
			                               (implicit arbitrary :Arbitrary[A], ev :E[A],
			                                         tag :ClassTag[A], exTag :ClassTag[T],
			                                         filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
			                                         mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
				testNonEmpty[T](f)
		}

	protected override def orderedTestSpecific[A, F, M, FM]
	                                          (f :Iterable[A] => Iterable[A])
	                                          (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A],
	                                                    filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
	                                                    mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
		orderedTest(f)

	protected override def orderedTestSpecificNonEmpty[T <: Throwable] :OrderedTestSpecificNonEmpty[T] =
		new OrderedTestSpecificNonEmpty[T] {
			override def apply[A, F, M, FM]
			                  (f :Iterable[A] => Iterable[A])
			                  (implicit arbitrary :Arbitrary[A], ev :E[A], tag :ClassTag[A], exTag :ClassTag[T],
			                            filt :Filter[A], fldA :FoldSide[F, A], evf :E[F], fld :Fold[A],
			                            mp :Map[A, M], evm :E[M], fmap :FlatMap[A, FM], evfm :E[FM]) :Prop =
				orderedTestNonEmpty[T](f)
		}

	protected implicit override def intEvidence    :Dummy[Int] = new Dummy
	protected implicit override def longEvidence   :Dummy[Long] = new Dummy
	protected implicit override def stringEvidence :Dummy[String] = new Dummy
	protected implicit override def intSEvidence   :Dummy[S[Int]] = new Dummy
	protected implicit override def intCEvidence   :Dummy[C[Int]] = new Dummy
	protected implicit override def pairEvidence[A :Dummy, B :Dummy] :Dummy[(A, B)] = new Dummy

	protected override def S[T :Dummy] :Factory[T, S[T]] = referenceFactory
	protected override def C[T :Dummy] :Factory[T, C[T]] = checkedFactory

	protected def factoryProps :Properties = new IterableFactoryProps

	private class IterableFactoryProps extends Properties("iterableFactory") {
		property("from") = forAll { items :Iterable[Int] =>
			test[Int, Long, Long, Long](referenceFactory from items, checkedFactory from items)
		}
	}

	include(factoryProps)
}



object IterableProps {
	class Filter[T](val f :T => Boolean)
	class FoldSide[A, T](val zero :A, val f :(A, T) => A)
	class Fold[T](val zero :T, val f :(T, T) => T)
	class Map[-T, +A](val f :T => A)
	class FlatMap[-T, +A](val f :T => IterableOnce[A])
	class Dummy[T]
//	type FlatMap[-T, +A] = Map[T, IterableOnce[A]]

//	def collect[T](implicit filter :Filter[T]) :PartialFunction[T, T] = { case a :T if filter.f(a) => a }
	def collect[T, A](implicit filter :Filter[T], map :Map[T, A]) :PartialFunction[T, A] = {
		case a :T @unchecked if filter.f(a) => map.f(a)
	}
	@inline def filter[T](implicit filter :Filter[T]) :T => Boolean = filter.f
	@inline def filter[T](x :T)(implicit filter :Filter[T]) :Boolean = filter.f(x)
	@inline def foldZero[A, T](implicit fold :FoldSide[A, T]) :A = fold.zero
//	@inline def foldZero[T](implicit fold :Fold[T]) :T = fold.zero
	@inline def value[T](implicit fold :Fold[T]) :T = fold.zero
	@inline def foldLeft[A, T](implicit fold :FoldSide[A, T]) :(A, T) => A = fold.f
	@inline def foldLeft[A, T](a :A, t :T)(implicit fold :FoldSide[A, T]) :A = fold.f(a, t)
	@inline def foldRight[A, T](implicit fold :FoldSide[A, T]) :(T, A) => A = (t, a) => fold.f(a, t)
	@inline def foldRight[A, T](t :T, a :A)(implicit fold :FoldSide[A, T]) :A = fold.f(a, t)
	@inline def fold[T](implicit fold :Fold[T]) :(T, T) => T = fold.f
	@inline def fold[T](a :T, b :T)(implicit fold :Fold[T]) :T = fold.f(a, b)
	@inline def map[T, A](implicit map :Map[T, A]) :T => A = map.f
	@inline def map[T, A](x :T)(implicit map :Map[T, A]) :A = map.f(x)
	@inline def flatMap[T, A](implicit map :FlatMap[T, A]) :T => IterableOnce[A] = map.f
	@inline def flatMap[T, A](x :T)(implicit map :FlatMap[T, A]) :IterableOnce[A] = map.f(x)
	@inline implicit def dummy[T] :Dummy[T] = new Dummy[T]
}






/** Base class for property specifications of collection type `C`, and its semantically equivalent interface type `S`,
  * requiring type class `E` for their element types.
  * @tparam C the type constructor of the tested collection.
  * @tparam S a more generic, standard (with an existing implementation) type constructor for the reference collection;
  *           must be also a subtype of `Iterable[T]`.
  * @tparam E type constructor for evidence needed to create an instance of `C[X]`.
  */
abstract class EvidenceIterableProps[C[T] <: IterableOps[T, Iterable, C[T]],
                                     S[T] >: C[T] <: IterableOps[T, Iterable, S[T]], E[_]]
                                    (name :String)
                                    (val checkedFactory :EvidenceIterableFactory[C, E],
                                     val referenceFactory :EvidenceIterableFactory[S, E])
                                    (implicit override val intEvidence :E[Int], override val longEvidence :E[Long],
                                     override val stringEvidence :E[String],
                                     override val intSEvidence :E[S[Int]], override val intCEvidence :E[C[Int]])
	extends SpecificIterableProps[C, S, E](name)
{
	def this(checkedFactory :EvidenceIterableFactory[C, E], referenceFactory :EvidenceIterableFactory[S, E])
	        (implicit intEvidence :E[Int], longEvidence :E[Long], stringEvidence :E[String],
	                  intSEvidence :E[S[Int]], intCEvidence :E[C[Int]]) =
		this(checkedFactory.toString)(checkedFactory, referenceFactory)

	//todo: same properties as in IterableProps
	protected override def S[T :E] = referenceFactory
	protected override def C[T :E] = checkedFactory

	protected def factoryProps :Properties = new EvidenceIterableFactoryProps

	private class EvidenceIterableFactoryProps extends Properties("iterableFactory") {
		property("from") = forAll { items :Iterable[Int] =>
			test(referenceFactory from items, checkedFactory from items)
		}
	}

	include(factoryProps)
}


abstract class ClassTagIterableProps[C[T] <: IterableOps[T, Iterable, C[T]], S[T] >: C[T] <: IterableOps[T, Iterable, S[T]]]
                                    (name :String)
                                    (checkedFactory :EvidenceIterableFactory[C, ClassTag],
                                     referenceFactory :EvidenceIterableFactory[S, ClassTag])
                                    (implicit override val intSEvidence :ClassTag[S[Int]],
                                     override val intCEvidence :ClassTag[C[Int]])
	extends EvidenceIterableProps[C, S, ClassTag](name)(checkedFactory, referenceFactory)
{
//	property("map")               = testSpecific((_ :S[Int]).map(map))
//	property("flatMap(Seq)")      = testSpecific((_ :S[Int]).flatMap(flatMap)) //This is very slow
//	property(s"flatMap($name)")   = testSpecific((_ :S[Int]).flatMap(i => flatMap(i) to C))
//	property(s"flatten($typeS)")  = testSpecific((_ :S[C[Int]]).flatten) //todo: this is extremely slow
//	property("collect")           = testSpecific((_ :S[Int]).collect { case i if filter(i) => i })
//	property("concat")            = forAll { list :List[Int] => testSpecific((_ :S[Int]) ++ list) }

	override implicit def pairEvidence[A :ClassTag, B :ClassTag] :ClassTag[(A, B)] = implicitly[ClassTag[(A, B)]]
}






trait SugaredIterableProps[C[X] <: SugaredIterableOps[X, IterableOnce, C[X]],
                           S[X] >: C[X] <: IterableOps[X, IterableOnce, S[X]], E[_]]
	extends SpecificIterableProps[C, S, E]
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

	property("removed(Int)") = forAllIndices[Int] { (subject :C[Int], index :Int) =>
		val vec = subject to Vector
		if (index < 0 || index >= vec.length)
			subject.removed(index).throws[IndexOutOfBoundsException]
		else {
			val expect = vec.take(index) :++ (if (index == Int.MaxValue) Vector.empty else vec.drop(index + 1))
			val result = subject.removed(index)
			test(expect to S, result) lbl s"removed($index)"
		}
	}
	property("removed(Int, Int)") =	forAll { (subject :C[Int], from :Int, until :Int) =>
		val vec = subject.toVector
		val expect = vec.take(from) :++ vec.drop(math.max(from, until))
		val result = subject.removed(from, until)
		test(expect to S, result) lbl s"removed($from, $until)" lbl s"Input: $subject :${subject.localClassName}"
	}

	property("copyRangeToArray") = forAll { (s :C[Int]) => copyRangeToArrayProp(s to Vector, s) }


	protected override def props[T, F, M, FM](expect :S[T], result :C[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
	                                          filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                          mp :Map[T, M], evm :E[M], fmap :FlatMap[T, FM], evfm :E[FM]) :Seq[Prop] =
		super.props(expect, result) ++ Seq(
			"copyRangeToArray"  lbl_: copyRangeToArrayProp(if (hasOrder) expect else Vector from result, result),
			"removed(Int)"      lbl_: forAll { i :Int =>
                                        if (i < 0 || i >= expect.size)
	                                        result.removed(i).throws[IndexOutOfBoundsException]
                                        else {
	                                        val removed = expect.take(i) ++ expect.drop(i + 1)
	                                        compare(S[T] fromSpecific removed, result.removed(i)) lbl "expect: " + removed
                                        }
			                          },
			"removed(Int, Int)" lbl_: forAll { (i :Int, j :Int) =>
		                                val removed = expect.take(i) ++ expect.drop(math.max(i, j))
                                        compare(S[T] fromSpecific removed, result.removed(i, j))
			                          },
		)

	private def copyRangeToArrayProp[T :Arbitrary :ClassTag](ordered :Iterable[T], subject :C[T]) :Prop =
		forAll { (capacity :Short, start :Int, from :Int, len :Int) =>
			val cap       = capacity.toInt.abs
			val from0     = math.max(0, from)
			val specific1 = Array.const(cap)(Arbitrary.arbitrary[T].sample.get)
			val specific2 = specific1.clone
			val generic1  = specific1.map[Any](_.toString)
			val generic2  = generic1.clone()
			//For some strange reason the check for zero array size is not made before checking for negative start.
			val copy = len min ordered.size - from0 min cap - (start max 0) max 0
			if (start < 0 && (len min ordered.size - from0 max 0) > 0)
				(s"copyRangeToArray(Array[Int]|${specific2.length}|, $start, $from, $len)" lbl_:
					subject.copyRangeToArray(specific1, start, from, len).throws[IndexOutOfBoundsException]) &&
					(s"copyRangeToArray(Array[Any]|${generic2.length}|, $start, $from, $len)" lbl_:
						subject.copyRangeToArray(generic1, start, from, len).throws[IndexOutOfBoundsException])
			else {
				val clippedStart = if (copy == 0) 0 else start
				val copiedSpecific1 = ordered.drop(from).copyToArray(specific1, clippedStart, copy)
				val copiedSpecific2 = subject.copyRangeToArray(specific2, start, from, len)
				val copiedGeneric1  = ordered.drop(from).copyToArray(generic1, clippedStart, copy)
				val copiedGeneric2  = subject.copyRangeToArray(generic2, start, from, len)
				all(
					(copiedSpecific1 =? copiedSpecific2) && (
						ArraySeq.unsafeWrapArray(specific1) =? ArraySeq.unsafeWrapArray(specific2)
					) lbl s"copyRangeToArray(Array[Int]|${specific2.length}|, $start, $from, $len)",
					(copiedGeneric1 =? copiedGeneric2) && (
						ArraySeq.unsafeWrapArray(specific1) =? ArraySeq.unsafeWrapArray(specific2)
					) lbl s"copyRangeToArray(Array[Any]|${generic2.length}|, $start, $from, $len)"
				)
			}
		}
}





//object SeqProperties extends Properties("immutable.Seq builder implementations") {
//
//	type A = Int
//
//	property("Seq builder stateful testing") = new SeqBuilderStateProperties(Seq.newBuilder[A]).property()
//	property("List builder stateful testing") = new SeqBuilderStateProperties(List.newBuilder[A]).property()
//	property("ArraySeq builder stateful testing") = new SeqBuilderStateProperties(ArraySeq.newBuilder[A]).property()
//	property("Queue builder stateful testing") = new SeqBuilderStateProperties(Queue.newBuilder[A]).property()
//	property("IndexedSeq builder stateful testing") = new SeqBuilderStateProperties(IndexedSeq.newBuilder[A]).property()
//	property("Stream builder stateful testing") = new SeqBuilderStateProperties(Stream.newBuilder[A]).property()
//	property("Vector builder stateful testing") = new SeqBuilderStateProperties(Vector.newBuilder[A]).property()
//	property("WrappedString builder stateful testing") = new SeqBuilderStateProperties(WrappedString.newBuilder).property()
//}



//todo: test the builder for all Iterables
class BuilderProp[A, To <: Iterable[A]](newBuilder : => Builder[A, To])(implicit arbA :Arbitrary[A])
	extends Commands
{
	override type State = Seq[A]
	override type Sut = mutable.Builder[A, To]

	import Gen._

	val commandGen = Gen.oneOf(
		const(Clear),
		const(Result),
		choose(-1, 10000).map(SizeHint(_)),
		arbA.arbitrary.map(a => AddOne(a)),
		listOf(arbA.arbitrary).map(AddAll(_)),
		buildable[Vector[A]]().map(AddAll(_)),
		buildable[Set[A]]().map(AddAll(_))
	)

	override def genInitialState :Gen[State] = newBuilder.result().toSeq

	override def canCreateNewSut(newState :State, initSuts :scala.Iterable[State], runningSuts :scala.Iterable[Sut]) = true
	override def newSut(state :State) :mutable.Builder[A, To] = newBuilder.addAll(state)
	override def destroySut(sut :Sut) :Unit = ()
	override def initialPreCondition(state :State) = state.isEmpty

	override def genCommand(state :State) :Gen[Command] = commandGen

	case object Clear extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) :Unit = sut.clear()
		override def nextState(state :State) = Vector.empty
		override def preCondition(state :State) = true
	}

	case object Result extends Command {
		override type Result = State
		override def postCondition(state :State, result :Try[Result]) = result.map(_.toVector) == Success(state.toVector)
		override def run(sut :Sut) = sut.result().toVector
		override def nextState(state :State) = state
		override def preCondition(state :State) = true
	}

	case class SizeHint(size :Int) extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) = sut.sizeHint(size)
		override def nextState(state :State) = state
		override def preCondition(state :State) = true
	}

	case class AddOne(elem :A) extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) = sut.addOne(elem)
		override def nextState(state :State) = state.appended(elem)
		override def preCondition(state :State) = true
	}

	case class AddAll(elems :scala.collection.immutable.Iterable[A]) extends UnitCommand {
		override def postCondition(state :State, success :Boolean) = success
		override def run(sut :Sut) = sut.addAll(elems)
		override def nextState(state :State) = state.appendedAll(elems)
		override def preCondition(state :State) = true
	}
}






/*
class SeqTest extends AllocationTest {

	@Test def `t9936 indexWhere`() :Unit = {
		assertEquals(2, "abcde".indexOf('c', -1))
		assertEquals(2, "abcde".indexOf('c', -2))
		assertEquals(2, "abcde".toVector.indexOf('c', -1))
		assertEquals(2, "abcde".toVector.indexOf('c', -2))
		assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -1))
		assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -2))
	}

	@Test def combinations() :Unit = {
		assertEquals(List(Nil), Nil.combinations(0).toList)
		assertEquals(Nil, Nil.combinations(1).toList)
		assertEquals(List(List(1, 2), List(1, 3), List(2, 3)), List(1, 2, 3).combinations(2).toList)
		assertEquals(List(List(1, 2, 3)), List(1, 2, 3).combinations(3).toList)
	}

	@Test
	def hasCorrectDistinct() :Unit = {
		assertEquals(Seq(1, 2, 3, 4, 5), Seq(1, 1, 2, 3, 3, 3, 4, 5, 5).distinct)
	}

	@Test
	def hasCorrectDistinctBy() :Unit = {
		val result = Seq("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").distinctBy(_.length)

		assertEquals(Seq("a", "aa", "aaa", "bbbb"), result)
	}

	@Test
	def hasCorrectIndexOfSlice() :Unit = {
		assertEquals(0, Vector(0, 1).indexOfSlice(List(0, 1)))
		assertEquals(0, Vector(0, 1).indexOfSlice(Vector(0, 1)))
		assertEquals(1, Vector(0, 1, 2, 0, 1, 2).indexOfSlice(Vector(1, 2)))
		assertEquals(4, Vector(0, 1, 2, 0, 1, 2).indexOfSlice(Vector(1, 2), from = 2))
		assertEquals(-1, List(0, 1).indexOfSlice(List(1, 2)))
	}

	@Test
	def hasCorrectLastIndexOfSlice() :Unit = {
		assertEquals(0, Vector(0, 1).lastIndexOfSlice(List(0, 1)))
		assertEquals(0, Vector(0, 1).lastIndexOfSlice(Vector(0, 1)))
		assertEquals(4, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2)))
		assertEquals(1, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2), end = 3))
		assertEquals(-1, List(0, 1).lastIndexOfSlice(List(1, 2)))
	}

	@Test
	def hasCorrectDiff() :Unit = {
		val s1 = Seq(1, 2, 3, 4, 5)
		val s2 = Seq(1, 3, 5, 7, 9)

		assertEquals(Seq(2, 4), s1.diff(s2))
	}

	@Test
	def hasCorrectIntersect() :Unit = {
		val s1 = Seq(1, 2, 3, 4, 5)
		val s2 = Seq(1, 3, 5, 7, 9)

		assertEquals(Seq(1, 3, 5), s1.intersect(s2))
	}

	@deprecated("Tests deprecated API", since = "2.13")
	@Test
	def unionAlias() :Unit = {
		val s1 = Seq(1, 2, 3)
		val s2 = Seq(4, 5, 6)
		assertEquals(s1.concat(s2), s1.union(s2))
	}

	@Test
	def testLengthIs() :Unit = {
		val s = Seq(1, 2, 3)
		assert(s.lengthIs <= 3)
		assert(s.lengthIs == 3)
		assert(s.lengthIs >= 3)
		assert(s.lengthIs <= 4)
		assert(s.lengthIs < 4)
		assert(s.lengthIs != 4)
		assert(s.lengthIs >= 2)
		assert(s.lengthIs > 2)
		assert(s.lengthIs != 2)
	}

	@Test def emptyNonAllocating() :Unit = {
		nonAllocating(Seq.empty)
		nonAllocating(Seq())
	}

	@Test def smallSeqAllocation() :Unit = {
		if (CompileTime.versionNumberString == "2.13.2") return
		exactAllocates(Sizes.list * 1, "collection seq  size 1")(Seq("0"))
		exactAllocates(Sizes.list * 2, "collection seq  size 2")(Seq("0", "1"))
		exactAllocates(Sizes.list * 3, "collection seq  size 3")(Seq("0", "1", ""))
		exactAllocates(Sizes.list * 4, "collection seq  size 4")(Seq("0", "1", "2", "3"))
		exactAllocates(Sizes.list * 5, "collection seq  size 5")(Seq("0", "1", "2", "3", "4"))
		exactAllocates(Sizes.list * 6, "collection seq  size 6")(Seq("0", "1", "2", "3", "4", "5"))
		exactAllocates(Sizes.list * 7, "collection seq  size 7")(Seq("0", "1", "2", "3", "4", "5", "6"))
	}

	@Test def largeSeqAllocation() :Unit = {
		def expected(n :Int) = Sizes.list * n + Sizes.wrappedRefArray(n) + Sizes.wrappedRefArrayIterator
		exactAllocates(expected(10), "collection seq size 10")(
			Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
		exactAllocates(expected(20), "collection seq size 20")(
			Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
	}
}
*/
