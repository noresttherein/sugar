package net.noresttherein.sugar

import java.util.PrimitiveIterator

import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, SeqFactory, Stepper}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Buffer, IndexedBuffer}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{ArrayLike, IArray, IArrayLike, IRefArray}
import net.noresttherein.sugar.matching.MatchPattern
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions._




//consider: extending imports
package object collections {
	private[collections] final val Ver = 1L

	final val ElemTypes = Specializable.AllNumeric

	/** An empty `LazyList` for more convenient concatenation. */
	val LazyNil :LazyList[Nothing] = LazyList.empty


	type JavaIterator[E]    = java.util.Iterator[E]
	type JavaIntIterator    = PrimitiveIterator.OfInt
	type JavaLongIterator   = PrimitiveIterator.OfLong
	type JavaDoubleIterator = PrimitiveIterator.OfDouble

	/** An opaque wrapper over a Java iterator, possibly one of the [[java.util.PrimitiveIterator PrimitiveIterator]]
	  * subclasse, exposing iterator-like API, returning elements of type `E`. Specialized subtypes are available
	  * for all built in value types, allowing iteration over them without boxing or explicit casting/convnersion.
	  * It follows, that the type of the elements of this jterator may differ from the element type of the underlying
	  * [[net.noresttherein.sugar.collections.JavaIterator JavaIterator]]. The extension methods available through
	  * `net.noresttherein.sugar.collections.extensions.`[[net.noresttherein.sugar.collections.extensions.JTeratorExtension JteratorExtension]]
	  * and, in particular, extension methods for its specialized subtypes, convert `Int`, `Long`, or `Double`
	  * into one of the types with lower precision.
	  * @see [[net.noresttherein.sugar.collections.ByteJterator ByteJterator]]
	  * @see [[net.noresttherein.sugar.collections.CharJterator CharJterator]]
	  * @see [[net.noresttherein.sugar.collections.ShortJterator ShortJterator]]
	  * @see [[net.noresttherein.sugar.collections.IntJterator IntJterator]]
	  * @see [[net.noresttherein.sugar.collections.LongJterator LongJterator]]
	  * @see [[net.noresttherein.sugar.collections.FloatJterator FloatJterator]]
	  * @see [[net.noresttherein.sugar.collections.DoubleJterator DoubleJterator]]
	  * @see [[net.noresttherein.sugar.collections.BooleanJterator BooleanJterator]]
	  */
	type Jterator[+E]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]],
	  * with an API exposed as extension methods imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.IntJteratorExtension IntJteratorExtension]].
	  * It exists for uniformity with other ''jterators'', and to replace ''nextInt()'' with the standard `next()`,
	  * which helps avoiding calling of a boxing method by accident.
	  */
	type IntJterator <: Jterator[Int]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfLong PrimitiveIterator.OfLong]],
	  * with an API exposed as extension methods imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.LongJteratorExtension LongJteratorExtension]].
	  * It exists for uniformity with other ''jterators'', and to replace ''nextLong()'' with the standard `next()`,
	  * which helps avoiding calling of a boxing method by accident.
	  */
	type LongJterator <: Jterator[Long]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfDouble PrimitiveIterator.OfDouble]],
	  * with an API exposed as extension methods imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.DoubleJteratorExtension DoubleJteratorExtension]].
	  * It exists for uniformity with other ''jterators'', and to replace ''nextDouble()'' with the standard `next()`,
	  * which helps avoiding calling of a boxing method by accident.
	  */
	type DoubleJterator <: Jterator[Double]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfDouble PrimitiveIterator.OfDouble]],
	  * providing extension methods allowing to treat it as an `Iterator[Float]`, imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.FloatJteratorExtension FloatJteratorExtension]].
	  */
	type FloatJterator <: Jterator[Float]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]],
	  * providing extension methods allowing to treat it as an `Iterator[Short]`, imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.ShortJteratorExtension ShortJteratorExtension]].
	  */
	type ShortJterator <: Jterator[Short]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]],
	  * providing extension methods allowing to treat it as an `Iterator[Char]`, imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.CharJteratorExtension CharJteratorExtension]].
	  */
	type CharJterator <: Jterator[Char]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]],
	  * providing extension methods allowing to treat it as an `Iterator[Byte]`, imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.ByteJteratorExtension ByteJteratorExtension]].
	  */
	type ByteJterator <: Jterator[Byte]

	/** An opaque alias for [[java.util.PrimitiveIterator.OfInt PrimitiveIterator.OfInt]],
	  * providing extension methods allowing to treat it as an `Iterator[Boolean]`, imported from
	  * [[net.noresttherein.sugar.collections.extensions extensions]].[[net.noresttherein.sugar.collections.extensions.ByteJteratorExtension ByteJteratorExtension]].
	  * Zero values produced by the underlying iterator are returned as `false`, while any non zero value
	  * is returned as `true`.
	  */
	type BooleanJterator <: Jterator[Boolean]

	/** An opaque alias for [[java.util.Iterator]] returning a reference type, treating it in a uniform manner
	  * with jterators representing primitive iterators.
	  */
	type RefJterator[+T <: AnyRef] <: Jterator[T]

	type SplitStepper[+X]    = Stepper[X] with EfficientSplit
	type AnySplitStepper[+X] = AnyStepper[X] with EfficientSplit
	type IntSplitStepper     = IntStepper with EfficientSplit
	type LongSplitStepper    = LongStepper with EfficientSplit
	type DoubleSplitStepper  = DoubleStepper with EfficientSplit


	val KnownSize :MatchPattern[IterableOnce[_], Int] =
		MatchPattern { elems :IterableOnce[_] => Opt.satisfying(elems.knownSize)(_ >= 0) }


	final val defaultIndexedSeqProperty = "net.noresttherein.sugar.collections.IndexedSeqFactory"
	final val defaultArraySeqProperty   = "net.noresttherein.sugar.collections.ArraySeqFactory"
	final val defaultBufferProperty     = "net.noresttherein.sugar.collections.BufferFactory"


	private final val RelayArrayClassName = "net.noresttherein.sugar.collections.RelayArray"

	//todo: move all package protected methods and fields to another object for binary compatibility
	// (or make them private/public).
	/** A factory of sequences wrapping an arrays in a [[net.noresttherein.sugar.collections.RelayArray RelayArray]],
	  * if the latter is not on the classpath.
	  */
	private[sugar] final val RelayArrayFactory :Opt[ArrayLikeSliceFactory[IndexedSeq, IArrayLike]] =
		Opt.guard {
			val factoryClass = Class.forName(RelayArrayClassName + "Internals$")
			factoryClass.getField("MODULE$").get(null).asInstanceOf[ArrayLikeSliceFactory[IndexedSeq, IArrayLike]]
		}

	//consider: nothing really prevents us from making it ArrayLikeSliceFactory
	final val DefaultArraySeq :ArrayLikeSliceWrapper[IndexedSeq, IArrayLike] =
		arrayWrapperFromProperty(defaultArraySeqProperty) orElse RelayArrayFactory getOrElse IArrayLikeSlice

	/** The default `IndexedSeq` implementation used by the library. */
	private[collections] val DefaultIndexedSeq :SeqFactory[IndexedSeq] =
		seqFactoryFromProperty[IndexedSeq](defaultIndexedSeqProperty) orElse RelayArrayFactory getOrElse IndexedSeq

	/** An `IndexedSeq` used as a temporary buffer when a collection method cannot be implemented for a particular
	  * collection type, for example when traversing in reverse on an `Iterator` or a `LinearSeq`.
	  * Equal to [[net.noresttherein.sugar.collections.RelayArray RelayArray]] if available, or `ArraySeq.untagged`
	  * otherwise.
	  */
	private[collections] val TemporaryIndexedSeq :SeqFactory[IndexedSeq] =
		RelayArrayFactory getOrElse ArraySeq.untagged

	val DefaultBuffer :BufferFactory[Buffer] =
		bufferFactoryFromProperty(defaultBufferProperty) getOrElse MatrixBuffer.untagged

	//todo: see if we can drop the IndexedBuffer bound and replace usages of TemporaryBuffer with DefaultBuffer
	private[collections] val TemporaryBuffer :BufferFactory[IndexedBuffer] = MatrixBuffer.untagged


	private def loadObject(className :String) :Any =
		try {
			val companionClass = Class.forName(className + '$')
			val field          = companionClass.getField("MODULE$")
			field.get(null)
		} catch {
			case cause :Exception => try {
				val factoryClass = Class.forName(className)
				factoryClass.getConstructor().newInstance()
			} catch {
				case e :Exception =>
					throw e.addInfo(
						className + " is neither a Scala object, nor does it have a default constructor"
					).suppress(cause)
			}
		}

	private def loadSeqFactory[CC[X] <: collection.Seq[X]]
	                          (collectionClassName :String)(implicit tag :ClassTag[CC[Any]]) :SeqFactory[CC] =
	{
		val factory = loadObject(collectionClassName)
		val f = try factory.asInstanceOf[SeqFactory[CC]] catch {
			case e :ClassCastException =>
				throw new ClassCastException(
					collectionClassName + " object " + factory + " is not a SeqFactory: " + factory.className
				).initCause(e)
		}
		val what = f.from(1::2::Nil)
		if (!(tag.runtimeClass isAssignableFrom what.getClass))
			throw new IllegalStateException(
				collectionClassName + " is not a factory for " + tag.runtimeClass.name +
					"; created a " + what.getClass.name + " instead."
			)
		f
	}

	private def seqFactoryFromProperty[CC[X] <: collection.Seq[X]]
	                                  (property :String)(implicit tag :ClassTag[CC[Any]]) :Opt[SeqFactory[CC]] =
		Opt(System.getProperty(property)).map { className =>
			try loadSeqFactory[CC](className) catch {
				case e :Exception =>
					throw e.addInfo("Property " + property +
						" does not denote a SeqFactory[" + tag.runtimeClass.localName + "]."
					)
			}
		}

	private def bufferFactoryFromProperty(property :String) :Opt[BufferFactory[Buffer]] =
		Opt(System.getProperty(property)).map { className =>
			try {
				loadSeqFactory[Buffer](className) match {
					case factory :BufferFactory[Buffer] => factory
					case ArrayBuffer => ArrayBufferFactory
					case factory => new BufferFactoryAdapter(factory)
				}
			} catch {
				case e :Exception => throw e.addInfo("Property " + property + " does not denote a SeqFactory[Buffer].")
			}
		}

	private def arrayWrapperFromProperty(property :String) :Opt[ArrayLikeSliceWrapper[IndexedSeq, IArrayLike]] =
		Opt(System.getProperty(property)) match {
			case Got("scala.collection.immutable.ArraySeq") =>
				Got(ArraySeqFactory.asInstanceOf[ArrayLikeSliceWrapper[IndexedSeq, IArrayLike]])
			case Got(className) =>
					try {
						val factory   = loadObject(className).asInstanceOf[ArrayLikeSliceWrapper[IndexedSeq, IArrayLike]]
						if (!factory.isImmutable)
							throw new IllegalStateException(
								"Default ArrayLikeSlicefactory is not immutable: " + factory + ": " + className + "."
							)
						val testInput = IArray(1, 2, 3, 4)
						val _         = factory.slice(testInput, 1, 3)
						Got(factory)
					} catch {
						case e :Exception =>
							throw e.addInfo("Property " + property + " does not denote an ArrayLikeSliceWrapper")
					}
			case _ => Lack
		}
}




package collections {

	sealed abstract class Mutability extends Serializable {
		def isMutable   :Boolean = false
		def isImmutable :Boolean = false
	}

	@SerialVersionUID(0)
	object Mutability {
		@SerialVersionUID(0) case object Immutable extends Mutability {
			override def isImmutable = true
		}
		@SerialVersionUID(0) case object Mutable extends Mutability {
			override def isMutable = true
		}
		@SerialVersionUID(0) case object Unspecified extends Mutability
	}

}
