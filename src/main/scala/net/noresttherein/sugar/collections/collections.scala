package net.noresttherein.sugar

import java.util.PrimitiveIterator

import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, SeqFactory, Stepper}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Buffer, IndexedBuffer}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{IArray, IArrayLike, IRefArray}
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

	/** A factory of sequences wrapping an arrays in a [[net.noresttherein.sugar.collections.RelayArray RelayArray]],
	  * if the latter is not on the classpath.
	  */
	private[sugar] final val RelayArrayFactory :Opt[ArrayLikeSliceFactory[IndexedSeq, IArrayLike]] =
		Opt.guard {
			val factoryClass = Class.forName(RelayArrayClassName + "Internals$")
			factoryClass.getField("MODULE$").get(null).asInstanceOf[ArrayLikeSliceFactory[IndexedSeq, IArrayLike]]
		}

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

	private[collections] val TemporaryBuffer :BufferFactory[IndexedBuffer] = MatrixBuffer.untagged


	private def loadSeqFactory[CC[X] <: collection.Seq[X]]
	                          (collectionClassName :String)(implicit tag :ClassTag[CC[Any]]) :SeqFactory[CC] =
	{
		val factory = try {
			val companionClass = Class.forName(collectionClassName + '$')
			val field = companionClass.getField("MODULE$")
			field.get(null)
		} catch {
			case cause :Exception => try {
				val factoryClass = Class.forName(collectionClassName)
				factoryClass.getConstructor().newInstance()
			} catch {
				case e :Exception =>
					throw e.addInfo(
						collectionClassName + " is neither a Scala object, nor does it have a default constructor"
					).suppress(cause)
			}
		}
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
