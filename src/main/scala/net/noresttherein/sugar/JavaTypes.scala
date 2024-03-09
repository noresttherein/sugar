package net.noresttherein.sugar

import scala.annotation.nowarn
import scala.collection.{IterableFactory, MapFactory, SortedIterableFactory, SortedMapFactory, SpecificIterableFactory}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.{ArrayStepper, IndexedSeqStepper, JavaIterator, JavaIteratorShape}
import net.noresttherein.sugar.extensions.{ArrayAsIterableOnceExtension, IterableOnceExtension, castTypeParamMethods}




/** Type aliases for ubiquitous Java types which have Scala counterparts sharing their name. */
@SerialVersionUID(Ver)
object JavaTypes { //todo: 'companion' objects with delegates to Java static methods.
	type JArray            = java.lang.reflect.Array

	type JVoid             = java.lang.Void
	type JBoolean          = java.lang.Boolean
	type JByte             = java.lang.Byte
	type JShort            = java.lang.Short
	type JInt              = java.lang.Integer
	type JLong             = java.lang.Long
	type JChar             = java.lang.Character
	type JFloat            = java.lang.Float
	type JDouble           = java.lang.Double

	type JBigDecimal       = java.math.BigDecimal
	type JBigInt           = java.math.BigInteger

	type JStringBuilder    = java.lang.StringBuilder
	@deprecated("Use collections.JavaIterator. The name of JIterator is too similar to Jterator.", "0.1")
	type JIterator[T]      = java.util.Iterator[T]
	@deprecated("Use collections.JavaIntIterator. The name of JIntIterator is too similar to IntJterator.", "0.1")
	type JIntIterator      = java.util.PrimitiveIterator.OfInt
	@deprecated("Use collections.JavaLongIterator. The name of JLongIterator is too similar to LongJterator.", "0.1")
	type JLongIterator     = java.util.PrimitiveIterator.OfLong
	@deprecated("Use collections.JavaDoubleIterator. The name of JDoubleIterator is too similar to DoubleJterator.", "0.1")
	type JDoubleIterator   = java.util.PrimitiveIterator.OfDouble
	type JCollection[T]    = java.util.Collection[T]
	type JList[T]          = java.util.List[T]
	type JArrayList[T]     = java.util.ArrayList[T]
	type JLinkedList[T]    = java.util.LinkedList[T]
	type JSet[T]           = java.util.Set[T]
	type JHashSet[T]       = java.util.HashSet[T]
	type JTreeSet[T]       = java.util.TreeSet[T]
	type JBitSet           = java.util.BitSet
	type JMap[K, V]        = java.util.Map[K, V]
	type JHashMap[K, V]    = java.util.HashMap[K, V]
	type JTreeMap[K, V]    = java.util.TreeMap[K, V]
	type JQueue[T]         = java.util.Queue[T]
	type JDeque[T]         = java.util.Deque[T]
	type JPriorityQueue[T] = java.util.PriorityQueue[T]

	@deprecated("Use collections.JavaIterator. The name of JIterator is too similar to Jterator.", "0.1")
	@nowarn("cat=deprecation")
	@SerialVersionUID(Ver)
	case object JIterator extends IterableFactory[JIterator] {
		override def from[A](source :IterableOnce[A]) :JIterator[A] = source.javaIterator
		override def empty[A] :JIterator[A] = JavaIterator.empty
		override def newBuilder[A] :Builder[A, JIterator[A]] = 
			Array.newBuilder(ClassTag.Any.castParam[A]).mapResult(JavaIterator.over(_))

		def over[T, I <: JIterator[_]](seq :collection.IndexedSeq[T])(implicit shape :JavaIteratorShape[T, I]) :I =
			IndexedSeqStepper(seq)(shape.stepperShape).javaIterator.asInstanceOf[I]

		def over[T, I <: JIterator[_]](array :Array[T])(implicit shape :JavaIteratorShape[T, I]) :I =
			ArrayStepper(array)(shape.stepperShape).javaIterator.asInstanceOf[I]

		def slice[T, I <: JIterator[_]](seq :collection.IndexedSeq[T], from :Int, until :Int)
		                               (implicit shape :JavaIteratorShape[T, I]) :I =
			IndexedSeqStepper.slice(seq, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]

		def slice[T, I <: JIterator[_]](array :Array[T], from :Int, until :Int)
		                               (implicit shape :JavaIteratorShape[T, I]) :I =
			ArrayStepper.slice(array, from, until)(shape.stepperShape).javaIterator.asInstanceOf[I]
	}

	@deprecated("Use collections.JavaIntIterator. The name of JIntIterator is too similar to IntJterator.", "0.1")
	@nowarn("cat=deprecation")
	@SerialVersionUID(Ver)
	object JIntIterator extends SpecificIterableFactory[Int, JIntIterator] {
		override def empty :JIntIterator = JavaIterator.ofInt()
		override def newBuilder :Builder[Int, JIntIterator] = Array.newBuilder[Int].mapResult(JavaIterator.ofInt(_))
		override def fromSpecific(it :IterableOnce[Int]) :JIntIterator = it.javaIterator
	}

	@deprecated("Use collections.JavaIntIterator. The name of JIntIterator is too similar to IntJterator.", "0.1")
	@nowarn("cat=deprecation")
	@SerialVersionUID(Ver)
	object JLongIterator extends SpecificIterableFactory[Long, JLongIterator] {
		override def empty :JLongIterator = JavaIterator.ofLong()
		override def newBuilder :Builder[Long, JLongIterator] = Array.newBuilder[Long].mapResult(JavaIterator.ofLong(_))
		override def fromSpecific(it :IterableOnce[Long]) :JLongIterator = it.javaIterator
	}

	@deprecated("Use collections.JavaIntIterator. The name of JIntIterator is too similar to IntJterator.", "0.1")
	@nowarn("cat=deprecation")
	@SerialVersionUID(Ver)
	object JDoubleIterator extends SpecificIterableFactory[Double, JDoubleIterator] {
		override def empty :JDoubleIterator = JavaIterator.ofDouble()
		override def newBuilder :Builder[Double, JDoubleIterator] =
			Array.newBuilder[Double].mapResult(JavaIterator.ofDouble(_))
		override def fromSpecific(it :IterableOnce[Double]) :JDoubleIterator = it.javaIterator
	}
	
	@SerialVersionUID(Ver)
	case object JCollection extends IterableFactory.Delegate[JCollection](JList)

	@SerialVersionUID(Ver)
	case object JList extends IterableFactory.Delegate[JList](JArrayList) {
		override def empty[A] :JList[A] = java.util.List.of()
	}

	@SerialVersionUID(Ver)
	case object JArrayList extends IterableFactory[JArrayList] {
		override def from[A](source :IterableOnce[A]) :JArrayList[A] = source match {
			case it :Iterable[A] => new JArrayList(it.asJavaCollection)
			case _ => (new JArrayList[A] /: source.iterator) { (res, elem) => res add elem; res } 
		}
		override def empty[A] :JArrayList[A] = new JArrayList()

		override def newBuilder[A] :Builder[A, JArrayList[A]] = new JCollectionBuilder(new JArrayList[A])
	}
	
	@SerialVersionUID(Ver)
	case object JLinkedList extends IterableFactory[JLinkedList] {
		override def from[A](source :IterableOnce[A]) :JLinkedList[A] = source match {
			case it :Iterable[A] => new JLinkedList(it.asJavaCollection)
			case _ => (new JLinkedList[A] /: source.iterator) { (res, elem) => res add elem; res } 
		}
		override def empty[A] :JLinkedList[A] = new JLinkedList()

		override def newBuilder[A] :Builder[A, JLinkedList[A]] = new JCollectionBuilder(new JLinkedList[A])
	}

	@SerialVersionUID(Ver)
	case object JQueue extends IterableFactory.Delegate[JQueue](JLinkedList)

	@SerialVersionUID(Ver)
	case object JDeque extends IterableFactory.Delegate[JDeque](JLinkedList)

	@SerialVersionUID(Ver)
	case object JPriorityQueue extends SortedIterableFactory[JPriorityQueue] {
		override def from[A :Ordering](source :IterableOnce[A]) :JPriorityQueue[A] =
			(new JPriorityQueue[A](Ordering[A]) /: source) { (res, elem) => res.add(elem); res }

		override def empty[A :Ordering] :JPriorityQueue[A] = new JPriorityQueue(Ordering[A])

		override def newBuilder[A :Ordering] :Builder[A, JPriorityQueue[A]] =
			new JCollectionBuilder(new JPriorityQueue(Ordering[A]))
	}

	@SerialVersionUID(Ver)
	case object JSet extends IterableFactory.Delegate[JSet](JHashSet) {
		override def empty[A] :JSet[A] = java.util.Set.of()
	}

	@SerialVersionUID(Ver)
	case object JHashSet extends IterableFactory[JHashSet] {
		override def from[A](source :IterableOnce[A]) :JHashSet[A] = source match {
			case it :Iterable[A] => new JHashSet(it.asJavaCollection)
			case _ => (new JHashSet[A] /: source.iterator) { (res, elem) => res add elem; res } 
		}
		override def empty[A] :JHashSet[A] = new JHashSet()

		override def newBuilder[A] :Builder[A, JHashSet[A]] = new JCollectionBuilder(new JHashSet[A])
	}

	@SerialVersionUID(Ver)
	case object JTreeSet extends SortedIterableFactory[JTreeSet] {
		override def from[E :Ordering](it :IterableOnce[E]) :JTreeSet[E] = new JTreeSet(Ordering[E])
		override def empty[A :Ordering] :JTreeSet[A] = new JTreeSet(Ordering[A])
		override def newBuilder[A :Ordering] :Builder[A, JTreeSet[A]] = new JCollectionBuilder(new JTreeSet(Ordering[A]))
	}

	@SerialVersionUID(Ver)
	case object JBitSet extends SpecificIterableFactory[Int, JBitSet] {
		override def fromSpecific(it :IterableOnce[Int]) :JBitSet =
			(new JBitSet /: it.toBasicOps) { (res, i) => res.set(i); res }

		override def empty :JBitSet = new JBitSet

		override def newBuilder :Builder[Int, JBitSet] = new ReusableBuilder[Int, JBitSet] {
			private[this] var res = new JBitSet
			override def addOne(elem :Int) = { res.set(elem); this }
			override def clear() :Unit = if (res != null) res.clear()
			override def result() :JBitSet = { val set = res; res = null; set }
		}
	}

	@SerialVersionUID(Ver)
	case object JMap extends MapFactory.Delegate[JMap](JHashMap) {
		override def empty[K, V] :JMap[K, V] = java.util.Map.of()
	}

	@SerialVersionUID(Ver)
	case object JHashMap extends MapFactory[JHashMap] {
		override def from[K, V](it :IterableOnce[(K, V)]) :JHashMap[K, V] =
			(new JHashMap[K, V] /: it.toBasicOps) { (res, kv) => res.put(kv._1, kv._2); res }

		override def empty[K, V] :JHashMap[K, V] = new JHashMap()

		override def newBuilder[K, V] :Builder[(K, V), JHashMap[K, V]] =
			new ReusableBuilder[(K, V), JHashMap[K, V]] {
				private[this] var res = new JHashMap[K, V]
				override def addOne(elem :(K, V)) = { res.put(elem._1, elem._2); this }
				override def clear() :Unit = if (res != null) res.clear()
				override def result() = { val map = res; res = null; map }
			}
	}

	@SerialVersionUID(Ver)
	case object JTreeMap extends SortedMapFactory[JTreeMap] {
		override def from[K :Ordering, V](it :IterableOnce[(K, V)]) :JTreeMap[K, V] =
			(new JTreeMap[K, V](Ordering[K]) /: it.toBasicOps) { (res, kv) => res.put(kv._1, kv._2); res }

		override def empty[K :Ordering, V] :JTreeMap[K, V] = new JTreeMap(Ordering[K])

		override def newBuilder[K :Ordering, V] :Builder[(K, V), JTreeMap[K, V]] =
			new ReusableBuilder[(K, V), JTreeMap[K, V]] {
				private[this] var res = new JTreeMap[K, V](Ordering[K])
				override def addOne(elem :(K, V)) = { res.put(elem._1, elem._2); this }
				override def clear() :Unit = if (res != null) res.clear()
				override def result() = { val map = res; res = null; map }
			}
	}


	private class JCollectionBuilder[A, C[X] >: Null <: JCollection[X]](private[this] var res :C[A])
		extends Builder[A, C[A]]
	{
		override def clear() :Unit = if (res != null) res.clear()
		override def result() = { val c = res; res = null; c }
		override def addOne(elem :A) = { res add elem; this }
	}



	/** Implicit unboxing of java wrappers of primitive types (`Integer`, `java.lang.Long`, etc.) to Scala value types. */
	object equivalence {
		@inline implicit def javaBooleanIsBoolean :JBoolean =:= Boolean = ev.asInstanceOf[JBoolean =:= Boolean]
		@inline implicit def javaByteIsByte       :JByte =:= Byte = ev.asInstanceOf[JByte =:= Byte]
		@inline implicit def javaShortIsShort     :JShort =:= Short = ev.asInstanceOf[JShort =:= Short]
		@inline implicit def javaCharIsChar       :JChar =:= Char = ev.asInstanceOf[JChar =:= Char]
		@inline implicit def javaIntIsInt         :JInt =:= Int = ev.asInstanceOf[JInt =:= Int]
		@inline implicit def javaLongIsLong       :JLong =:= Long = ev.asInstanceOf[JLong =:= Long]
		@inline implicit def javaFloatIsFloat     :JFloat =:= Float = ev.asInstanceOf[JFloat =:= Float]
		@inline implicit def javaDoubleIsDouble   :JDouble =:= Double = ev.asInstanceOf[JDouble =:= Double]

		@inline implicit def booleanIsJavaBoolean :Boolean =:= JBoolean = ev.asInstanceOf[Boolean =:= JBoolean]
		@inline implicit def byteIsJavaByte       :Byte =:= JByte = ev.asInstanceOf[Byte =:= JByte]
		@inline implicit def shortIsJavaShort     :Short =:= JShort = ev.asInstanceOf[Short =:= JShort]
		@inline implicit def charIsJavaChar       :Char =:= JChar = ev.asInstanceOf[Char =:= JChar]
		@inline implicit def intIsJavaInt         :Int =:= JInt = ev.asInstanceOf[Int =:= JInt]
		@inline implicit def longIsJavaLong       :Long =:= JLong = ev.asInstanceOf[Long =:= JLong]
		@inline implicit def floatIsJavaFloat     :Float =:= JFloat = ev.asInstanceOf[Float =:= JFloat]
		@inline implicit def doubleIsJavaDouble   :Double =:= JDouble = ev.asInstanceOf[Double =:= JDouble]

		private[this] val ev :Any =:= Any = implicitly[Any=:=Any]
	}
}
