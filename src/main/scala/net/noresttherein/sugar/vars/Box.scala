package net.noresttherein.sugar.vars

import java.lang.invoke.MethodHandles

import scala.annotation.{nowarn, tailrec}
import scala.Specializable.Args

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A mutable `Option`-like value holder. It expands on the base [[net.noresttherein.sugar.vars.InOut InOut]]
  * interface by allowing an instance to hold no value at all. Aside from the inherited mutator methods setting
  * the value of this variable, a box can be at any time [[net.noresttherein.sugar.vars.Box.clear cleared]],
  * emptying its contents. An empty box will throw a [[NoSuchElementException]] from any method accessing
  * its current value, be it directly through the [[net.noresttherein.sugar.vars.Box.value value]] property
  * and [[net.noresttherein.sugar.vars.InOut.get get]]/[[net.noresttherein.sugar.vars.Ref.apply apply]]`()` methods,
  * or indirectly by ''test-and-set'' and ''get-and-set'' operations and similar.
  *
  * While the default implementation is not thread safe, [[net.noresttherein.sugar.vars.InOut.testAndSet testAndSet]]
  * method is complemented with [[net.noresttherein.sugar.vars.Box.testAndSwap testAndSwap]], which generalize
  * the function of the former by accepting an `Option` or an [[net.noresttherein.sugar.vars.Opt Opt]].
  * @see [[net.noresttherein.sugar.vars.Mutable]]
  * @define Ref `Box`
  * @define ref boxed variable
  * @author Marcin Mościcki
  */
sealed trait Box[@specialized(SpecializedVars) T] extends InOut[T] with Serializable {
	/** Returns `false`. */
	final override def isFinal = false
//
//	/** Returns `true` if the box currently holds a value. In case of atomic access,
//	  * methods [[net.noresttherein.sugar.vars.InOut.value value]], [[net.noresttherein.sugar.vars.InOut.get get]]
//	  * and [[net.noresttherein.sugar.vars.Ref.apply apply]]`()`
//	  * will not throw an exception.
//	  */
//	@inline final override def nonEmpty :Boolean = !isEmpty

	/** Returns `false`. */
	final override def isFinalizable: Boolean = false

	/** Returns `false`. */
	final override def isConst :Boolean = false

	/** Same as [[net.noresttherein.sugar.vars.Box.nonEmpty nonEmpty]]. */
	@inline final override def isDefined :Boolean = !isEmpty

	/** Same as [[net.noresttherein.sugar.vars.Box.nonEmpty nonEmpty]]. */
	@inline final override def isDefinite :Boolean = !isEmpty

	/** Returns `1` if the `Box` contains a value or `0` otherwise. */
	@inline def size :Int = if (isEmpty) 0 else 1

	/** Same as [[net.noresttherein.sugar.vars.Ref.value value]]. */
	@inline final override def get :T = value

	/** Throws an [[UnsupportedOperationException]]. */
	override def const :T = throw new UnsupportedOperationException("Box has no constant value")

	override def toOption    :Option[T] = option
	override def constOption :Option[T] = None
	override def toOpt       :Opt[T] = opt
	override def constOpt    :Opt[T] = Lack
	override def toUnsure    :Unsure[T] = unsure
	override def constUnsure :Unsure[T] = Missing

	/** Replaces the contents of this box with the content of the given `Option`.
	  * It is a setter matching getter `this.`[[net.noresttherein.sugar.vars.Ref.option option]]
	  * and is equivalent to `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` content.get` if `content` is `Some`,
	  * or [[net.noresttherein.sugar.vars.Box.clear clear]]`()` if `content` is `None`.
	  */
	def option_=(content :Option[T]) :Unit = if (content.isDefined) value = content.get else clear()

	/** Replaces the contents of this box with the content of the given optional value.
	  * It is a setter method matching getter `this.`[[net.noresttherein.sugar.vars.Ref.opt opt]] and is equivalent to
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` content.`[[net.noresttherein.sugar.vars.Opt.get get]]
	  * if `content.`[[net.noresttherein.sugar.vars.Opt.isDefined isDefined]],
	  * or [[net.noresttherein.sugar.vars.Box.clear clear]]`()` if `content` is empty.
	  */
	def opt_=(content :Opt[T]) :Unit = if (content.isDefined) value = content.get else clear()

	/** Replaces the contents of this box with the content of the given unsure value.
	  * It is a setter method matching getter `this.`[[net.noresttherein.sugar.vars.Ref.unsure unsure]] and is equivalent
	  * to `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` content.`[[net.noresttherein.sugar.vars.Unsure.get get]]
	  * if `content.`[[net.noresttherein.sugar.vars.Unsure.isDefined isDefined]],
	  * or [[net.noresttherein.sugar.vars.Box.clear clear]]`()` if `content` is empty.
	  */
	def unsure_=(content :Unsure[T]) :Unit = if (content.isDefined) value = content.get else clear()

	/** Replaces the contents of this box with the content of the given value wrapper (possibly
	  * a [[net.noresttherein.sugar.vars.Unsure Unsure]] or another [[net.noresttherein.sugar.vars.Box Box]] instance).
	  * It is equivalent to
	  * `this.`[[net.noresttherein.sugar.vars.Box.unsure_= unsure]]` = content.`[[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]].
	  * The semantics of `toUnsure` differ depending on the runtime class of `content`;
	  * [[net.noresttherein.sugar.vars.Val Val]] subclasses will generally return
	  * the [[net.noresttherein.sugar.vars.Val.const final]] value of the `Val`, if it is already computed,
	  * with the exceptions of [[net.noresttherein.sugar.vars.Lazy Lazy]], which always return their value,
	  * initializing themselves if needed. Mutable [[net.noresttherein.sugar.vars.InOut InOut]] will, as a rule,
	  * return the current value, if available.
	  * Note that if `content` is not thread safe, the above calls can return corrupt, partially constructed objects.
	  * Any thread safe type however guarantees the atomicity of these operations.
	  */
	def :?=(content :Ref[T]) :Unit =
		if (content.isSpecialized) unsure = content.toUnsure //Unsure cheaper than Opt
		else opt = content.opt //use content.opt for atomicity (at least if content is thread safe)

	/** Replaces the contents of this box with the content of the given optional value.
	  * It is equivalent to
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` content.get`
	  * if `content` is `Some`, or [[net.noresttherein.sugar.vars.Box.clear clear]]`()` if `content` is `None`.
	  */
	@inline final def reset(content :Option[T]) :Unit = option = content

	/** Replaces the contents of this box with the content of the given optional value.
	  * It is equivalent to
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` content.`[[net.noresttherein.sugar.vars.Opt.get get]]
	  * if `content.`[[net.noresttherein.sugar.vars.Opt.isDefined isDefined]],
	  * or [[net.noresttherein.sugar.vars.Box.clear clear]]`()` if `content` is empty.
	  */
	@inline final def reset(content :Opt[T]) :Unit = opt = content

	/** Replaces the contents of this box with the content of the given unsure value.
	  * It is equivalent to
	  * `this `[[net.noresttherein.sugar.vars.InOut.:= :=]]` content.`[[net.noresttherein.sugar.vars.Unsure.get get]]
	  * if `content.`[[net.noresttherein.sugar.vars.Unsure.isDefined isDefined]],
	  * or [[net.noresttherein.sugar.vars.Box.clear clear]]`()` if `content` is empty.
	  */
	@inline final def reset(content: Unsure[T]) :Unit = unsure = content

	/** Replaces the contents of this box with the content of the given value wrapper (possibly
	  * a [[net.noresttherein.sugar.vars.Unsure Unsure]] or another [[net.noresttherein.sugar.vars.Box Box]] instance).
	  *
	  * It is equivalent to
	  * `this.`[[net.noresttherein.sugar.vars.Box.unsure_= unsure]]` = content.`[[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]].
	  * The semantics of `toUnsure` differ depending on the runtime class of `content`;
	  * [[net.noresttherein.sugar.vars.Val Val]] subclasses will return
	  * the [[net.noresttherein.sugar.vars.Val.const final]] value of the `Val`, if it is already computed,
	  * with the exceptions of [[net.noresttherein.sugar.vars.Lazy Lazy]], which always return their value,
	  * initializing themselves if needed. Mutable [[net.noresttherein.sugar.vars.InOut InOut]] will, as a rule,
	  * return the current value, if available.
	  * Note that if `content` is not thread safe, the above calls can return corrupt, partially constructed objects.
	  * Any thread safe type however guarantees the atomicity of these operations.
	  */
	def reset(content :Ref[T]) :Unit = this :?= content


	/** Returns the current value and removes it from this `Box`. If there is no value in this box,
	  * a `NoSuchElementException` will be thrown.
	  */
	@inline final def remove() :T = { val res = value; clear(); res }

	/** Empties the box, returning its current value, if any. */
	@inline final def removeOpt() :Opt[T] = { val res = opt; clear(); res }

	/** Empties the box, unreferencing the currently held value, if any. */
	def clear() :Unit

	/** Empties the box, providing it currently contains the specified value.
	  * Any references to the currently held value are freed.
	  * This method is useful primarily on a thread safe [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]],
	  * where it happens atomically.
	  *
	  * @param expect a test value which must equal the current value of the box.
	  * @return true ''iff'' the box was cleared by this method.
	  */
	def clear(expect :T) :Boolean =
		isDefined && value == expect && {
			clear(); true
		}

	/** Sets the content of this box, providing it is currently empty.
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  *
	  * @return true ''iff'' the box was empty and now contains the new value.
	  */
	def put(newValue :T) :Boolean = isEmpty && {
		value = newValue; true
	}


	/** Implements a ''get-and-set'' operation, which sets the value of this box to `content`
	  * ([[net.noresttherein.sugar.vars.Box.clear clearing]] it
	  * if `content.`[[Option.isEmpty isEmpty]]) and returns the value contained by
	  * this box before the method was called. In a synchronised context it is equivalent to
	  * {{{
	  *     def swap(content :Option[T]) = {
	  *         val current = this.option
	  *         if (content.isDefined) this.value = content.get
	  *         else this.clear()
	  *         current
	  *     }
	  * }}}
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  * @param content the new content for this variable: if non empty, `this.value` will be set to its value;
	  *                if empty, the variable will be cleared.
	  * @return the previous content of this variable, replaced by `content`.
	  */
	def swap(content :Option[T]) :Option[T] = { val res = option; option = content; res }

	/** Implements a ''get-and-set'' operation, which sets the value of this box to `content`
	  * ([[net.noresttherein.sugar.vars.Box.clear clearing]] it
	  * if `content.`[[net.noresttherein.sugar.vars.Opt.isEmpty isEmpty]]) and returns the value contained by
	  * this box before the method was called. In a synchronised context it is equivalent to
	  * {{{
	  *     def swap(content :Opt[T]) = {
	  *         val current = this.opt
	  *         if (content.isDefined) this.value = content.get
	  *         else this.clear()
	  *         current
	  *     }
	  * }}}
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  * @param content the new content for this variable: if non empty, `this.value` will be set to its value;
	  *                if empty, the variable will be cleared.
	  * @return the previous content of this variable, replaced by `content`.
	  */
	def swap(content :Opt[T]) :Opt[T] = { val res = opt; opt = content; res }

	/** Implements a ''get-and-set'' operation, which sets the value of this box to `content`
	  * ([[net.noresttherein.sugar.vars.Box.clear clearing]] it
	  * if `content.`[[net.noresttherein.sugar.vars.Unsure.isEmpty isEmpty]]) and returns the value contained by
	  * this box before the method was called. In a synchronised context it is equivalent to
	  * {{{
	  *     def swap(content :Unsure[T]) = {
	  *         val current = this.unsure
	  *         if (content.isDefined) this.value = content.get
	  *         else this.clear()
	  *         current
	  *     }
	  * }}}
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  * @param content the new content for this variable: if non empty, `this.value` will be set to its value;
	  *                if empty, the variable will be cleared.
	  * @return the previous content of this variable, replaced by `content`.
	  */
	def swap(content :Unsure[T]) :Unsure[T] = { val res = unsure; unsure = content; res }

	/** Replaces the contents of this box, if they are equal to `expect`, with the contents of `newValue`.
	  * For the purpose of this method, 'contents' are defined as
	  * `this.`[[net.noresttherein.sugar.vars.Box.option option]].
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.sugar.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.sugar.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.sugar.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.sugar.vars.Box.isEmpty]] if both arguments are empty.
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  */
	def testAndSwap(expect :Option[T], newValue :Option[T]) :Boolean = expect match {
		case Some(current) => newValue match {
			case Some(value) => testAndSet(current, value)
			case _ => clear(current)
		}
		case _ if newValue.isDefined => put(newValue.get)
		case _ => isEmpty //clear a box if it's empty
	}

	/** Replaces the contents of this box, if they are equal to `expect`, with the contents of `newValue`.
	  * For the purpose of this method, 'contents' are defined as `this.`[[net.noresttherein.sugar.vars.Box.opt opt]].
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.sugar.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.sugar.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.sugar.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.sugar.vars.Box.isEmpty]] if both arguments are empty.
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  */
	def testAndSwap(expect :Opt[T], newValue :Opt[T]) :Boolean = expect match {
		case Got(current) => newValue match {
			case Got(value) => testAndSet(current, value)
			case _ => clear(current)
		}
		case _ if newValue.isDefined => put(newValue.get)
		case _ => isEmpty //clear a box if it's empty
	}

	/** Replaces the contents of this box, if they are equal to `expect`, with the contents of `newValue`.
	  * For the purpose of this method, 'contents' are defined as `this.`[[net.noresttherein.sugar.vars.Box.unsure unsure]].
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.sugar.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.sugar.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.sugar.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.sugar.vars.Box.isEmpty]] if both arguments are empty.
	  * On a [[net.noresttherein.sugar.vars.VolatileBox VolatileBox]], this operation happens atomically.
	  */
	def testAndSwap(expect :Unsure[T], newValue :Unsure[T]) :Boolean = expect match {
		case Sure(current) => newValue match {
			case Sure(value) => testAndSet(current, value)
			case _ => clear(current)
		}
		case _ if newValue.isDefined => put(newValue.get)
		case _ => isEmpty //clear a box if it's empty
	}

	/** Replaces the contents of this box, if they are equal to `expect`, with the current contents of `newValue`.
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.sugar.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.sugar.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.sugar.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.sugar.vars.Box.isEmpty]] if both arguments are empty.
	  */
	def testAndSwap(expect :Ref[T], newValue :Ref[T]) :Boolean =
		if (expect.isSpecialized && newValue.isSpecialized)
			testAndSwap(expect.toUnsure, newValue.toUnsure)
		else
			testAndSwap(expect.toOpt, newValue.toOpt)


	override def equals(that :Any) :Boolean = that match {
		case other :Box[_] => (this eq other) || (opt == other.opt)
		case _ => false
	}
//	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Box[_]]
	override def hashCode :Int = opt.hashCode

	override def mkString :String = mkString("Box")
}




/** A factory of mutable value holders which can be empty.
  * @see [[net.noresttherein.sugar.vars.Box!]]
  */
@SerialVersionUID(Ver)
object Box {
	/** Creates a new, empty box. The returned instance is not thread safe. */
	def apply[@specialized(SpecializedVars) T] :Box[T] = new Plain

	/** Creates a new box, initialized with the given value. The returned instance is not thread safe. */
	def apply[@specialized(SpecializedVars) T](value :T) :Box[T] = {
		val box = new Plain[T]
		box := value
		box
	}

	/** Creates a new, empty box. The method is exactly equivalent to [[net.noresttherein.sugar.vars.Box.apply Box]]`[T]`,
	  * but is more readable than `Box.apply` if the type parameter is inferred.
	  */
	def empty[@specialized(SpecializedVars) T] :Box[T] = new Plain


	@inline def unapply[@specialized(SpecializedVars) T](box :Box[T]) :Box[T] = box



	/** A non-synchronized `Box` implementation. */
	@SerialVersionUID(Ver)
	private class Plain[@specialized(SpecializedVars) T] extends Box[T] {
		@nowarn private[this] var nullVal :T = _//the default value used to clear x so we don't keep a reference to the old value
		private[this] var x :T = _
		private[this] var full :Boolean = false

		override def isEmpty :Boolean = !full

		override def value :T =
			if (full) x else throw new NoSuchElementException("Box()")

		override def value_=(newValue :T) :Unit = {
			x = newValue
			full = true
		}
		override def option :Option[T] = if (full) Some(x) else None
		override def opt :Opt[T] = if (full) Got(x) else Lack
		override def unsure :Unsure[T] = if (full) Sure(x) else Missing

		override def clear() :Unit = { full = false; x = nullVal }

		override def testAndSet(expect :T, newValue :T) :Boolean =
			(full && x == expect) && { x = newValue; true }

		override def isSpecialized :Boolean = getClass != classOf[Plain[_]]
	}
}






/** A thread safe implementation of [[net.noresttherein.sugar.vars.Box Box]], with `@volatile` memory access semantics
  * and atomic test-and-set operations like in [[net.noresttherein.sugar.vars.Volatile Volatile]]. All synchronisation
  * is non-suspending (spin locks) and features a lower overhead than monitor synchronization, but is not optimized
  * for high-contention usage scenarios.
  * @define Ref `VolatileBox`
  */
sealed trait VolatileBox[@specialized(SpecializedVars) T] extends Box[T]



@SerialVersionUID(Ver)
object VolatileBox {
	/** Creates a new, empty box. The instance is thread safe with the same memory access semantics
	  * as a [[net.noresttherein.sugar.vars.Volatile Volatile]] variable. All operations are atomic.
	  */
	def apply[@specialized(SpecializedVars) T] :VolatileBox[T] = {
		var res :VolatileBox[T] = new OfVal
		if (res.getClass == unspecializedClass)
			res = new OfRef[T]
		res
	}

	/** Creates a new, atomic, box, initialized with the given value. The instance is thread safe
	  * with the same memory access semantics as a [[net.noresttherein.sugar.vars.Volatile Volatile]] variable.
	  * All operations are atomic.
	  */
	def apply[@specialized(SpecializedVars) T](value :T) :VolatileBox[T] = {
		val res = VolatileBox[T]
		res.value = value
		res
	}

	/** Creates a new, empty atomic box. The method is exactly equivalent
	  * to [[net.noresttherein.sugar.vars.VolatileBox.apply Box]]`[T]`, but is more readable than `VolatileBox.apply`
	  * if the type parameter is inferred.
	  */
	@inline def empty[@specialized(SpecializedVars) T] :VolatileBox[T] = VolatileBox[T]


	private final val Empty = 0
	private final val Full = 1
	private final val Locked = 2

	private val ValStateField = MethodHandles.lookup.findVarHandle(classOf[OfVal[_]], "state", Integer.TYPE)
	private val RefOptField = MethodHandles.lookup.findVarHandle(classOf[OfRef[_]], "x", classOf[Any])
	private val unspecializedClass = classOf[OfVal[Any]]


	//todo: see if there is actually any performance benefit over OfRef
	/** A non-boxing implementation of `VolatileBox`, suitable for value types.
	  * [[net.noresttherein.sugar.vars.VolatileBox.OfRef OfRef]] should be used instead
	  * for reference types, as it offers better performance and this class may no longer provide required
	  * memory access guarantees in the future.
	  */
	@SerialVersionUID(Ver)
	private final class OfVal[@specialized(SpecializedVars) T] private[vars] extends VolatileBox[T] {
		private[this] var x :T = _ //access always sandwiched between two accesses to state, so it needs not to be volatile
		@scala.volatile private[this] var state :Int = Empty //tells if the box is empty and serves as a spin lock variable

		override def isEmpty :Boolean = state == Empty

		override def value :T = opt getOrElse throwNoSuch()

		override def value_=(newValue :T) :Unit = set(newValue, Full)

		override def opt :Opt[T] = {
			val current = state
			if (current == Empty)
				Lack
			else if (lockNonEmpty(current)) {
				val res = Got(x)
				state = Full
				res
			} else
				Lack
		}
		override def unsure :Unsure[T] = {
			val current = state
			if (current == Empty)
				Missing
			else if (lockNonEmpty(current)) {
				val res = Sure(x)
				state = Full
				res
			} else
				Missing
		}

		override def clear() :Unit = set(x, Empty) //T is a value type, so we don't care about unreferencing the current value.

		override def clear(expect :T) :Boolean = {
			val current = state
			current != Empty && lockNonEmpty(current) && {
				if (x == expect) {
					state = Empty; true
				} else {
					state = Full; false
				}
			}
		}

		override def put(newValue :T) :Boolean = {
			val current = state
			if (lock(current) == Empty) {
				x = newValue; state = Full; true
			} else {
				state = Full; false
			}
		}
		override def swap(content :Opt[T]) :Opt[T] = {
			val res = if (lock() == Empty) Lack else Got(x)
			if (content.isDefined) {
				x = content.get
				state = Full
			} else
				state = Empty
			res
		}
		override def swap(content :Option[T]) :Option[T] = {
			val res = if (lock() == Empty) None else Some(x)
			content match {
				case Some(newValue) => x = newValue; state = Full
				case _ => state = Empty
			}
			res
		}
		override def swap(content :Unsure[T]) :Unsure[T] = {
			val res = if (lock() == Empty) Missing else Sure(x)
			content match {
				case Sure(newValue) => x = newValue; state = Full
				case _ => state = Empty
			}
			res
		}

		override def ?=(newValue :T) :T = {
			if (!lockNonEmpty())
				throwNoSuch()
			val res = x
			x = newValue
			state = Full
			res
		}
		override def testAndSet(expect :T, newValue :T) :Boolean = {
			val current = state
			current != Empty && { lock(current) != Empty } && {
				if (x == expect) {
					x = newValue
					state = Full
					true
				} else {
					state = Full
					false
				}
			}
		}

		override def apply(f :T => T) :T =
			if (lockNonEmpty()) {
				val res = f(x)
				state = Full
				res
			} else
				throwNoSuch()

		override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T =
			if (lockNonEmpty()) {
				val res = f(z, x)
				state = Full
				res
			} else
				throwNoSuch()

		override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T =
			if (lockNonEmpty()) {
				val res = f(x, z)
				state = Full
				res
			} else
				throwNoSuch()

		private def set(newValue :T, resultState :Int) :Unit = {
			lock()
			x = newValue
			state = resultState
		}

		/** Atomically waits until the state is not Locked and sets it to Locked, returning the state before locking.  */
		private[this] def lock(currentState :Int = state) :Int = {
			var current = currentState
			while (current == Locked || !ValStateField.weakCompareAndSet(this :AnyRef, current, Locked))
				current = state
			current
		}
		/** Atomically waits until the state is not Locked and sets it to Locked if it is Empty.
		  * @return `true` if `currentState` was not Empty and has not concurrently been set to Empty during this call.
		  */
		private[this] def lockNonEmpty(currentState :Int = state) :Boolean = {
			var current = currentState
			while (current != Empty || ValStateField.weakCompareAndSet(this, Full, Locked))
				current = state
			current == Full
		}

		private[vars] override def isSpecialized = true
	}



	/** A simple implementation of VolatileBox based on an `Opt` field erased to `AnyRef` in the byte code,
	  * which reduces all operations to a single test-and-set (in the worst case)
	  */
	@SerialVersionUID(Ver)
	private final class OfRef[T] extends VolatileBox[T] {
		private[this] var x :Opt[T] = Lack //name x is used by VolatileBox.RefOptField

		override def isEmpty :Boolean = x.isEmpty
		override def value :T = x.get
		override def value_=(newValue :T) :Unit = x = Got(newValue)

		override def opt: Opt[T] = x

		override def clear() :Unit = x = Lack
		override def clear(expect :T) :Boolean = testAndSet(expect, emptyContent)
		override def put(newValue :T) :Boolean = testAndSet(emptyContent, newValue)

		override def swap(content :Opt[T]) :Opt[T] =
			(RefOptField.getAndSet(this, contentsOf(content)) :T) match {
				case empty :AnyRef if empty eq emptyContent => Lack
				case old => Got(old)
			}
		override def swap(content :Option[T]) :Option[T] =
			(RefOptField.getAndSet(this, content getOrElse emptyContent) :T) match {
				case empty :AnyRef if empty eq emptyContent => None
				case old => Some(old)
			}
		override def swap(content :Unsure[T]) :Unsure[T] =
			(RefOptField.getAndSet(this, content getOrElse emptyContent) :T) match {
				case empty :AnyRef if empty eq emptyContent => Missing
				case old => Sure(old)
			}

		override def testAndSwap(expect :Opt[T], newValue :Opt[T]) :Boolean =
			testAndSet(contentsOf(expect), contentsOf(newValue))

		@tailrec override def ?=(newValue :T) :T = x match {
			case Got(expect) =>
				if (testAndSet(expect, newValue))
					expect
				else
					this ?= newValue
			case _ => throwNoSuch()
		}

		@inline override def testAndSet(expect :T, newValue :T) :Boolean =
			RefOptField.compareAndSet(this, expect, newValue)

		@tailrec override def apply(f: T => T): T = x match {
			case Got(expect) =>
				val res = f(expect)
				if (testAndSet(expect, res)) res
				else apply(f)
			case _ => throwNoSuch()
		}
		@tailrec override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = x match {
			case Got(expect) =>
				val res = f(z, expect)
				if (testAndSet(expect, res)) res
				else applyLeft(z)(f)
			case _ => throwNoSuch()
		}
		@tailrec override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = x match {
			case Got(expect) =>
				val res = f(expect, z)
				if (testAndSet(expect, res)) res
				else applyRight(z)(f)
			case _ => throwNoSuch()
		}

		private[vars] override def isSpecialized = false
	}


	private def throwNoSuch() :Nothing = throw new NoSuchElementException("Box()")

	@inline private def contentsOf[T](opt :Opt[T]) :T =
		if (opt.isDefined) opt.get else Opt.NoContent.asInstanceOf[T]

	@inline private def emptyContent[T] = Opt.NoContent.asInstanceOf[T]
}
