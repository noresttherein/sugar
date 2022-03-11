package net.noresttherein.slang.vars

import java.lang.invoke.MethodHandles

import net.noresttherein.slang.vars.InOut.SpecializedVars
import net.noresttherein.slang.vars.Opt.{Got, Lack}




/** A mutable [[net.noresttherein.slang.vars.Option]]-like value holder. It expands on the base [[net.noresttherein.slang.vars.InOut InOut]]
  * interface by allowing an instance to hold no value at all. Aside from the inherited mutator methods setting
  * the value of this variable, a box can be at any time [[net.noresttherein.slang.vars.Box.clear cleared]],
  * emptying its contents. An empty box will throw a [[NoSuchElementException]] from any method accessing
  * its current value, be it directly through the [[net.noresttherein.slang.vars.Box.value value]] property
  * and [[net.noresttherein.slang.vars.InOut.get get]]/[[net.noresttherein.slang.vars.Ref.apply apply]]`()` methods,
  * or indirectly by ''test-and-set'' and ''get-and-set'' operations and similar.
  *
  * While the default implementation is not thread safe, in order to complement existing
  * [[net.noresttherein.slang.vars.InOut.testAndSet testAndSet]] method,
  * @author Marcin Mościcki
  */
sealed trait Box[@specialized(SpecializedVars) T] extends InOut[T] with Serializable {
	/** Returns `!`[[net.noresttherein.slang.vars.InOut.isDefined]].
	  * An empty box will throw a [[NoSuchElementException]] if the value is accessed by any method
	  * such as [[net.noresttherein.slang.vars.InOut.value value]] or [[net.noresttherein.slang.vars.InOut.get get]],
	  * but also [[net.noresttherein.slang.vars.InOut.?= ?=]],
	  * [[net.noresttherein.slang.vars.InOut.testAndSet testAndSet]] and others.
	  */
	@inline final def isEmpty :Boolean = !isDefined

	/** Returns `true` if the box currently holds a value. In case of atomic access,
	  * methods [[net.noresttherein.slang.vars.InOut.value value]], [[net.noresttherein.slang.vars.InOut.get]]
	  * and [[net.noresttherein.slang.vars.Ref.apply apply]]`()` will not throw an exception.
	  * Equivalent to [[net.noresttherein.slang.vars.Ref.isDefined isDefined]].
	  */
	@inline final def nonEmpty :Boolean = isDefined


	/** Replaces the contents of this box with the content of the given optional value.
	  * This is equivalent to
	  * `this `[[net.noresttherein.slang.vars.InOut.:= :=]]` content.get`
	  * if `content` is `Some`, or [[net.noresttherein.slang.vars.Box.clear clear]]`()` if `content` is `None`.
	  */
	def reset(content :Option[T]) :Unit = asOption = content

	/** Replaces the contents of this box with the content of the given optional value.
	  * This is equivalent to
	  * `this `[[net.noresttherein.slang.vars.InOut.:= :=]]` content.`[[net.noresttherein.slang.vars.Opt.get get]]
	  * if `content.`[[net.noresttherein.slang.vars.Opt.isDefined isDefined]],
	  * or [[net.noresttherein.slang.vars.Box.clear clear]]`()` if `content` is empty.
	  */
	def reset(content :Opt[T]) :Unit = opt = content

	/** Replaces the contents of this box with the content of the given value wrapper
	  * (possibly a [[net.noresttherein.slang.vars.Unsure Unsure]] or another [[net.noresttherein.slang.vars.Box Box]]
	  * instance).
	  *
	  * If `content` is a [[net.noresttherein.slang.vars.Val Val]], it simply assigns its value
	  * to this variable with `this.`[[net.noresttherein.slang.vars.InOut.value_= value]]` = content.get`.
	  * This call can block awaiting the initialization of `content` by other threads, which possibly has not even begun
	  *  - for example for [[net.noresttherein.slang.vars.Out Out]] or [[net.noresttherein.slang.vars.SignalVal SignalVal]].
	  * If you wish to assign the current value, use `this reset content.`[[net.noresttherein.slang.vars.Ref.opt opt]]
	  * or explicit conditional assignment instead.
	  *
	  * If `content` is a [[net.noresttherein.slang.vars.Sure Sure]], the result is the same as above, while
	  * [[net.noresttherein.slang.vars.Blank Blank]] results in the contents of this variable being immediately
	  * [[net.noresttherein.slang.vars.Box.clear cleared]].
	  *
	  * If content is neither a `Val` nor a `Unsure`, it attempts to retrieve its value with either
	  * [[net.noresttherein.slang.vars.Ref.opt opt]] or [[net.noresttherein.slang.vars.Ref.asOption asOption]]
	  * and delegates to one of the overloaded `reset` variants. In this case the operation is not expected to block
	  * for longer than necessary to atomically read the current value.
	  * Note that if `content` is not thread safe, the above calls can return result in an erroneous
	  * [[NoSuchElementException]] being thrown or even corrupt, partially constructed objects.
	  * Any thread safe type however guarantees the atomicity of these operations.
	  */
	def reset(content :Ref[T]) :Unit = unsure = content


	/** Replaces the contents of this box with the content of the given `Option`.
	  * It is a setter matching getter `this.`[[net.noresttherein.slang.vars.Ref.? ?]] and is equivalent to both
	  * `this.`[[net.noresttherein.slang.vars.Box.asOption_= asOption]]` = content`
	  * and `this `[[net.noresttherein.slang.vars.InOut.:= :=]]` content.get` if `content` is `Some`,
	  * or [[net.noresttherein.slang.vars.Box.clear clear]]`()` if `content` is `None`.
	  */
	def `?_=`(content :Option[T]) :Unit = if (content.isDefined) value = content.get else clear()

	/** Replaces the contents of this box with the content of the given `Option`.
	  * It is a setter matching getter `this.`[[net.noresttherein.slang.vars.Ref.asOption asOption]]
	  * and is equivalent to `this `[[net.noresttherein.slang.vars.InOut.:= :=]]` content.get` if `content` is `Some`,
	  * or [[net.noresttherein.slang.vars.Box.clear clear]]`()` if `content` is `None`.
	  */
	def asOption_=(content :Option[T]) :Unit = if (content.isDefined) value = content.get else clear()

	/** Replaces the contents of this box with the content of the given optional value.
	  * It is a setter method matching getter `this.`[[net.noresttherein.slang.vars.Ref.opt opt]] and is equivalent to
	  * `this `[[net.noresttherein.slang.vars.InOut.:= :=]]` content.`[[net.noresttherein.slang.vars.Opt.get get]]
	  * if `content.`[[net.noresttherein.slang.vars.Opt.isDefined isDefined]],
	  * or [[net.noresttherein.slang.vars.Box.clear clear]]`()` if `content` is empty.
	  */
	def opt_=(content :Opt[T]) :Unit = if (content.isDefined) value = content.get else clear()

	/** Replaces the contents of this box with the content of the given value wrapper (possibly
	  * a [[net.noresttherein.slang.vars.Unsure Unsure]] or another [[net.noresttherein.slang.vars.Box Box]] instance).
	  *
	  * If `content` is a [[net.noresttherein.slang.vars.Val Val]], it simply assigns its value
	  * to this variable with `this.`[[net.noresttherein.slang.vars.InOut.value_= value]]` = content.get`.
	  * This call can block awaiting the initialization of `content` by other threads, which possibly has not even begun
	  *  - for example for [[net.noresttherein.slang.vars.Out Out]] or [[net.noresttherein.slang.vars.SignalVal SignalVal]].
	  * If you wish to assign the current value, use `this reset content.`[[net.noresttherein.slang.vars.Ref.opt opt]]
	  * or explicit conditional assignment instead.
	  *
	  * If `content` is a [[net.noresttherein.slang.vars.Sure Sure]], the result is the same as above, while
	  * [[net.noresttherein.slang.vars.Blank Blank]] results in the contents of this variable being immediately
	  * [[net.noresttherein.slang.vars.Box.clear cleared]].
	  *
	  * If content is neither a `Val` nor a `Unsure`, it attempts to retrieve its value with either
	  * [[net.noresttherein.slang.vars.Ref.opt opt]] or [[net.noresttherein.slang.vars.Ref.asOption asOption]]
	  * and delegates to one of the overloaded `reset` variants. In this case the operation is not expected to block
	  * for longer than necessary to atomically read the current value.
	  * Note that if `content` is not thread safe, the above calls can return result in an erroneous
	  * [[NoSuchElementException]] being thrown or even corrupt, partially constructed objects.
	  * Any thread safe type however guarantees the atomicity of these operations.
	  */
	def unsure_=(content :Ref[T]) :Unit = content match {
		case _ :Val[_] => value = content.get
		case hit :Sure[T] => value = hit.value
		case Blank => clear()
		case _ if content.isSpecialized => unsure = content.unsure //Shot cheaper than Opt
		case _ => opt = content.opt //use content.opt for atomicity (at least if content is thread safe)
	}


	/** Implements a ''get-and-set'' operation, which sets the value of this box to `content`
	  * ([[net.noresttherein.slang.vars.Box.clear clearing]] it
	  * if `content.`[[Option.isEmpty isEmpty]]) and returns the value contained by
	  * this box before the method was called. In a synchronised context it is equivalent to
	  * {{{
	  *     def swap(content :Option[T]) = {
	  *         val current = this.?
	  *         if (content.isDefined) this.value = content.get
	  *         else this.clear()
	  *         current
	  *     }
	  * }}}
	  * @param content the new content for this variable: if non empty, `this.value` will be set to its value;
	  *                if empty, the variable will be cleared.
	  * @return the previous content of this variable, replaced by `content`.
	  */
	def swap(content :Option[T]) :Option[T] = { val res = asOption; reset(content); res }

	/** Implements a ''get-and-set'' operation, which sets the value of this box to `content`
	  * ([[net.noresttherein.slang.vars.Box.clear clearing]] it
	  * if `content.`[[net.noresttherein.slang.vars.Opt.isEmpty isEmpty]]) and returns the value contained by
	  * this box before the method was called. In a synchronised context it is equivalent to
	  * {{{
	  *     def swap(content :Opt[T]) = {
	  *         val current = this.opt
	  *         if (content.isDefined) this.value = content.get
	  *         else this.clear()
	  *         current
	  *     }
	  * }}}
	  * @param content the new content for this variable: if non empty, `this.value` will be set to its value;
	  *                if empty, the variable will be cleared.
	  * @return the previous content of this variable, replaced by `content`.
	  */
	def swap(content :Opt[T]) :Opt[T] = { val res = opt; reset(content); res }

	/** Implements a ''get-and-set'' operation, which sets the value of this box to `content`
	  * ([[net.noresttherein.slang.vars.Box.clear clearing]] it
	  * if `content.`[[net.noresttherein.slang.vars.Unsure.isEmpty isEmpty]]) and returns the value contained by
	  * this box before the method was called. In a synchronised context it is equivalent to
	  * {{{
	  *     def swap(content :Unsure[T]) = {
	  *         val current = this.unsure
	  *         if (content.isDefined) this.value = content.get
	  *         else this.clear()
	  *         current
	  *     }
	  * }}}
	  * @param content the new content for this variable: if non empty, `this.value` will be set to its value;
	  *                if empty, the variable will be cleared.
	  * @return the previous content of this variable, replaced by `content`.
	  */
	def swap(content :Unsure[T]) :Unsure[T] = { val res = unsure; reset(content); res }


	/** Empties the box, unreferencing the currently held value, if any. */
	def clear() :Unit

	/** Empties the box, providing it currently contains the specified value.
	  * Any references to the currently held value are freed.
	  * @param expect a test value which must equal the current value of the box.
	  * @return true ''iff'' the box was cleared by this method.
	  */
	def clear(expect :T) :Boolean =
		isDefined && value == expect && { clear(); true }

	/** Sets the content of this box, providing it is currently empty.
	  * @return true ''iff'' the box was empty and now contains the new value.
	  */
	def put(newValue :T) :Boolean = isEmpty && { value = newValue; true }

	/** Replaces the contents of this box, if they are equal to `expect`, with the contents of `newValue`.
	  * For the purpose of this method, 'contents' are defined as `this.`[[net.noresttherein.slang.vars.Box.opt opt]].
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.slang.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.slang.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.slang.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.slang.vars.Box.isEmpty]] if both arguments are empty.
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
	  * For the purpose of this method, 'contents' are defined as
	  * `this.`[[net.noresttherein.slang.vars.Box.asOption asOption]].
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.slang.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.slang.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.slang.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.slang.vars.Box.isEmpty]] if both arguments are empty.
	  */
	def testAndSwap(expect :Option[T], newValue :Option[T]) :Boolean = expect match {
		case Some(current) => newValue match {
			case Some(value) => testAndSet(current, value)
			case _ => clear(current)
		}
		case _ if newValue.isDefined => put(newValue.get)
		case _ => isEmpty //clear a box if it's empty
	}

	/** Replaces the contents of this box, if they are equal to `expect`, with the current contents of `newValue`.
	  * Depending on whether each of the parameters is defined, this method is equivalent
	  * to one of [[net.noresttherein.slang.vars.Box.clear(expect* clear]]`(expect.get)`,
	  * [[net.noresttherein.slang.vars.Box.put put]]`(newValue.get)`,
	  * [[net.noresttherein.slang.vars.Box.testAndSet testAndSet]]`(expect.get, newValue.get)`, or simply
	  * [[net.noresttherein.slang.vars.Box.isEmpty]] if both arguments are empty.
	  */
	def testAndSwap(expect :Ref[T], newValue :Ref[T]) :Boolean =
		if (expect.isDefined)
			if (newValue.isDefined) testAndSet(expect.get, newValue.get)
			else clear(expect.get)
		else if (newValue.isDefined) put(newValue.get)
		else isEmpty

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Box[_]]

	override def toString :String = opt match {
		case Got(value) => String.valueOf(value)
		case _ => "Box()"
	}
}



object Box {
	/** Creates a new, empty box. The returned instance is not thread safe. */
	def apply[@specialized(SpecializedVars) T] :Box[T] = new FastBox

	/** Creates a new box, initialized with the given value. The returned instance is not thread safe. */
	def apply[@specialized(SpecializedVars) T](value :T) :Box[T] = {
		val box = new FastBox[T]
		box := value
		box
	}

	/** Creates a new, empty box. The method is exactly equivalent to [[net.noresttherein.slang.vars.Box.apply Box]]`[T]`,
	  * but is more readable than `Box.apply` if the type parameter is inferred.
	  */
	def empty[@specialized(SpecializedVars) T] :Box[T] = new FastBox


	@inline def unapply[@specialized(SpecializedVars) T](box :Box[T]) :Box[T] = box



	/** A non-synchronized `Box` implementation. */
	@SerialVersionUID(1L)
	private class FastBox[@specialized(SpecializedVars) T] extends Box[T] {
		private[this] var nullVal :T = _ //the default value used to clear x so we don't keep a reference to the old value
		private[this] var x :T = _
		private[this] var full :Boolean = true

		override def isDefined :Boolean = full

		override def value :T =
			if (full) x else throw new NoSuchElementException("Box()")

		override def value_=(newValue :T) :Unit = {
			x = newValue
			full = true
		}

		override def clear() :Unit = { full = false; x = nullVal }

		override def opt :Opt[T] = if (full) Got(x) else Lack

		override def isSpecialized :Boolean = getClass != classOf[FastBox[_]]
	}
}






/** A thread safe implementation of [[net.noresttherein.slang.vars.Box Box]], with `@volatile` memory access semantics
  * and atomic test-and-set operations like in [[net.noresttherein.slang.vars.Volatile Volatile]]. All synchronisation
  * is non-suspending (spin locks) and features a lower overhead than monitor synchronization, but is not optimized
  * for high-contention usage scenarios.
  */
sealed trait VolatileBox[@specialized(SpecializedVars) T] extends Box[T]


object VolatileBox {
	/** Creates a new, empty box. The instance is thread safe with the same memory access semantics
	  * as a [[net.noresttherein.slang.vars.Volatile Volatile]] variable. All operations are atomic.
	  */
	def apply[@specialized(SpecializedVars) T] :VolatileBox[T] = {
		var res :VolatileBox[T] = new VolatileValBox
		if (res.getClass == unspecializedClass)
			res = new VolatileRefBox[T]
		res
	}

	/** Creates a new, atomic, box, initialized with the given value. The instance is thread safe
	  *  with the same memory access semantics as a [[net.noresttherein.slang.vars.Volatile Volatile]] variable.
	  *  All operations are atomic.
	  */
	def apply[@specialized(SpecializedVars) T](value :T) :VolatileBox[T] = {
		val res = VolatileBox[T]
		res.value = value
		res
	}

	/** Creates a new, empty atomic box. The method is exactly equivalent
	  * to [[net.noresttherein.slang.vars.VolatileBox.apply Box]]`[T]`, but is more readable than `VolatileBox.apply`
	  * if the type parameter is inferred.
	  */
	@inline def empty[@specialized(SpecializedVars) T] :VolatileBox[T] = VolatileBox[T]


	private final val Empty = 0
	private final val Full = 1
	private final val Locked = 2

	private val ValStateField = MethodHandles.lookup.findVarHandle(classOf[VolatileValBox[_]], "state", Integer.TYPE)
	private val RefOptField = MethodHandles.lookup.findVarHandle(classOf[VolatileRefBox[_]], "x", classOf[Any])
	private val unspecializedClass = classOf[VolatileValBox[Any]]


	//todo: see if there is actually any performance benefit over VolatileRefBox
	/** A non-boxing implementation of `VolatileBox`, suitable for value types.
	  * [[net.noresttherein.slang.vars.VolatileBox.VolatileRefBox VolatileRefBox]] should be used instead
	  * for reference types, as it offers better performance and this class may no longer provide required
	  * memory access guarantees in the future.
	  */
	@SerialVersionUID(1L)
	private final class VolatileValBox[@specialized(SpecializedVars) T] private[vars] extends VolatileBox[T] {
		private[this] var x :T = _ //access always sandwiched between two accesses to state, so it needs not to be volatile
		@scala.volatile private[this] var state :Int = Empty //tells if the box is empty and serves as a spin lock variable

		override def isDefined :Boolean = state == Full

		override def value :T = opt getOrElse { throw new NoSuchElementException("Box()") }

		override def value_=(newValue :T) :Unit = set(newValue, Full)

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

		override def testAndSet(expect :T, newValue :T) :Boolean = {
			val current = state
			if (current == Empty)
				throw new NoSuchElementException("Box()")
			lockNonEmpty(current) && {
				if (x == expect) {
					x = newValue; state = Full; true
				} else {
					state = Full; false
				}
			}
		}

		@throws[NoSuchElementException]("if the box is currently empty.")
		override def ?=(newValue :T) :T = {
			val current = state
			if (current == Empty)
				throw new NoSuchElementException("Box()")
			if (!lockNonEmpty(current))
				throw new NoSuchElementException("Box()")
			val res = x
			x = newValue
			state = Full
			res
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
			val res = if (lock() == Empty) Blank else Sure(x)
			content match {
				case Sure(newValue) => x = newValue; state = Full
				case _ => state = Empty
			}
			res
		}

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
				Blank
			else if (lockNonEmpty(current)) {
				val res = Sure(x)
				state = Full
				res
			} else
				Blank
		}


		private def set(newValue :T, resultState :Int) :Unit = {
			lock()
			x = newValue
			state = resultState
		}

		private[this] def lock(currentState :Int = state) :Int = {
			var current = currentState
			while (current == Locked || !ValStateField.weakCompareAndSet(this :AnyRef, current, Locked))
				current = state
			current
		}
		private[this] def lockNonEmpty(currentState :Int = state) :Boolean = {
			var current = currentState
			while (current != Empty || ValStateField.weakCompareAndSet(this, Full, Locked))
				current = state
			current == Full
		}
		private def fail() = throw new NoSuchElementException("Box()")

		private[vars] override def isSpecialized = true
	}


	/** A simple implementation of VolatileBox based on an `Opt` field erased to `AnyRef` in the byte code,
	  * which reduces all operations to a single test-and-set (in the worst case)
	  */
	@SerialVersionUID(1L)
	private final class VolatileRefBox[T] extends VolatileBox[T] {
		private[this] var x :Opt[T] = Lack //name x is used by VolatileBox.RefOptField

		override def isDefined :Boolean = x.isDefined
		override def value :T = x.get
		override def value_=(newValue :T) :Unit = x = Got(newValue)

		override def clear() :Unit = x = Lack
		override def clear(expect :T) :Boolean = testAndSet(expect, emptyContent)
		override def put(newValue :T) :Boolean = testAndSet(emptyContent, newValue)
		override def testAndSet(expect :T, newValue :T) :Boolean = RefOptField.compareAndSet(this, expect, newValue)

		override def testAndSwap(expect :Opt[T], newValue :Opt[T]) :Boolean =
			testAndSet(contentsOf(expect), contentsOf(newValue))

		override def swap(content :Opt[T]) :Opt[T] =
			(RefOptField.getAndSet(this, contentsOf(content)) :T) match {
				case empty :AnyRef if empty eq emptyContent => Lack
				case old => Got(old)
			}
		override def swap(content :Option[T]) :Option[T] =
			(RefOptField.getAndSet(this, content getOrElse Opt.NoContent) :T) match {
				case empty :AnyRef if empty eq emptyContent => None
				case old => Some(old)
			}
		override def swap(content :Unsure[T]) :Unsure[T] =
			(RefOptField.getAndSet(this, content getOrElse Opt.NoContent) :T) match {
				case empty :AnyRef if empty eq emptyContent => Blank
				case old => Sure(old)
			}

		override def ?=(newValue :T) :T = swap(Got(newValue)).get

		override def opt :Opt[T] = x

		private[vars] override def isSpecialized = false
	}

	@inline private def contentsOf[T](opt :Opt[T]) :T =
		if (opt.isDefined) opt.get else Opt.NoContent.asInstanceOf[T]

	@inline private def emptyContent[T] = Opt.NoContent.asInstanceOf[T]
}
