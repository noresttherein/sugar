package net.noresttherein.slang.vars

import scala.reflect.ClassTag

import Opt.{Got, Lack}
import net.noresttherein.slang.raise
import net.noresttherein.slang.vars.Shot.{collector, unzip2Fail, unzip3Fail, WithFilter}
import net.noresttherein.slang.vars.InOut.SpecializedVars




/** A `@specialized` (for all value types with the exception of `Unit`) version of [[scala.Option]]
  * conforming to [[net.noresttherein.slang.vars.Val Val]] interface.
  * Like `Option`, it has two subclasses: [[net.noresttherein.slang.vars.Hit Hit]]`[T]` (equivalent of `Some[T]`)
  * and [[net.noresttherein.slang.vars.Miss Miss]] (equivalent of `None`).
  * The difference from `Option` and its raison d'etre is that it does not involve boxing of built in (or opaque)
  * value types, as long as the type parameter is statically known at creation and usage sites, or the calling code
  * is also specialized for the given value type. This can yield a noticeable performance benefit in scenarios
  * where instances are created and discarded rapidly, such as flat mapping and simple recursion.
  *
  * Like `Option`, it implements [[IterableOnce]]`[T]` and completely parrots full `Option` API, making it a suitable
  * drop in replacement.
  *
  * This type has a rival in this library in the form of [[net.noresttherein.slang.vars.Opt Opt]],
  * which is another `Option` replacement candidate, also aiming to reduce boxing. The difference lies in exactly
  * what boxing is avoided:
  *   - when an `Int` or other value type is placed in a Scala `Option`, two objects need to be created:
  *     an `Integer` wrapper, and `Some` itself containing the wrapper.
  *   - `Opt` is a value class, and in any context in which it doesn't require runtime promotion to a reference type
  *     (use as a type parameter, upcasting to `Any`, putting it into an `Array` or a collection), it is erased
  *     to a reference to the contained object. Value classes cannot be `@specialized` however,
  *     and thus instances for value types (and other value classes) require boxing to their wrapper types.
  *     For integer types, especially smaller values, `Unit` and `Boolean`, this is not a significant drawback
  *     due to the runtime environment's employment of caching of commonly used values. Value classes and floating point
  *     numbers will however require boxing. Another drawback is that its erasure clashes with the wrapped type itself,
  *     preventing overloaded methods, just as compiler bridges required when a method accepting/returning an `Opt`
  *     overrides a generic method with an abstract type in place of the `Opt` value clash with the method's erasure.
  *     When an instance is repeatedly passed between contexts in which it is referred to as an abstract type
  *     and where it is erased, this can paradoxically result in significantly more boxing than a once created
  *     and used as-is `Option` would require.
  *   - `Shot` does not involve boxing of value types used as type parameters, but is itself a reference type,
  *     so an object will always be created, while `Opt` can, in some circumstances, avoid boxing altogether.
  *     This boxing however means that it can 'safely' be used in generic contexts, as once created object.
  *     It does not have the other erasure related drawbacks of `Opt`.
  *
  * In cases where the wrapped type is expected to (often) be a value type, `Shot` is a safer option,
  * without the risks and headaches of `Opt`, especially for longer-living objects or when the programmer doesn't have
  * the full control of how and where the created instances are used. On the other hand, for short-living objects
  * which are likely to be used as parameters/return values of a single method call, such as `unapply` methods,
  * as well as when the wrapped type is known or realistically expected to be a reference type, `Opt` will generally
  * be the better choice. Both however should be used only when many objects are created in succession,
  * or when reducing the garbage collector pauses is a priority, or when the values do not escape the scope
  * of a single class, as in most cases the benefits are outweighed by the drawbacks of potential compatibility
  * issues due to using a non-standard solution.
  *
  * @author Marcin Mościcki
  */
sealed trait Shot[@specialized(SpecializedVars) +T] extends Ref[T] with IterableOnce[T] with Product with Serializable {
	/** Tests if this `Shot` does not contain a value (is equal to [[net.noresttherein.slang.vars.Miss Miss]]). */
	@inline final def isEmpty: Boolean = this eq Miss

	/** Tests if this `Shot` contains a value. If true, `get` will not throw an exception.
	  * Equivalent to [[net.noresttherein.slang.vars.Shot.isDefined isDefined]].
	  */
	@inline final def nonEmpty: Boolean = this ne Miss

	@inline final override def isDefined :Boolean = this ne Miss

	@inline override def knownSize :Int = if (this eq Miss) 0 else 1

	@inline override def productArity :Int = if (this eq Miss) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && (this ne Miss)) get
		else throw new IndexOutOfBoundsException(toString + ".productElement(" + n + ")")

	/** Forces extraction of the value.
	  * @return contained value, if this is a [[net.noresttherein.slang.vars.Hit Hit]].
	  * @throws NoSuchElementException if this `Shot` is empty.
	  */
	override def get :T

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline final def getOrElse[O >: T](alt: => O) :O =
		if (this eq Miss) alt else get

	/** Similarly to [[net.noresttherein.slang.vars.Shot.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty.
	  */
	@inline final def defaultTo[O >: T](alt: O) :O =
		if (this eq Miss) alt else get

	/** Assuming that `T` is a nullable type, return `null` if this `Shot` is empty, or the wrapped value otherwise. */
	@inline final def orNull[O >: T](implicit isNullable :Null <:< O) :O =
		if (this eq Miss) null.asInstanceOf[O] else get


	/** Gets the value of this instance or throws the exception given as the type parameter with the given message.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.slang.vars.Shot.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.slang.vars.Shot.orIllegal orIllegal]]
	  */
	@inline final def orThrow[E <: Throwable :ClassTag](msg: => String) :T =
		if (this eq Miss) raise[E](msg) else get

	/** Gets the value of this instance or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.slang.vars.Shot.orThrow orThrow]]
	  */
	@inline final def orNoSuch(msg: => String) :T =
		if (this eq Miss) throw new NoSuchElementException(msg) else get

	/** Gets the value of this instance or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.slang.vars.Shot.orThrow orThrow]]
	  */
	@inline final def orIllegal(msg: => String) :T =
		if (this eq Miss) throw new IllegalArgumentException(msg) else get

	/** Asserts that this instance is not empty, throwing an `AssertionError` otherwise, and returns its contents. */
	@inline final def orError(msg: => String) :T = {
		assert(this ne Miss, msg)
		get
	}



	/** Returns the value this `Shot` if it is not empty, or the lazily computed alternative otherwise. */
	@inline final def orElse[O >: T](alt: => Shot[O]) :Shot[O] =
		if (this eq Miss) alt else this

	/** Similarly to [[net.noresttherein.slang.vars.Shot.orElse orElse]], returns this `Shot` if it is not empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty.
	  */
	@inline final def ifEmpty[O >: T](alt: Shot[O]) :Shot[O] =
		if (this eq Miss) alt else this

	/** Returns this `Shot` if the condition is false and `Miss` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
	  */
	@inline final def orEmptyIf(condition :Boolean) :Shot[T] =
		if (condition) Miss else this

	/** Returns this `Shot` if the condition is true and `Miss` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
	  */
	@inline final def orEmptyUnless(condition :Boolean) :Shot[T] =
		if (condition) this else Miss



	/** Returns `this` ''iff'' this value is [[net.noresttherein.slang.vars.Miss Miss]],
	  * or a [[net.noresttherein.slang.vars.Hit Hit]] containing the result of applying
	  * the given function to its value otherwise.
	  */
	@inline final def map[O](p :T => O) :Shot[O] =
		if (this eq Miss) Miss else new Hit(p(get))

	/** Applies the given function to the content of this `Shot` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse alternative`, but in one step.
	  */
	@inline final def mapOrElse[X](f :T => X, alternative: => X) :X =
		if (this eq Miss) alternative else f(get)

	/** Returns the result of applying `f` to the value of this `Shot` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.slang.vars.Shot.mapOrElse mapOrElse]] instead.
	  *  @param  ifEmpty the expression to evaluate if empty.
	  *  @param  f       the function to apply if nonempty.
	  */
	@inline final def fold[B](ifEmpty: => B)(f: T => B): B =
		if (this eq Miss) ifEmpty else f(get)

	/** Returns the result of applying the given function to the value of this `Shot` if it is not empty,
	  * or `this` if `this.isEmpty`
	  */
	@inline final def flatMap[O](p :T => Shot[O]) :Shot[O] =
		if (this eq Miss) Miss else p(get)

	/** Flattens `Attempt[Attempt[O]]` to a single `Attempt[O]`. */
	@inline final def flatten[O](implicit isAttempt :T <:< Shot[O]) :Shot[O] =
		if (this eq Miss) Miss else get

	/** Returns a new `Shot` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.slang.vars.Miss Miss]] otherwise.
	  */
	@inline final def filter(p :T => Boolean) :Shot[T] =
		if ((this eq Miss) || p(get)) this else Miss

	/** Returns a new `Shot` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.slang.vars.Miss Miss]] otherwise.
	  */
	@inline final def filterNot(p :T => Boolean) :Shot[T] =
		if ((this eq Miss) || !p(get)) this else Miss

	/** Equivalent to `this.`[[net.noresttherein.slang.vars.Shot.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions.
	  */
	@inline final def withFilter(p :T => Boolean) :WithFilter[T] = new WithFilter[T](this, p)


	/** Tests if this `Shot` is not empty and its value is equal to the given argument. */
	@inline final def contains[O >: T](o :O): Boolean =
		(this ne Miss) && (get == o)

	/** Tests if this `Shot` is not empty and its value satisfies the given predicate. */
	@inline final def exists(p :T => Boolean): Boolean = (this ne Miss) && p(get)

	/** Tests if this `Shot` contains no value or its value satisfies the given predicate. */
	@inline final def forall(p :T => Boolean): Boolean = (this eq Miss) || p(get)

	/** Executes the given block for this `Shot`s value if it is not empty. */
	@inline final def foreach[O](f :T => O) :Unit = if (this ne Miss) f(get)

	/** Returns an empty `Shot` if this `Shot` is empty the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Shot`.
	  */
	def collect[O](f :PartialFunction[T, O]) :Shot[O] =
		if (this eq Miss)
			Miss
		else {
			val res = f.applyOrElse(get, collector)
			if (res.asInstanceOf[AnyRef] eq collector) Miss else new Hit(res)
		}


	/** Returns an `Shot` formed from the contents of `this` and `that` by combining the corresponding elements
	  * in a pair. If either of the two options is empty, `Miss` is returned.
	  */
	@inline final def zip[O](that :Shot[O]) :Shot[(T, O)] =
		if ((this eq Miss) | (that eq Miss)) Miss
		else new Hit((get, that.get))

	/** Converts an `Shot` of a pair into `Shot`s of its first and second elements. */
	@inline final def unzip[T1, T2](implicit asPair: T <:< (T1, T2)): (Shot[T1], Shot[T2]) =
		if (this eq Miss)
			unzip2Fail
		else {
			val pair = asPair(get)
			(new Hit(pair._1), new Hit(pair._2))
		}

	/** Converts an `Shot` of a triple into three `Shot`s, one containing the element from each position
	  * of the triple.
	  */
	final def unzip3[T1, T2, T3](implicit asTriple: T <:< (T1, T2, T3)): (Shot[T1], Shot[T2], Shot[T3]) =
		if (this eq Miss)
			unzip3Fail
		else {
			val triple = asTriple(get)
			(new Hit(triple._1), new Hit(triple._2), new Hit(triple._3))
		}


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline final override def iterator :Iterator[T] =
		if (this eq Miss) Iterator.empty else Iterator.single(get)

	/** Returns `Nil` if this `Shot` is empty or or `this.get::Nil` otherwise. */
	@inline final def toList :List[T] = if (this eq Miss) Nil else get::Nil

	/** Returns an empty list if this `Shot` is empty or a single element list with its value otherwise. */
	@inline final def toSeq :Seq[T] = if (this eq Miss) Nil else get::Nil

	/** Returns an empty collection if this `Shot` is empty or a singleton with its value otherwise. */
	@inline final def toIterable :Iterable[T] = if (this eq Miss) Iterable.empty else Iterable.single(get)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	override def asOption :Option[T] = None //overriden by Success

	override def asShot :Shot[T] = this

	/** Converts this `Shot` to an `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty.
	  */
	@inline final def toLeft[O](right: => O) :Either[T, O] =
		if (this eq Miss) Right(right) else Left(get)

	/** Converts this `Shot` to an `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty.
	  */
	@inline final def toRight[O](left: => O) :Either[O, T] =
		if (this eq Miss) Left(left) else Right(get)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Shot[_]]
}




/** A factory of `Option`-like values [[net.noresttherein.slang.vars.Shot Shot]]`[T]`, providing additionally
  * implicit conversions to and from [[net.noresttherein.slang.vars.Option]] and [[Iterable]].
  */
object Shot {

	/** An `Shot` factory which creates [[net.noresttherein.slang.vars.Hit Hit]]`(value)`
	  * if the argument is not null, and [[net.noresttherein.slang.vars.Miss Miss]] if it is null.
	  *  @param value the value.
	  *  @return `if (value == null) Fail else Success(value)`.
	  */
	def apply[@specialized(SpecializedVars) T](value :T) :Shot[T] =
		if (value == null) Miss else new Hit(value)

	/** Converts the given `Option[T]` into a lighter `Opt[T]` which is erased at runtime. */
	def some_?[@specialized(SpecializedVars) T](value :Option[T]) =
		if (value.isDefined) new Hit(value.get, value) else Miss

	/** Returns [[net.noresttherein.slang.vars.Miss Miss]] in a manner consistent with the collections hierarchy. */
	def empty[T] : Shot[T] = Miss

	/** When a given condition is true, evaluates the `value` argument and returns
	  * [[net.noresttherein.slang.vars.Hit Hit]]`(value)`. When the condition is false, `value` is not evaluated
	  * and [[net.noresttherein.slang.vars.Miss Miss]] is returned.
	  */
	def when[@specialized(SpecializedVars) T](cond: Boolean)(value: => T): Shot[T] =
		if (cond) new Hit(value) else Miss

	/** Unless a given condition is true, this will evaluate the `value` argument and
	  * return [[net.noresttherein.slang.vars.Hit Hit]]`(value)`. Otherwise, `value` is not evaluated
	  * and [[net.noresttherein.slang.vars.Miss Miss]] is returned.
	  */
	@inline def unless[@specialized(SpecializedVars) T](cond: Boolean)(a: => T): Shot[T] =
		when(!cond)(a)


	/** Optional implicit conversions to/from `Opt`, `Option` and `Iterable`.
	  * They involve boxing and are placed here for explicit importing.
	  */
	object implicits {
		/** An implicit conversion that converts an option to an iterable value. */
		@inline implicit def shotToIterable[A](opt :Shot[A]): Iterable[A] = opt.toIterable

		/** An implicit conversion from an `Attempt[A]` to an `Option[A]`.
		  * The results are cached, so repeated conversions of the same instance do not result in boxing.
		  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit.
		  */
		@inline implicit def shotToOption[T](value :Shot[T]) :Option[T] = value.asOption

		/** A nomen omen optional implicit conversion of an `Option[A]` to an `Attempt[A]`.
		  * @see [[net.noresttherein.slang.vars.Shot.OptionToShotConverter]]
		  */ //consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
		@inline implicit def optionToShot[A](opt :Option[A]) :Shot[A] = Shot.some_?(opt)

		@inline implicit def optToShot[A](opt :Opt[A]) :Shot[A] = opt.asShot

		@inline implicit def shotToOpt[A](opt :Shot[A]) :Opt[A] = opt.opt
	}

	/** Provides [[net.noresttherein.slang.vars.Shot.OptionToShotConverter.toShot toShot]] extension method
	  * for any `Option[T]`, converting it to an `Attempt[T]`. A fully implicit conversion is available for import
	  * as `Attempt.implicits.`[[net.noresttherein.slang.vars.Shot.implicits.optionToShot optionToShot]].
	  */ //consider: removing it. Duplicated by optional.OptionExtension in optional.explicits
	implicit class OptionToShotConverter[T](private val self :Option[T]) extends AnyVal {
		/** Converts this option into an `Shot` wrapping the same type/value. */
		@inline def toShot :Shot[T] = some_?(self)
	}



	/** The for-comprehension facade for [[net.noresttherein.slang.vars.Shot Shot]]`[A]`
	  * which does not evaluate the filter predicate until `map`, `flatMap` or `foreach` is called.
	  */
	class WithFilter[+A](self :Shot[A], p :A => Boolean) {
		def map[B](f: A => B): Shot[B] = self filter p map f
		def flatMap[B](f: A => Shot[B]): Shot[B] = self filter p flatMap f
		def foreach[U](f: A => U): Unit = self filter p foreach f
		def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
	}

	private[this] val collect :Any => Any = _ => collect
	private def collector[A] = collect.asInstanceOf[Any => A]

	private val unzip2Fail = (Miss, Miss)
	private val unzip3Fail = (Miss, Miss, Miss)
}




/** A `Shot[T]` carrying a value, a counterpart of Scala [[Some]].
  * Unlike `Some`, it is specialized and does not involve boxing of built in value types used as contents.
  * @tparam T the content type.
  */
final class Hit[@specialized(SpecializedVars) +T] private[vars]
               (x :T, private[this] var cachedOption :Option[T] = None, private[this] var cachedOpt :Opt[T] = Lack)
	extends Shot[T]
{
	/** Same as [[net.noresttherein.slang.vars.Hit.get get]], for compatibility with [[Some]].
	  * Usage of `value` instead of `get` or `apply()` guarantees that the receiver is a `Hit` instance
	  * rather than a, possibly empty, [[net.noresttherein.slang.vars.Shot Shot]].
	  * This can prevent sneaky bugs introduced by refactors changing the type of `this` reference.
	  */
	def value :T = x
	override def get :T = x

	override def asOption :Option[T] = {
		if (cachedOption eq None)
			cachedOption = Some(value)
		cachedOption
	}
	override def opt :Opt[T] = {
		if (cachedOpt.isEmpty)
			cachedOpt = Got(value)
		cachedOpt
	}

	private[vars] override def isSpecialized :Boolean = getClass != classOf[Hit[Any]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Hit[_] => value == other.value
		case _ => false
	}
	override def hashCode :Int = value.hashCode

	override def toString :String = "Hit(" + x + ")"
}


/** A factory and deconstructor of 'full'/successful [[net.noresttherein.slang.vars.Shot Shot]] instances. */
object Hit {
	/** Constructs a successful `Shot` containing the given value. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Hit[T] = new Hit(value)

	/** Extracts the value from the given `Shot` if it is successful. */
	@inline def unapply[T](value :Shot[T]) :Shot[T] = value
}



/** An empty ('missed') [[net.noresttherein.slang.vars.Shot Shot]] instance, a counterpart of [[scala.None]]. */
case object Miss extends Shot[Nothing] {
	override def get :Nothing = throw new NoSuchElementException("Fail()")
	override def opt :Opt[Nothing] = Lack
}
