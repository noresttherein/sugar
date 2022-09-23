package net.noresttherein.sugar.vars

import scala.reflect.ClassTag

import net.noresttherein.sugar.raise
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.FinalRef
import net.noresttherein.sugar.vars.Unsure.{WithFilter, collector, unzip2Fail, unzip3Fail}




/** A `@specialized` (for all value types with the exception of `Unit`) version of [[scala.Option]]
  * conforming to [[net.noresttherein.sugar.vars.Val Val]] interface.
  * Like `Option`, it has two subclasses: [[net.noresttherein.sugar.vars.Sure Sure]]`[T]` (equivalent of `Some[T]`)
  * and [[net.noresttherein.sugar.vars.Missing Missing]] (equivalent of `None`).
  * The difference from `Option` and its raison d'etre is that it does not involve boxing of built in (or opaque)
  * value types, as long as the type parameter is statically known at creation and usage sites, or the calling code
  * is also specialized for the given value type. This can yield a noticeable performance benefit in scenarios
  * where instances are created and discarded rapidly, such as flat mapping and simple recursion.
  *
  * Like `Option`, it implements [[IterableOnce]]`[T]` and completely parrots full `Option` API, making it a suitable
  * drop in replacement.
  *
  * This type has a rival in this library in the form of [[net.noresttherein.sugar.vars.Opt Opt]],
  * which is another `Option` replacement candidate, also aiming to reduce boxing. The difference lies in exactly
  * what boxing is avoided:
  *   - when an `Int` or other value type is placed in a Scala `Option`, two objects need to be created:
  *     an `Integer` wrapper, and `Some` itself containing the wrapper.
  *   - `Opt` is a value class, and in any context in which it doesn't require runtime promotion to a reference type
  *     (use as a type parameter, upcasting to `Any`, putting it into an `Array` or a collection,
  *     returning from a function or passing as an argument to one), it is erased to a reference to the contained object.
  *     However, instances for value types (and other value classes) require boxing to their wrapper types.
  *     For integer types, especially smaller values, `Unit` and `Boolean`, this is partly offset by the runtime
  *     environment's employment of caching of commonly used values. Value classes and floating point numbers
  *     will however require boxing. Another drawback is that its erasure clashes with the wrapped type itself,
  *     preventing overloaded methods, just as compiler bridges required when a method accepting/returning an `Opt`
  *     overrides a generic method with an abstract type in place of the `Opt` value clash with the method's erasure.
  *     When an instance is repeatedly passed between contexts in which it is referred to as an abstract type
  *     and where it is erased, this can paradoxically result in significantly more boxing than a once created
  *     and used as-is `Option` would require.
  *   - `Unsure` does not involve boxing of value types used as type parameters, but is itself a reference type,
  *     so an object will always be created, while `Opt` can, in some circumstances, avoid boxing altogether.
  *     This boxing however means that it can 'safely' be used in generic contexts, as once created object.
  *     It does not have the other erasure related drawbacks of `Opt`.
  *
  * In cases where the wrapped type is expected to (often) be a value type, `Unsure` is a safer option,
  * without the risks and headaches of `Opt`, especially for longer-living objects or when the programmer doesn't have
  * the full control of how and where the created instances are used. On the other hand, for short-living objects
  * which are likely to be used as parameters/return values of a single method call, such as `unapply` methods,
  * as well as when the wrapped type is known or realistically expected to be a reference type, `Opt` will generally
  * be the better choice. Both however should be used only when many objects are created in succession,
  * or when reducing the garbage collector pauses is a priority, or when the values do not escape the scope
  * of a single class, as in most cases the benefits are outweighed by the drawbacks of potential compatibility
  * issues due to using a non-standard solution.
  *
	* @define Ref `Unsure`
  * @author Marcin Mościcki
  */
sealed trait Unsure[@specialized(SpecializedVars) +T]
	extends Ref[T] with FinalRef[T] with IterableOnce[T] with Product with Serializable
{
	/** Tests if this `Unsure` does not contain a value (is equal to [[net.noresttherein.sugar.vars.Missing Missing]]). */
	@inline final override def isEmpty :Boolean = this eq Missing

	/** Tests if this `Unsure` contains a value. If true, `get` will not throw an exception.
		* Equivalent to [[net.noresttherein.sugar.vars.Unsure.isDefined isDefined]].
		*/
	@inline final override def nonEmpty :Boolean = this ne Missing

	/** Tests if this `Unsure` contains a value (is a [[net.noresttherein.sugar.vars.Sure Sure]]).
		* Equivalent to [[net.noresttherein.sugar.vars.Unsure.nonEmpty nonEmpty]].
		*/
	@inline final override def isDefined :Boolean = this ne Missing

	@inline final override def knownSize :Int = if (this eq Missing) 0 else 1

	@inline final override def productArity :Int = if (this eq Missing) 0 else 1

	@inline final override def productElement(n :Int) :Any =
		if (n == 1 && (this ne Missing)) get
		else throw new IndexOutOfBoundsException(toString + ".productElement(" + n + ")")

	/** Forces extraction of the value.
	  * @return contained value, if this is a [[net.noresttherein.sugar.vars.Sure Sure]].
	  * @throws NoSuchElementException if this `Unsure` is empty.
	  */
	override def get :T

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline final def getOrElse[O >: T](alt: => O) :O =
		if (this eq Missing) alt else get

	/** Similarly to [[net.noresttherein.sugar.vars.Unsure.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty.
	  */
	@inline final def orDefault[O >: T](alt: O) :O =
		if (this eq Missing) alt else get

	/** Assuming that `T` is a nullable type, return `null` if this `Unsure` is empty, or the wrapped value otherwise. */
	@inline final def orNull[O >: T](implicit isNullable :Null <:< O) :O =
		if (this eq Missing) null.asInstanceOf[O] else get


	/** Gets the value of this instance or throws the exception given as the type parameter with the given message.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Unsure.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Unsure.orIllegal orIllegal]]
	  */
	@inline final def orThrow[E <: Throwable :ClassTag](msg: => String) :T =
		if (this eq Missing) raise[E](msg) else get

	/** Gets the value of this instance or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Unsure.orThrow orThrow]]
	  */
	@inline final def orNoSuch(msg: => String) :T =
		if (this eq Missing) throw new NoSuchElementException(msg) else get

	/** Gets the value of this instance or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Unsure.orThrow orThrow]]
	  */
	@inline final def orIllegal(msg: => String) :T =
		if (this eq Missing) throw new IllegalArgumentException(msg) else get

	/** Asserts that this instance is not empty, throwing an `AssertionError` otherwise, and returns its contents. */
	@inline final def orError(msg: => String) :T = {
		assert(this ne Missing, msg)
		get
	}



	/** Returns the value this `Unsure` if it is not empty, or the lazily computed alternative otherwise. */
	@inline final def orElse[O >: T](alt: => Unsure[O]) :Unsure[O] =
		if (this eq Missing) alt else this

	/** Similarly to [[net.noresttherein.sugar.vars.Unsure.orElse orElse]], returns this `Unsure` if it is not empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty.
	  */
	@inline final def ifEmpty[O >: T](alt: Unsure[O]) :Unsure[O] =
		if (this eq Missing) alt else this

	/** Returns this `Unsure` if the condition is false and `Blank` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
	  */
	@inline final def orEmptyIf(condition :Boolean) :Unsure[T] =
		if (condition) Missing else this

	/** Returns this `Unsure` if the condition is true and `Blank` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
	  */
	@inline final def orEmptyUnless(condition :Boolean) :Unsure[T] =
		if (condition) this else Missing



	/** Returns `this` ''iff'' this value is [[net.noresttherein.sugar.vars.Missing Blank]],
	  * or a [[net.noresttherein.sugar.vars.Sure Sure]] containing the result of applying
	  * the given function to its value otherwise.
	  */
	@inline final def map[O](p :T => O) :Unsure[O] =
		if (this eq Missing) Missing else new Sure(p(get))

	/** Applies the given function to the content of this `Unsure` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse alternative`, but in one step.
	  */
	@inline final def mapOrElse[X](f :T => X, alternative: => X) :X =
		if (this eq Missing) alternative else f(get)

	/** Returns the result of applying `f` to the value of this `Unsure` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Unsure.mapOrElse mapOrElse]] instead.
	  *  @param  ifEmpty the expression to evaluate if empty.
	  *  @param  f       the function to apply if nonempty.
	  */
	@inline final def fold[B](ifEmpty: => B)(f: T => B): B =
		if (this eq Missing) ifEmpty else f(get)

	/** Returns the result of applying the given function to the value of this `Unsure` if it is not empty,
	  * or `this` if `this.isEmpty`
	  */
	@inline final def flatMap[O](p :T => Unsure[O]) :Unsure[O] =
		if (this eq Missing) Missing else p(get)

	/** Flattens `Attempt[Attempt[O]]` to a single `Attempt[O]`. */
	@inline final def flatten[O](implicit isAttempt :T <:< Unsure[O]) :Unsure[O] =
		if (this eq Missing) Missing else get

	/** Returns a new `Unsure` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Missing Blank]] otherwise.
	  */
	@inline final def filter(p :T => Boolean) :Unsure[T] =
		if ((this eq Missing) || p(get)) this else Missing

	/** Returns a new `Unsure` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Missing Blank]] otherwise.
	  */
	@inline final def filterNot(p :T => Boolean) :Unsure[T] =
		if ((this eq Missing) || !p(get)) this else Missing

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.Unsure.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions.
	  */
	@inline final def withFilter(p :T => Boolean) :WithFilter[T] = new WithFilter[T](this, p)


	/** Tests if this `Unsure` is not empty and its value is equal to the given argument. */
	@inline final def contains[O >: T](o :O): Boolean =
		(this ne Missing) && (get == o)

	/** Tests if this `Unsure` is not empty and its value satisfies the given predicate. */
	@inline final def exists(p :T => Boolean): Boolean = (this ne Missing) && p(get)

	/** Tests if this `Unsure` contains no value or its value satisfies the given predicate. */
	@inline final def forall(p :T => Boolean): Boolean = (this eq Missing) || p(get)

	/** Executes the given block for this `Unsure`s value if it is not empty. */
	@inline final def foreach[O](f :T => O) :Unit = if (this ne Missing) f(get)

	/** Returns an empty `Unsure` if this `Unsure` is empty the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Unsure`.
	  */
	def collect[O](f :PartialFunction[T, O]) :Unsure[O] =
		if (this eq Missing)
			Missing
		else {
			val res = f.applyOrElse(get, collector)
			if (res.asInstanceOf[AnyRef] eq collector) Missing else new Sure(res)
		}


	/** Returns an `Unsure` formed from the contents of `this` and `that` by combining the corresponding elements
	  * in a pair. If either of the two options is empty, `Blank` is returned.
	  */
	@inline final def zip[O](that :Unsure[O]) :Unsure[(T, O)] =
		if ((this eq Missing) | (that eq Missing)) Missing
		else new Sure((get, that.get))

	/** Converts an `Unsure` of a pair into `Unsure`s of its first and second elements. */
	@inline final def unzip[T1, T2](implicit asPair: T <:< (T1, T2)): (Unsure[T1], Unsure[T2]) =
		if (this eq Missing)
			unzip2Fail
		else {
			val pair = asPair(get)
			(new Sure(pair._1), new Sure(pair._2))
		}

	/** Converts an `Unsure` of a triple into three `Unsure`s, one containing the element from each position
	  * of the triple.
	  */
	final def unzip3[T1, T2, T3](implicit asTriple: T <:< (T1, T2, T3)): (Unsure[T1], Unsure[T2], Unsure[T3]) =
		if (this eq Missing)
			unzip3Fail
		else {
			val triple = asTriple(get)
			(new Sure(triple._1), new Sure(triple._2), new Sure(triple._3))
		}


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline final override def iterator :Iterator[T] =
		if (this eq Missing) Iterator.empty else Iterator.single(get)

	/** Returns `Nil` if this `Unsure` is empty or or `this.get::Nil` otherwise. */
	@inline final def toList :List[T] = if (this eq Missing) Nil else get::Nil

	/** Returns an empty list if this `Unsure` is empty or a single element list with its value otherwise. */
	@inline final def toSeq :Seq[T] = if (this eq Missing) Nil else get::Nil

	/** Returns an empty collection if this `Unsure` is empty or a singleton with its value otherwise. */
	@inline final def toIterable :Iterable[T] = if (this eq Missing) Iterable.empty else Iterable.single(get)

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.Unsure.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	override def option :Option[T] = None //overriden by Sure

	/** Converts this `Unsure` to an `Opt`. Same as [[net.noresttherein.sugar.vars.Unsure.toOpt toOpt]].
		* @return [[net.noresttherein.sugar.vars.Opt.Got Got]]`(this.`[[net.noresttherein.sugar.vars.Unsure.get get]]`)`
		*         if `this.`[[net.noresttherein.sugar.vars.Unsure.nonEmpty nonEmpty]]
		*         or [[net.noresttherein.sugar.vars.Opt.Lack Lack]] otherwise.
		*/
	override def opt :Opt[T] = Lack

	final override def unsure :Unsure[T] = this

	/** Converts this `Unsure` to an `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty.
	  */
	@inline final def toLeft[O](right: => O) :Either[T, O] =
		if (this eq Missing) Right(right) else Left(get)

	/** Converts this `Unsure` to an `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty.
	  */
	@inline final def toRight[O](left: => O) :Either[O, T] =
		if (this eq Missing) Left(left) else Right(get)

	/** Formats this `Unsure` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline final override def mkString(prefix :String) :String =
		if (this eq Missing) prefix + "()" else prefix + "(" + get + ")"

	/** Formats this `Unsure` as either `s"Unsure($get)"` or `"Unsure()"`. */
	@inline final override def mkString :String =
		if (this eq Missing) "Unsure()" else "Unsure(" + get + ")"

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Unsure[_]]
}




/** A factory of `Option`-like values [[net.noresttherein.sugar.vars.Unsure Unsure]]`[T]`, providing additionally
  * implicit conversions to and from [[scala.Option]], [[net.noresttherein.sugar.vars.Opt]] and [[Iterable]].
  */
object Unsure {

	/** An `Unsure` factory which creates [[net.noresttherein.sugar.vars.Sure Sure]]`(value)`
	  * if the argument is not null, and [[net.noresttherein.sugar.vars.Missing Blank]] if it is null.
	  *
	  *  @param value the value.
	  *  @return `if (value == null) Blank else Success(value)`.
	  */
	def apply[@specialized(SpecializedVars) T](value :T) :Unsure[T] =
		if (value == null) Missing else new Sure(value)

	/** Converts the given `Option[T]` into a specialized `Unsure[T]`. */
	def some_?[@specialized(SpecializedVars) T](value :Option[T]) :Unsure[T] =
		if (value.isDefined) new Sure(value.get, value) else Missing

	/** Converts the given `Opt[T]` into a ''specialized'' `Unsure[T]` instance. */
	def got_?[@specialized(SpecializedVars) T](value :Opt[T]) :Unsure[T] =
		if (value.isDefined) new Sure(value.get, cachedOpt = value) else Missing

	/** Converts the given `Option[T]` into a specialized `Unsure[T]`. */
	def fromOption[@specialized(SpecializedVars) T](value: Option[T]): Unsure[T] =
		if (value.isDefined) new Sure(value.get, value) else Missing

	/** Converts the given `Opt[T]` into a ''specialized'' `Unsure[T]` instance. */
	def fromOpt[@specialized(SpecializedVars) T](value: Opt[T]): Unsure[T] =
		if (value.isDefined) new Sure(value.get, cachedOpt = value) else Missing


	/** Returns [[net.noresttherein.sugar.vars.Missing Blank]] in a manner consistent with the collections hierarchy. */
	def empty[T] : Unsure[T] = Missing

	/** When a given condition is true, evaluates the `value` argument and returns
	  * [[net.noresttherein.sugar.vars.Sure Sure]]`(value)`. When the condition is false, `value` is not evaluated
	  * and [[net.noresttherein.sugar.vars.Missing Blank]] is returned.
	  */
	def when[@specialized(SpecializedVars) T](cond: Boolean)(value: => T): Unsure[T] =
		if (cond) new Sure(value) else Missing

	/** Unless a given condition is true, this will evaluate the `value` argument and
	  * return [[net.noresttherein.sugar.vars.Sure Sure]]`(value)`. Otherwise, `value` is not evaluated
	  * and [[net.noresttherein.sugar.vars.Missing Blank]] is returned.
	  */
	@inline def unless[@specialized(SpecializedVars) T](cond: Boolean)(a: => T): Unsure[T] =
		when(!cond)(a)


	/** The for-comprehension facade for [[net.noresttherein.sugar.vars.Unsure Unsure]]`[A]`
	  * which does not evaluate the filter predicate until `map`, `flatMap` or `foreach` is called.
	  */
	class WithFilter[+A](self :Unsure[A], p :A => Boolean) {
		def map[B](f: A => B): Unsure[B] = self filter p map f
		def flatMap[B](f: A => Unsure[B]): Unsure[B] = self filter p flatMap f
		def foreach[U](f: A => U): Unit = self filter p foreach f
		def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
	}



//	/** Provides [[net.noresttherein.sugar.vars.Unsure.OptionToUnsureConverter.toUnsure toUnsure]] extension method
//	  * for any `Option[T]`, converting it to a `Unsure[T]`. A fully implicit conversion is available for import
//	  * as `Unsure.implicits.`[[net.noresttherein.sugar.vars.Unsure.implicits.optionToUnsure optionToUnsure]].
//	  */ //consider: removing it. Duplicated by optional.OptionExtension in optional.explicits
//	implicit class OptionToUnsureConverter[T](private val self :Option[T]) extends AnyVal {
//		/** Converts this option into an `Unsure` wrapping the same type/value. */
//		@inline def toUnsure :Unsure[T] = some_?(self)
//	}
//
//
	/** Optional implicit conversions to/from `Opt`, `Option` and `Iterable`.
	  * They involve boxing and are placed here for explicit importing.
	  */
	object implicits {
		/** An implicit conversion that converts an option to an iterable value. */
		@inline implicit def unsureToIterable[A](opt :Unsure[A]): Iterable[A] = opt.toIterable

		/** An implicit conversion from an `Unsure[A]` to an `Option[A]`.
		  * The results are cached, so repeated conversions of the same instance do not result in boxing.
		  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit.
		  */
		@inline implicit def unsureToOption[T](value :Unsure[T]) :Option[T] = value.option

		/** A nomen omen optional implicit conversion of an `Option[A]` to an `Unsure[A]`.
		  * @see [[net.noresttherein.sugar.optional.OptionExtension]]
		  */ //consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
		@inline implicit def optionToUnsure[@specialized(SpecializedVars) A](opt :Option[A]) :Unsure[A] = some_?(opt)

		/** Wraps any object in a [[net.noresttherein.sugar.vars.Sure Sure]] monad. */
		@inline implicit def sureAny[@specialized(SpecializedVars) A](sure :A) :Sure[A] = Sure(sure)

		/** A nomen omen optional implicit conversion of an `Opt[A]` to a `Unsure[A]`.
		  * @see [[net.noresttherein.sugar.optional.OptionExtension.toUnsure OptionExtension.toUnsure]]
		  */ //consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
		@inline implicit def optToUnsure[@specialized(SpecializedVars) A](opt :Opt[A]) :Unsure[A] = got_?(opt)

		@inline implicit def unsureToOpt[A](opt :Unsure[A]) :Opt[A] = opt.opt
	}

	/** Importing the contents of this object replace all usage of [[Option]]/[[Some]]/[[None]] in the scope with
	  * [[net.noresttherein.sugar.vars.Unsure Unsure]]/[[net.noresttherein.sugar.vars.Sure Sure]]/[[net.noresttherein.sugar.vars.Missing Blank]].
	  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
	  * which allow seamless interoperability with standard Scala APIs.
	  *
	  * Other files which reference classes defined in the import's scope may also
	  */
	object UnsureAsOption {
		type Option[T] = Unsure[T]
		type Some[T]   = Sure[T]

		val Option = Unsure
		val Some   = Sure
		val None = Missing
		//same names as in implicits so if both are imported one shadows the other
		@inline implicit def unsureToOption[T](opt :Unsure[T]) :scala.Option[T] = opt.option
		@inline implicit def optionToUnsure[@specialized(SpecializedVars) T](opt :scala.Option[T]) :Unsure[T] = some_?(opt)

		@inline implicit def someToSure[@specialized(SpecializedVars) T](opt :scala.Some[T]) :Sure[T] =
			new Sure(opt.get, opt)
		@inline implicit def sureToSome[@specialized(SpecializedVars) T](opt :Sure[T]) :scala.Some[T] =
			opt.option.asInstanceOf[scala.Some[T]]

		@inline implicit def noneToMissing(none :scala.None.type): Missing.type = Missing
		@inline implicit def missingToNone(missing: Missing.type) :scala.None.type = scala.None
	}



	private[this] val collect :Any => Any = _ => collect
	private def collector[A] = collect.asInstanceOf[Any => A]

	private val unzip2Fail = (Missing, Missing)
	private val unzip3Fail = (Missing, Missing, Missing)
}






/** An `Unsure[T]` carrying a value, a counterpart of Scala [[Some]].
  * Unlike `Some`, it is specialized and does not involve boxing of built in value types used as contents.
  * @tparam T the content type.
  */
@SerialVersionUID(1L)
final class Sure[@specialized(SpecializedVars) +T] private[vars] //consider: don't these vars have to be @volatile?
                (x :T, private[this] var cachedOption :Option[T] = None, private[this] var cachedOpt :Opt[T] = Lack)
	extends Unsure[T]
{
	/** Same as [[net.noresttherein.sugar.vars.Sure.get get]], for compatibility with [[Some]].
	  * Usage of `sure` instead of `get` or `apply()` guarantees that the receiver is a `Sure` instance
	  * rather than a, possibly empty, [[net.noresttherein.sugar.vars.Unsure Unsure]].
	  * This can prevent sneaky bugs introduced by refactors changing the type of `this` reference.
	  */
	def sure :T = x
	override def get :T = x

	override def option :Option[T] = {
		if (cachedOption eq None)
			cachedOption = Some(value)
		cachedOption
	}
	override def opt :Opt[T] = {
		if (cachedOpt.isEmpty)
			cachedOpt = Got(value)
		cachedOpt
	}

	private[vars] override def isSpecialized :Boolean = getClass != classOf[Sure[Any]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Sure[_] => value == other.value
		case _ => false
	}
	override def hashCode :Int = value.hashCode

	override def toString :String = "Sure(" + x + ")"
}


/** A factory and deconstructor of full (''sure'') [[net.noresttherein.sugar.vars.Unsure Unsure]] instances,
  *  fulfilling the same function as [[scala.Some]].
  */
object Sure {
	/** Constructs a successful `Unsure` containing the given value. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Sure[T] = new Sure(value)

	/** Extracts the value from the given `Unsure` if it is not empty. */
	@inline def unapply[T](value :Unsure[T]) :Unsure[T] = if (value == null) Missing else value
}






/** An empty (''missing'') [[net.noresttherein.sugar.vars.Unsure Unsure]] instance, a counterpart of [[scala.None]]. */
@SerialVersionUID(1L)
case object Missing extends Unsure[Nothing] {
	override def get :Nothing = throw new NoSuchElementException("Missing.get")
	override def opt :Opt[Nothing] = Lack
	override def option :Option[Nothing] = None
}
