package net.noresttherein.sugar.vars

import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, outOfBounds_!, raise}
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Ref.FinalRef
import net.noresttherein.sugar.vars.Unsure.{WithFilter, collector, unzip2Fail, unzip3Fail}




/** A `@specialized` (for all value types except `Unit`) version of [[scala.Option]]
  * conforming to [[net.noresttherein.sugar.vars.Val Val]] interface.
  * Like `Option`, it has two subclasses: [[net.noresttherein.sugar.vars.Sure Sure]]`[T]` (equivalent of `Some[T]`)
  * and [[net.noresttherein.sugar.vars.Missing Missing]] (equivalent of `None`).
  * The difference from `Option` and its raison d'être is that it does not involve boxing of built-in (or opaque)
  * value types, as long as the type parameter is statically known at creation and usage sites, or the calling code
  * is also specialized for the given value type. This can yield a noticeable performance benefit in scenarios
  * where instances are created and discarded rapidly, such as flat mapping and simple recursion.
  *
  * Like `Option`, it implements [[IterableOnce]]`[T]` and completely parrots full `Option` API, making it a suitable
  * drop in replacement.
  *
  * This type has rivals in this library in the form of value class [[net.noresttherein.sugar.vars.Maybe Maybe]],
  * and type [[net.noresttherein.sugar.vars.Opt Opt]], which are other `Option` replacement candidates,
  * also aiming to reduce boxing. The difference lies in exactly what boxing is avoided:
  *   - when an `Int` or other value type is placed in a Scala `Option`, two objects need to be created:
  *     an `Integer` wrapper, and `Some` itself containing the wrapper.
  *   - `Maybe` is a value class, and in any context in which it doesn't require runtime promotion to a reference type
  *     (use as a type parameter, upcasting to `Any`, putting it into an `Array` or a collection,
  *     returning from a function or passing as an argument to one), it is erased to a reference to the contained object.
  *     However, instances for value types (and other value classes) require boxing to their wrapper types.
  *     For integer types, especially smaller values, `Unit` and `Boolean`, this is partly offset by the runtime
  *     environment's employment of caching of commonly used values. Value classes and floating point numbers
  *     will however require boxing. Another drawback is that its erasure clashes with the wrapped type itself,
  *     preventing overloaded methods, just as compiler bridges required when a method accepting/returning a `Maybe`
  *     overrides a generic method with an abstract type in place of the `Maybe` value clash with the method's erasure.
  *     When an instance is repeatedly passed between contexts in which it is referred to as an abstract type
  *     and where it is erased, this can paradoxically result in significantly more boxing than a once created
  *     and used as-is `Option` would require.
  *   - `Opt` is a type alias erased to `AnyRef`, so all notes about value types needing boxing within `Maybe`
  *     also apply here. Where it has an advantage over the former is that it introduces no boxing class at all
  *     (except for the case of `One(None)`), making it more efficient as array elements
  *     or function parameters and return types. It is however in several regards a rather leaky abstraction.
  *   - `Unsure` does not involve boxing of value types used as type parameters, but is itself a reference type,
  *     so an object will always be created, while `Maybe` can, in some circumstances, avoid boxing altogether.
  *     This boxing however means that it can 'safely' be used in generic contexts, as once created object.
  *     It does not have the other erasure related drawbacks of `Maybe`.
  *
  * In cases where the wrapped type is expected to (often) be a value type, `Unsure` is a safer option,
  * without the risks and headaches of `Maybe`, especially for longer-living objects or when the programmer doesn't have
  * the full control of how and where the created instances are used. On the other hand, for short-living objects
  * which are likely to be used as parameters/return values of a single method call, such as `unapply` methods,
  * as well as when the wrapped type is known or realistically expected to be a reference type, `Maybe` will generally
  * be the better choice. Both however should be used only when many objects are created in succession,
  * or when reducing the garbage collector pauses is a priority, or when the values do not escape the scope
  * of a single class, as in most cases the benefits are outweighed by the drawbacks of potential compatibility
  * issues due to using a non-standard solution.
  *
  * @see [[net.noresttherein.sugar.vars.Opt Opt]]
  * @see [[net.noresttherein.sugar.vars.Maybe Maybe]]
  * @see [[net.noresttherein.sugar.vars.Nullable Nullable]]
  * @see [[net.noresttherein.sugar.vars.IntOpt IntOpt]]
  * @see [[net.noresttherein.sugar.vars.Ternary Ternary]]
  * @define Ref `Unsure`
  * @define ref unsure value
  * @define coll unsure value
  * @author Marcin Mościcki
  */ //Uncertain is a longer name, but Certain is a better name than Sure. Hmm.
sealed trait Unsure[@specialized(SpecializedVars) +T]
	extends Ref[T] with FinalRef[T] with IterableOnce[T] with Product with Serializable
{
	/** Tests if this `Unsure` does not contain a value (is equal to [[net.noresttherein.sugar.vars.Missing Missing]]). */
	@inline final override def isEmpty :Boolean = this eq Missing

	/** Tests if this `Unsure` contains a value (is a [[net.noresttherein.sugar.vars.Sure Sure]]).
	  * Equivalent to [[net.noresttherein.sugar.vars.Unsure.nonEmpty nonEmpty]]. */
	@inline final override def isDefined :Boolean = this ne Missing

	@inline final override def knownSize :Int = if (this eq Missing) 0 else 1

	/** Returns `1` if the `Unsure` carries a value and `0` otherwise. */
	@inline final def size :Int = if (this eq Missing) 0 else 1

	@inline final override def productArity :Int = if (this eq Missing) 0 else 1

	@inline final override def productElement(n :Int) :Any =
		if (n == 1 && (this ne Missing)) get
		else outOfBounds_!(toString + ".productElement(" + n + ")")

	/** Forces extraction of the value.
	  * @return contained value, if this is a [[net.noresttherein.sugar.vars.Sure Sure]].
	  * @throws NoSuchElementException if this `Unsure` is empty. */
	override def get :T

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline final def getOrElse[O >: T](or: => O) :O =
		if (this eq Missing) or else get

	/** Similarly to [[net.noresttherein.sugar.vars.Unsure.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline final def orDefault[O >: T](or: O) :O =
		if (this eq Missing) or else get

	/** Assuming that `T` is a nullable type, return `null` if this `Unsure` is empty, or the wrapped value otherwise. */
	@inline final def orNull[O >: T](implicit isNullable :Null <:< O) :O =
		if (this eq Missing) null.asInstanceOf[O] else get

	/** Gets the value of this instance or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Unsure.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Unsure.orIllegal orIllegal]] */
	@inline final def orThrow[E <: Throwable :ClassTag](msg: => String) :T =
		if (this eq Missing) raise[E](msg) else get

	/** Gets the value of this instance or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Unsure.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Unsure.orIllegal orIllegal]] */
	@inline final def orThrow[E <: Throwable :ClassTag] :T =
		if (this eq Missing) raise[E] else get

	/** Gets the value of this instance or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Unsure.orThrow orThrow]] */
	@inline final def orNoSuch(msg: => String) :T =
		if (this eq Missing) noSuch_!(msg) else get

	/** Gets the value of this instance or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Unsure.orThrow orThrow]] */
	@inline final def orNoSuch :T =
		if (this eq Missing) noSuch_!("Missing") else get

	/** Gets the value of this instance or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.Unsure.orThrow orThrow]] */
	@inline final def orIllegal(msg: => String) :T =
		if (this eq Missing) illegal_!(msg) else get

	/** Gets the value of this instance or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.Unsure.orThrow orThrow]] */
	@inline final def orIllegal :T =
		if (this eq Missing) illegal_!("Missing") else get

	/** Asserts that this instance is not empty, throwing an `AssertionError` otherwise, and returns its contents. */
	@inline final def orError(msg: => String) :T = {
		assert(this ne Missing, msg)
		get
	}

	/** Asserts that this instance is not empty, throwing an `AssertionError` otherwise, and returns its contents. */
	@inline final def orError :T = {
		assert(this ne Missing)
		get
	}



	/** Returns the value this `Unsure` if it is not empty, or the lazily computed alternative otherwise. */
	@inline final def orElse[O >: T](or: => Unsure[O]) :Unsure[O] =
		if (this eq Missing) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.Unsure.orElse orElse]], returns this `Unsure` if it is not empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline final def ifEmpty[O >: T](or: Unsure[O]) :Unsure[O] =
		if (this eq Missing) or else this

	/** Returns this `Unsure` if the condition is false and `Missing` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline final def orEmptyIf(condition :Boolean) :Unsure[T] =
		if (condition) Missing else this

	/** Returns this `Unsure` if the condition is true and `Missing` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline final def orEmptyUnless(condition :Boolean) :Unsure[T] =
		if (condition) this else Missing



	/** Returns `this` ''iff'' this value is [[net.noresttherein.sugar.vars.Missing Missing]],
	  * or a [[net.noresttherein.sugar.vars.Sure Sure]] containing the result of applying
	  * the given function to its value otherwise. */
	@inline final def map[O](p :T => O) :Unsure[O] =
		if (this eq Missing) Missing else new Sure(p(get))


	/** Applies the given function to the content of this `Unsure` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline final def mapOrElse[X](f :T => X, or: => X) :X =
		if (this eq Missing) or else f(get)

	/** Returns the result of applying `f` to the value of this `Unsure` if it is non-empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Unsure.mapOrElse mapOrElse]] instead.
	  * @param  ifEmpty the expression to evaluate if empty.
	  * @param  f       the function to apply if nonempty. */
	@inline final def fold[B](ifEmpty: => B)(f: T => B): B =
		if (this eq Missing) ifEmpty else f(get)

	/** The same as [[net.noresttherein.sugar.vars.Unsure.map map]], but exception thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.Missing Missing]] is returned instead. */
	@inline final def guardMap[B](f :T => B) :Unsure[B] =
		if (this eq Missing)
			Missing
		else try {
			new Sure(f(get))
		} catch {
			case _ :Exception => Missing
		}

	/** Returns `if (`[[net.noresttherein.sugar.vars.Unsure.isDefined isDefined]]`) `[[net.noresttherein.sugar.vars.Missing Missing]]` else p(`[[net.noresttherein.sugar.vars.Unsure.get get]]`)`. */
	@inline final def flatMap[O](p :T => Unsure[O]) :Unsure[O] =
		if (this eq Missing) Missing else p(get)

	/** Flattens `Attempt[Attempt[O]]` to a single `Attempt[O]`. */
	@inline final def flatten[O](implicit isAttempt :T <:< Unsure[O]) :Unsure[O] =
		if (this eq Missing) Missing else get

	/** Returns `Missing` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed[O >: T](o :O) :Unsure[T] =
		if ((this eq Missing) || get == o) Missing else this

	/** Returns `Missing` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: T](that :IterableOnce[O]) :Unsure[T] = that match {
		case _ if this eq Missing => this
		case it :Set[O]     => if (it(get)) Missing else this
		case it :Ranking[O] => if (it.contains(get)) Missing else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val x = get
			while (i.hasNext)
				if (i.next() == x)
					return Missing
			this
	}

	/** Returns a new `Unsure` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Missing Missing]] otherwise. */
	@inline final def filter(p :T => Boolean) :Unsure[T] =
		if ((this eq Missing) || p(get)) this else Missing

	/** Returns a new `Unsure` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Missing Missing]] otherwise. */
	@inline final def filterNot(p :T => Boolean) :Unsure[T] =
		if ((this eq Missing) || !p(get)) this else Missing

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.Unsure.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
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
	  * otherwise applies it and wraps the result it in a new `Unsure`. */
	def collect[O](f :PartialFunction[T, O]) :Unsure[O] =
		if (this eq Missing)
			Missing
		else {
			val res = f.applyOrElse(get, collector)
			if (res.asInstanceOf[AnyRef] eq collector) Missing else new Sure(res)
		}


	/** Returns an `Unsure` formed from the contents of `this` and `that` by combining the corresponding elements
	  * in a pair. If either of the two options is empty, `Missing` is returned. */
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
	  * of the triple. */
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

	/** Returns `Nil` if this `Unsure` is empty or `this.get::Nil` otherwise. */
	@inline final def toList :List[T] = if (this eq Missing) Nil else get::Nil

	/** Returns an empty list if this `Unsure` is empty or a single element list with its value otherwise. */
	@inline final def toSeq :Seq[T] = if (this eq Missing) Nil else get::Nil

	/** Returns an empty collection if this `Unsure` is empty or a singleton with its value otherwise. */
	@inline final def toIterable :Iterable[T] = if (this eq Missing) Iterable.empty else Iterable.single(get)

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.Unsure.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	override def option :Option[T] = None //overridden by Sure

	/** Converts this `Unsure` to a `Maybe`. Same as [[net.noresttherein.sugar.vars.Unsure.toMaybe toMaybe]].
	  * @return [[net.noresttherein.sugar.vars.Maybe.Yes Yes]]`(this.`[[net.noresttherein.sugar.vars.Unsure.get get]]`)`
	  *         if `this.`[[net.noresttherein.sugar.vars.Unsure.nonEmpty nonEmpty]]
	  *         or [[net.noresttherein.sugar.vars.Maybe.No No]] otherwise. */
	override def opt :Opt[T] = None

	final override def unsure :Unsure[T] = this

	/** Converts this `Unsure` to an `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline final def toLeft[O](right: => O) :Either[T, O] =
		if (this eq Missing) Right(right) else Left(get)

	/** Converts this `Unsure` to an `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline final def toRight[O](left: => O) :Either[O, T] =
		if (this eq Missing) Left(left) else Right(get)

	/** Converts this `Unsure` to a `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline final def toRed[O](blue: => O) :Pill[T, O] =
		if (this eq Missing) Blue(blue) else Red(get)

	/** Converts this `Unsure` to a `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline final def toBlue[O](red: => O) :Pill[O, T] =
		if (this eq Missing) Red(red) else Blue(get)

	/** Converts this `Unsure` to `Outcome`, returning the content as `Done`,
	  * or the given `String` as `Failed` error message if empty. */
	@inline final def doneOr(err: => String) :Outcome[T] =
		if (this eq Missing) Failed(() => err) else Done(get)

	/** Converts this `Unsure` to `Outcome`, returning the content as `Done`,
	  * or the given `Throwable` as an error if empty. */
	@inline final def doneOr(err :Throwable) :Outcome[T] =
		if (this eq Missing) Failed(err) else Done(get)

	/** Formats this `Unsure` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline final override def mkString(prefix :String) :String =
		if (this eq Missing) prefix + "()" else prefix + "(" + get + ")"

	/** Formats this `Unsure` as either `s"Unsure($get)"` or `"Unsure()"`. */
	@inline final override def mkString :String =
		if (this eq Missing) "Unsure()" else "Unsure(" + get + ")"

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :Unsure[_]) :Boolean = (this ne Missing) & (other ne Missing) && get == other.get

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :Unsure[_]) :Unsure[Boolean] = (this, other) match {
		case (Sure(a), Sure(b)) => Sure(a == b)
		case _ => Missing
	}

	//consider: allowing them to equal any constant Ref
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Unsure[_]]
}




/** A factory of `Option`-like values [[net.noresttherein.sugar.vars.Unsure Unsure]]`[T]`, providing additionally
  * implicit conversions to and from [[scala.Option]], [[net.noresttherein.sugar.vars.Maybe]] and [[Iterable]].
  */
@SerialVersionUID(Ver)
case object Unsure {

	/** An `Unsure` factory which creates [[net.noresttherein.sugar.vars.Sure Sure]]`(value)`
	  * if the argument is not null, and [[net.noresttherein.sugar.vars.Missing Missing]] if it is null.
	  * @param value the value.
	  * @return `if (value == null) Missing else Success(value)`. */
	def apply[@specialized(SpecializedVars) T](value :T) :Unsure[T] =
		if (value == null) Missing else new Sure(value)

	/** Converts the given `Option[T]` into a specialized `Unsure[T]`. */
	def some_?[@specialized(SpecializedVars) T](value :Option[T]) :Unsure[T] =
		if (value.isDefined) new Sure(value.get, value) else Missing

	/** Converts the given `Maybe[T]` into a ''specialized'' `Unsure[T]` instance. */
	def yes_?[@specialized(SpecializedVars) T](value :Maybe[T]) :Unsure[T] =
		if (value.isDefined) new Sure(value.get, cachedOpt = value.toOpt) else Missing

	/** Converts the given `Opt[T]` into a ''specialized'' `Unsure[T]` instance. */
	def one_?[@specialized(SpecializedVars) T](value :Opt[T]) :Unsure[T] =
		if (value.isDefined) new Sure(value.get, cachedOpt = value) else Missing

	/** Converts the given `Option[T]` into a specialized `Unsure[T]`. */
	def fromOption[@specialized(SpecializedVars) T](value: Option[T]): Unsure[T] =
		if (value.isDefined) new Sure(value.get, value) else Missing

	/** Converts the given `Maybe[T]` into a ''specialized'' `Unsure[T]` instance. */
	def fromMaybe[@specialized(SpecializedVars) T](value: Maybe[T]): Unsure[T] =
		if (value.isDefined) new Sure(value.get, cachedOpt = value.toOpt) else Missing

	/** Converts the given `Opt[T]` into a ''specialized'' `Unsure[T]` instance. */
	def fromOpt[@specialized(SpecializedVars) T](value: Opt[T]): Unsure[T] =
		if (value.isDefined) new Sure(value.get, cachedOpt = value) else Missing


	/** Returns [[net.noresttherein.sugar.vars.Missing Missing]] in a manner consistent with the collections' hierarchy. */
	def empty[T] : Unsure[T] = Missing

	/** When a given condition is true, evaluates the `value` argument and returns
	  * [[net.noresttherein.sugar.vars.Sure Sure]]`(value)`. When the condition is false, `value` is not evaluated
	  * and [[net.noresttherein.sugar.vars.Missing Missing]] is returned. */
	def when[@specialized(SpecializedVars) T](cond: Boolean)(value: => T): Unsure[T] =
		if (cond) new Sure(value) else Missing

	/** Unless a given condition is true, this will evaluate the `value` argument and
	  * return [[net.noresttherein.sugar.vars.Sure Sure]]`(value)`. Otherwise, `value` is not evaluated
	  * and [[net.noresttherein.sugar.vars.Missing Missing]] is returned. */
	@inline def unless[@specialized(SpecializedVars) T](cond: Boolean)(a: => T): Unsure[T] =
		when(!cond)(a)

	/** Executes the given lazy expression in a `try-catch` block, returning `Missing` in case
	  * any exception is caught. Otherwise, the value is returned in a `Sure` instance as normal. */
	@inline def guard[A](a : => A) :Unsure[A] =
		try { new Sure(a) } catch {
			case _ :Exception => Missing
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `Missing` in case
	  * any exception is caught. Otherwise, the result is returned in a `Sure` instance as normal. */
	@inline def guard[A, B](f : A => B)(a :A) :Unsure[B] =
		try { new Sure(f(a)) } catch {
			case _ :Exception => Missing
		}

	/** Returns the first argument as `Sure` if it satisfies the predicate `p`.
	  * @return `Sure(value).filter(p)`. */
	@inline def satisfying[A](value :A)(p :A => Boolean) :Unsure[A] =
		if (p(value)) Sure(value) else Missing

	/** The for-comprehension facade for [[net.noresttherein.sugar.vars.Unsure Unsure]]`[A]`
	  * which does not evaluate the filter predicate until `map`, `flatMap` or `foreach` is called.
	  */
	class WithFilter[+A](self :Unsure[A], p :A => Boolean) {
		def map[B](f: A => B): Unsure[B] = self filter p map f
		def flatMap[B](f: A => Unsure[B]): Unsure[B] = self filter p flatMap f
		def foreach[U](f: A => U): Unit = self filter p foreach f
		def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
	}



	/** Optional implicit conversions to/from `Maybe`, `Option` and `Iterable`.
	  * They involve boxing and are placed here for explicit importing.
	  */
	@SerialVersionUID(Ver)
	object conversions {
		/** An implicit conversion that converts an option to an iterable value. */
		@inline implicit def UnsureToIterable[A](opt :Unsure[A]): Iterable[A] = opt.toIterable

		/** An implicit conversion from an `Unsure[A]` to an `Option[A]`.
		  * The results are cached, so repeated conversions of the same instance do not result in boxing.
		  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit. */
		@inline implicit def UnsureToOption[T](value :Unsure[T]) :Option[T] = value.option

		/** A nomen omen optional implicit conversion of an `Option[A]` to an `Unsure[A]`.
		  * @see [[net.noresttherein.sugar.optional.extensions.OptionExtension]] */
		@inline implicit def OptionToUnsure[@specialized(SpecializedVars) A](opt :Option[A]) :Unsure[A] = some_?(opt)

		/** Wraps any object in a [[net.noresttherein.sugar.vars.Sure Sure]] monad. */
		@inline implicit def sureAny[@specialized(SpecializedVars) A](sure :A) :Sure[A] = Sure(sure)

		/** A nomen omen optional implicit conversion of a `Maybe[A]` to a `Unsure[A]`.
		  * @see [[net.noresttherein.sugar.optional.extensions.OptionExtension.toUnsure OptionExtension.toUnsure]] */
		@inline implicit def MaybeToUnsure[@specialized(SpecializedVars) A](opt :Maybe[A]) :Unsure[A] = yes_?(opt)

		@inline implicit def UnsureToMaybe[A](opt :Unsure[A]) :Maybe[A] = opt.maybe

		@inline implicit def OptToUnsure[@specialized(SpecializedVars) A](opt :Opt[A]) :Unsure[A] =
			fromOpt(opt)

		@inline implicit def OptFromUnsure[A](opt :Unsure[A]) :Opt[A] = Opt.fromUnsure(opt)
	}

	/** Importing the contents of this object replace all usage of [[Option]]/[[Some]]/[[None]] in the scope with
	  * [[net.noresttherein.sugar.vars.Unsure Unsure]]/[[net.noresttherein.sugar.vars.Sure Sure]]/[[net.noresttherein.sugar.vars.Missing Missing]].
	  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
	  * which allow seamless interoperability with standard Scala APIs.
	  *
	  * While the idea of using it in production code should be carefully reconsidered,
	  * it is useful for quickly checking during profiling what impact using `Option` vs `Maybe` has.
	  *
	  * Other files which reference classes defined in the import's scope may also
	  */
	@SerialVersionUID(Ver)
	object UnsureAsOption {
		type Option[T] = Unsure[T]
		type Some[T]   = Sure[T]

		val Option = Unsure
		val Some   = Sure
		val None = Missing
		//same names as in implicits so if both are imported one shadows the other
		@inline implicit def UnsureToOption[T](opt :Unsure[T]) :scala.Option[T] = opt.option
		@inline implicit def OptionToUnsure[@specialized(SpecializedVars) T](opt :scala.Option[T]) :Unsure[T] = some_?(opt)

		@inline implicit def SomeToSure[@specialized(SpecializedVars) T](opt :scala.Some[T]) :Sure[T] =
			new Sure(opt.get, opt)
		@inline implicit def sureToSome[@specialized(SpecializedVars) T](opt :Sure[T]) :scala.Some[T] =
			opt.option.asInstanceOf[scala.Some[T]]

		@inline implicit def NoneToMissing(none :scala.None.type): Missing.type = Missing
		@inline implicit def MissingToNone(missing: Missing.type) :scala.None.type = scala.None
	}



	private[this] val collect :Any => Any = _ => collect
	private def collector[A] = collect.asInstanceOf[Any => A]

	private val unzip2Fail = (Missing, Missing)
	private val unzip3Fail = (Missing, Missing, Missing)
}






/** An `Unsure[T]` carrying a value, a counterpart of Scala [[Some]].
  * Unlike `Some`, it is specialized and does not involve boxing of built-in value types used as contents.
  * @tparam T the content type.
  */
@SerialVersionUID(Ver) //todo: stop using default arguments, they involve additional method calls.
final class Sure[@specialized(SpecializedVars) +T] private[vars] //consider: don't these vars have to be @volatile?
                (x :T, private[this] var cachedOption :Option[T] = None, private[this] var cachedOpt :Opt[T] = None)
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
			cachedOption = Some(x)
		cachedOption
	}
	override def opt :Opt[T] = {
		if (cachedOpt.isEmpty)
			cachedOpt = One(value)
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
  * fulfilling the same function as [[scala.Some]].
  */
@SerialVersionUID(Ver)
object Sure {
	/** Constructs a successful `Unsure` containing the given value. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Sure[T] = new Sure(value)

	/** Extracts the value from the given `Unsure` if it is not empty. */
	@inline def unapply[T](value :Unsure[T]) :Unsure[T] = if (value == null) Missing else value
}






/** An empty (''missing'') [[net.noresttherein.sugar.vars.Unsure Unsure]] instance, a counterpart of [[scala.None]]. */
@SerialVersionUID(Ver)
case object Missing extends Unsure[Nothing] {
	override def get :Nothing = noSuch_!("Missing.get")
	override def opt :Opt[Nothing] = None
	override def option :Option[Nothing] = None
}
