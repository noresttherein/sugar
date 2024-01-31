package net.noresttherein.sugar.vars


import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, outOfBounds_!, unsupported_!, raise}
import net.noresttherein.sugar.vars.IntOpt.{AnInt, Content, NoContent, WithFilter}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Opt.One




/** A specialized `Option`-like value class for `Int` values which doesn't box the wrapped value. It behaves
  * exactly like `scala.Option[Int]`, but does not require creation of an object under any circumstances,
  * and thus yields performance benefits in tight recursion/loops. As a value class, it has no distinct subclasses
  * for empty and non-empty instances, which results in certain differences from `Option`. Aside from the value inlining,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, because
  * it is implemented internally by a `Long`, `IntOpt` will clash with an overloaded method accepting `Long`.
  * @note Using an `IntOpt` via a super trait, such as [[net.noresttherein.sugar.vars.Ref Ref]]
  *       or [[scala.collection.IterableOnce IterableOnce]], or passing it as a type parameter (in particular, using
  *       it as a function parameter or returned value) will however result in boxing
  *       which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  *       composing several `IntOpt`s' can result in closures being created (as manual nesting of `flatMap` calls
  *       also can).
  *       For this reason its best to either directly use `isEmpty` and `get` in a conditional expression,
  *       or use the [[net.noresttherein.sugar.vars.Ternary.Known$ Known]] matching pattern, which should,
  *       by modern compilers, be translated to non-boxing byte code.
  * @see [[net.noresttherein.sugar.vars.IntOpt.AnInt$]]
  * @see [[net.noresttherein.sugar.vars.IntOpt.NoInt]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  * @see [[net.noresttherein.sugar.vars.Maybe]]
  * @define Ref `IntOpt`
  * @define ref optional integer
  * @define coll optional `Int`
  */
@SerialVersionUID(Ver)
class IntOpt private[IntOpt](private val x :Long) //private[IntOpt] to allow inlining of its construction
	extends AnyVal with Ref[Int] with IterableOnce[Int] with Product with Equals with Serializable
{
	/** A flag member type specifying if the `IntOpt` is full or empty on type level through refinement.
	  * For example, `opt :IntOpt { type isEmpty = false }` guarantees that `opt.get` will return a value.
	  * Naturally, this being erased type, this guarantee is weaker than `opt :Some[Int]`, because any instance
	  * can be successfully cast to a type with any value for this type.
	  * @see [[net.noresttherein.sugar.vars.IntOpt.AnInt!]] */
	type isEmpty <: Boolean with Singleton

	@inline override def isFinal = true

	/** Tests if this `IntOpt` does not contain a value (is equal to [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]]). */
	@inline override def isEmpty :Boolean = x == NoContent

	/** Tests if this `IntOpt` contains a value. If true, `get` will not throw an exception. */
	@inline override def nonEmpty: Boolean = x != NoContent

	/** Tests if this `IntOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isConst :Boolean = x != NoContent

	/** Tests if this `IntOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined :Boolean = x != NoContent

	/** Tests if this `IntOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefinite :Boolean = x != NoContent

	/** Tests if this `IntOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isFinalizable :Boolean = x != NoContent

	@inline override def knownSize :Int = if (x == NoContent) 0 else 1

	/** Returns `1` if the `IntOpt` carries a value and `0` otherwise. */
	@inline def size :Int = if (x == NoContent) 0 else 1

	@inline override def productArity :Int = if (x == NoContent) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && x != NoContent) x.toInt
		else outOfBounds_!(toString + ".productElement(" + n + ")")


	/** The wrapped value. It is the same as [[net.noresttherein.sugar.vars.IntOpt.get get]], but this method
	  * is only possible to call on instances for which it is statically known that the value exists based on
	  * [[net.noresttherein.sugar.vars.IntOpt.isEmpty! isEmpty]] member type.
	  * @return the contained value. */
	@inline def sure(implicit nonEmpty :this.type <:< AnInt) :Int =
		if (x == NoContent) noSuch_!("NoInt.sure")
		else x.toInt

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `IntOpt` is empty. */
	@inline override def get :Int =
		if (x == NoContent) noSuch_!("NoInt.get")
		else x.toInt

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws NoSuchElementException if this `IntOpt` is empty. */
	@inline override def value :Int =
		if (x == NoContent) noSuch_!("NoInt.value")
		else x.toInt

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `IntOpt` is empty. */
	@inline override def const :Int =
		if (x == NoContent) unsupported_!("NoInt.const")
		else x.toInt

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `IntOpt` is empty. */
	@inline override def apply() :Int =
		if (x == NoContent) unsupported_!("NoInt.const")
		else x.toInt

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse(or: => Int) :Int =
		if (x == NoContent) or else x.toInt

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: Int](or: => O) :O =
		if (x == NoContent) or else x.toInt

	/** Similarly to [[net.noresttherein.sugar.vars.IntOpt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault(or: Int) :Int =
		if (x == NoContent) or else x.toInt

	/** Similarly to [[net.noresttherein.sugar.vars.IntOpt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault[O >: Int](or: O) :O =
		if (x == NoContent) or else x.toInt

//
//	/** Gets the integer in the $Ref or throws the exception given as the argument.
//	  * @see [[net.noresttherein.sugar.vars.IntOpt.orNoSuch orNoSuch]]
//	  * @see [[net.noresttherein.sugar.vars.IntOpt.orIllegal orIllegal]] */
//	@inline def orThrow(e : => Throwable) :Int =
//		if (x == NoContent) throw e else x.toInt

	/** Gets the element in the `IntOpt` or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :Int =
		if (x == NoContent) raise[E](msg) else x.toInt

	/** Gets the element in the `IntOpt` or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag] :Int =
		if (x == NoContent) raise[E] else x.toInt

	/** Gets the element in this `IntOpt` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :Int =
		if (x == NoContent) noSuch_!(msg) else x.toInt

	/** Gets the element in this `IntOpt` or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orThrow orThrow]] */
	@inline def orNoSuch :Int =
		if (x == NoContent) noSuch_! else x.toInt

	/** Gets the element in this `IntOpt` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :Int =
		if (x == NoContent) illegal_!(msg) else x.toInt

	/** Gets the element in this `IntOpt` or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.IntOpt.orThrow orThrow]] */
	@inline def orIllegal :Int =
		if (x == NoContent) illegal_! else x.toInt

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError(msg: => String) :Int = {
		assert(x != NoContent, msg)
		x.toInt
	}

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError :Int = {
		assert(x != NoContent)
		x.toInt
	}



	/** Returns the value this `IntOpt` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse(or: => IntOpt) :IntOpt =
		if (x == NoContent) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.IntOpt.orElse orElse]], returns this `IntOpt` if it is not empty
	  * and `or` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def ifEmpty(or: IntOpt) :IntOpt =
		if (x == NoContent) or else this

	/** Returns this `IntOpt` if the condition is false and `NoInt` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyIf(condition :Boolean) :IntOpt =
		new IntOpt(if (condition) NoContent else x)

	/** Returns this `IntOpt` if the condition is true and `NoInt` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyUnless(condition :Boolean) :IntOpt =
		new IntOpt(if (!condition) NoContent else x)



	/** Returns a new `IntOpt` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map(f :Int => Int) :IntOpt =
		if (x == NoContent) this else new IntOpt(f(x.toInt) & Content)

	/** Returns a new `Maybe` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O](f :Int => O) :Maybe[O] =
		if (x == NoContent) No else Yes(f(x.toInt))

	/** Applies the given function to the content of this `IntOpt` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline def mapOrElse(f :Int => Int, or: => Int) :Int =
		if (x == NoContent) or else f(x.toInt)

	/** Applies the given function to the content of this `IntOpt` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to
	  * `this.toMaybe.`[[net.noresttherein.sugar.vars.Maybe.mapOrElse mapOrElse]]`(f, or)`, but in one step. */
	@inline def mapOrElse[O](f :Int => O, or: => O) :O =
		if (x == NoContent) or else f(x.toInt)

	/** Returns the result of applying `f` to the value of this `IntOpt` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  *
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.IntOpt.mapOrElse mapOrElse]] instead.
	  *  @param  ifEmpty the expression to evaluate if empty.
	  *  @param  f       the function to apply if nonempty. */
	@inline def fold[O](ifEmpty: => O)(f: Int => O) :O =
		if (x == NoContent) ifEmpty else f(x.toInt)

	/** The same as [[net.noresttherein.sugar.vars.IntOpt.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] is returned instead. */
	@inline def guardMap(f :Int => Int) :IntOpt =
		if (x == NoContent) this
		else new IntOpt(try f(x.toInt) & Content catch { case _ :Exception => NoContent })

	/** The same as [[net.noresttherein.sugar.vars.IntOpt.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] is returned instead. */
	@inline def guardMap[O](f :Int => O) :Maybe[O] =
		if (x == NoContent)
			No
		else try {
			Yes(f(x.toInt))
		} catch {
			case _ :Exception => No
		}

	/** Returns the result of applying the given function to the value of this `IntOpt` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap(f :Int => IntOpt) :IntOpt =
		if (x == NoContent) this else f(x.toInt)

	/** Returns the result of applying the given function to the value of this `IntOpt` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap[O](f :Int => Maybe[O]) :Maybe[O] =
		if (x == NoContent) No else f(x.toInt)

	/** Returns an empty `IntOpt` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed(value :Int) :IntOpt =
		if (x == (value & Content)) new IntOpt(NoContent) else this

	/** Returns an empty `IntOpt` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: Int](that :IterableOnce[O]) :IntOpt = that match {
		case _ if x == NoContent => this
		case it :Set[O] => if (it(x.toInt)) new IntOpt(NoContent) else this
		case it :Ranking[O] => if (it.contains(x.toInt)) new IntOpt(NoContent) else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val o = x.toInt
			while (i.hasNext)
				if (i.next() == o)
					return new IntOpt(NoContent)
			this
	}

	/** Returns a new `IntOpt` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] otherwise. */
	@inline def filter(p :Int => Boolean) :IntOpt =
		if ((x == NoContent) || p(x.toInt)) this else new IntOpt(NoContent)

	/** Returns a new `IntOpt` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] otherwise. */
	@inline def filterNot(p :Int => Boolean) :IntOpt =
		if ((x == NoContent) || !p(x.toInt)) this else new IntOpt(NoContent)

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.IntOpt.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :Int => Boolean) :WithFilter = new WithFilter(this, p)


	/** Tests if this `IntOpt` is not empty and its value is equal to the given argument. */
	@inline def contains(o :Int): Boolean = x == (o & Content)

	/** Tests if this `IntOpt` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :Int => Boolean): Boolean = (x != NoContent) && p(x.toInt)

	/** Tests if this `IntOpt` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :Int => Boolean): Boolean = (x == NoContent) || p(x.toInt)

	/** Executes the given block for this `IntOpt`s value if it is not empty. */
	@inline def foreach[O](f :Int => O) :Unit = if (x != NoContent) f(x.toInt)

	/** Returns an empty `IntOpt` if this `IntOpt` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `IntOpt`. */
	@inline def collect(f :PartialFunction[Int, Int]) :IntOpt =
		if (x == NoContent)
			this
		else
			new IntOpt(f.andThen(_ & Content).applyOrElse(x.toInt, { x :Int => NoContent }))

	/** Returns an empty `Maybe` if this `IntOpt` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Maybe`. */
	@inline def collect[O](f :PartialFunction[Int, O]) :Maybe[O] =
		if (x == NoContent)
			No
		else
			f.asInstanceOf[PartialFunction[Int, AnyRef]].applyOrElse(x.toInt, Maybe.NoContent) match {
				case Maybe.NoContent   => No
				case o :O @unchecked => Yes(o)
			}


	/** Returns an `IntOpt` formed from the contents of `this` and `that` by combining the corresponding elements in a pair.
	  * If either of the two IntOptions is empty, `NoInt` is returned. */
	@inline def zip(that :IntOpt) :Maybe[(Int, Int)] = toMaybe.zip(that.toMaybe)

	/** Returns an `IntOpt` formed from the contents of `this` and `that` by combining the corresponding elements
	  * in a pair. If either of the two IntOptions is empty, `NoInt` is returned. */
	@inline def zip[O](that :Maybe[O]) :Maybe[(Int, O)] = toMaybe.zip(that)


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline override def iterator :Iterator[Int] =
		if (x == NoContent) Iterator.empty else Iterator.single(x.toInt)

	/** Returns `Nil` if this `IntOpt` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[Int] = if (x == NoContent) Nil else x.toInt :: Nil

	/** Returns an empty list if this `IntOpt` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[Int] = if (x == NoContent) Nil else x.toInt :: Nil

	/** Returns an empty collection if this `IntOpt` is empty or a singleton with its value otherwise. */
	@inline def toIterable :Iterable[Int] = if (x == NoContent) Iterable.empty else Iterable.single(x.toInt)

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.IntOpt.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def option :Option[Int] = if (x == NoContent) None else Some(x.toInt)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def toOption :Option[Int] = if (x == NoContent) None else Some(x.toInt)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def constOption :Option[Int] = if (x == NoContent) None else Some(x.toInt)

	@inline override def maybe      :Maybe[Int] = if (x == NoContent) No else Yes(x.toInt)
	@inline override def toMaybe    :Maybe[Int] = if (x == NoContent) No else Yes(x.toInt)
	@inline override def maybeConst :Maybe[Int] = if (x == NoContent) No else Yes(x.toInt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.IntOpt.toUnsure toUnsure]]. */
	@inline override def unsure :Unsure[Int] =
		if (x == NoContent) Missing else new Sure(x.toInt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. */
	@inline override def toUnsure: Unsure[Int] =
		if (x == NoContent) Missing else new Sure(x.toInt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any.
      * Same as [[net.noresttherein.sugar.vars.IntOpt.toUnsure toUnsure]]. */
	@inline override def unsureConst :Unsure[Int] =
		if (x == NoContent) Missing else new Sure(x.toInt)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.IntOpt.toOpt toOpt]]. */
	@inline override def opt :Opt[Int] = if (x == NoContent) None else One(x.toInt)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any. */
	@inline override def toOpt :Opt[Int] =
		if (x == NoContent) None else One(x.toInt)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.IntOpt.toOpt toOpt]]. */
	@inline override def constOpt :Opt[Int] =
		if (x == NoContent) None else One(x.toInt)
//
//	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
//	  * This conversion does not require boxing. */
//	@inline override def ?? :Opt[A] =
//		if (x == NoContent) None else One(x.asInstanceOf[A])


	/** Converts this `IntOpt` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[Int, O] =
		if (x == NoContent) Right(right) else Left(x.toInt)

	/** Converts this `IntOpt` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, Int] =
		if (x == NoContent) Left(left) else Right(x.toInt)


	/** Converts this `IntOpt` to `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline def toRed[O](blue: => O) :Pill[Int, O] =
		if (x == NoContent) Blue(blue) else Red(x.toInt)

	/** Converts this `IntOpt` to `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline def toBlue[O](red: => O) :Pill[O, Int] =
		if (x == NoContent) Red(red) else Blue(x.toInt)

	/** Converts this `IntOpt` to `Outcome`, returning the content as `Done`,
	  * or the value of the given `String` as `Failed` error message if empty. */
	@inline def doneOr(err: => String) :Outcome[Int] =
		if (x == NoContent) Failed(() => err) else Done(get)

	/** Converts this `IntOpt` to `Outcome`, returning the content as `Done`,
	  * or the given `Throwable` as an error. */
	@inline def doneOr(err: Throwable) :Outcome[Int] =
		if (x == NoContent) err.asInstanceOf[Outcome[Int]] else Done(get)

	/** Formats this `IntOpt` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline override def mkString(prefix :String) :String =
		if (x == NoContent) prefix + "()" else prefix + "(" + x + ")"

	/** Formats this `IntOpt` as `s"IntOpt($get)"` or `"IntOpt()"`. */
	@inline override def mkString :String = if (x == NoContent) "IntOpt()" else "IntOpt(" + x + ")"

	@inline override def toString :String = if (x == NoContent) "NoInt" else "AnInt(" + x + ")"

	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[IntOpt]

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :IntOpt) :Boolean = x != NoContent & other.x != NoContent && x == other.x

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :IntOpt) :Ternary =
		if (x == NoContent) Ternary.Unknown
		else if (other.x == NoContent) Ternary.Unknown
		else Ternary.Known(x == other.x)
}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.sugar.vars.IntOpt IntOpt]]s.
  * @see [[net.noresttherein.sugar.vars.IntOpt.NoInt]]
  * @see [[net.noresttherein.sugar.vars.IntOpt.AnInt$]]
  */
@SerialVersionUID(Ver)
object IntOpt {
	private final val Content = 0xffffffffL
	private final val NoContent = Long.MinValue

	/** Returns `AnInt` containing the argument.
	  * This is the same as [[net.noresttherein.sugar.vars.IntOpt.AnInt.apply AnInt]]`(value)`,
	  * but the return type is `IntOpt`, rather than `AnInt`.
	  */
	@inline def apply(value :Int) :IntOpt = new IntOpt(value & Content)

	/** Converts the given `Option[Int]` into a specialized `IntOpt`, which is erased at runtime. */
	@inline def some_?(value :Option[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Maybe[Int]` into a specialized `IntOpt`, erased at runtime. */
	@inline def yes_?(value :Maybe[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Unsure[Int]` into a specialized `IntOpt`, erased at runtime. */
	@inline def sure_?(value :Unsure[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Opt[Int]` into a specialized `IntOpt` for interoperability. */
	@inline def one_?(value :Opt[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Maybe[Int]` into a specialized `IntOpt`, erased at runtime. */
	@inline def fromMaybe(value: Maybe[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Option[Int]` into a specialized `IntOpt`, erased at runtime. */
	@inline def fromOption(value: Option[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Unsure[Int]` into a specialized `IntOpt`, erased at runtime. */
	@inline def fromUnsure(value: Unsure[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** Converts the given `Opt[Int]` into a specialized `IntOpt` for interoperability. */
	@inline def fromOpt(value :Opt[Int]) :IntOpt =
		new IntOpt(if (value.isDefined) value.get & Content else NoContent)

	/** When a given condition is true, evaluates the `a` argument and returns `AnInt(a).`
	  * When the condition is false, `a` is not evaluated and `NoInt` is returned. */
	@inline def when(cond: Boolean)(a: => Int): IntOpt =
		new IntOpt(if (cond) a & Content else NoContent)

	/** Unless a given condition is true, this will evaluate the `a` argument and return `AnInt(a)`.
	  * Otherwise, `a` is not evaluated and `NoInt` is returned. */
	@inline def unless[A](cond: Boolean)(a: => Int): IntOpt =
		new IntOpt(if (!cond) a & Content else NoContent)

	/** Executes the given lazy expression in a `try-catch` block, returning `NoInt` in case
	  * any exception is caught. Otherwise the value is returned as a `AnInt` instance as normal. */
	@inline def guard(a : => Int) :IntOpt =
		try { AnInt(a) } catch {
			case _ :Exception => NoInt
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `NoInt` in case
	  * any exception is caught. Otherwise the result is returned as a `AnInt` instance as normal. */
	@inline def guard[A](f :A => Int)(a :A) :IntOpt =
		try { AnInt(f(a)) } catch {
			case _ :Exception => NoInt
		}

	/** Returns the first argument as `AnInt` if it satisfies the predicate `p`.
	  * @return `AnInt(value).filter(p)`.
	  */
	@inline def satisfying(value :Int)(p :Int => Boolean) :IntOpt =
		new IntOpt(if (p(value)) value & Content else NoContent)

	/** Returns the argument as `AnInt` if `value >= 0`. */
	@inline def nonNegative(value :Int) :IntOpt =
		new IntOpt(if (value >= 0) value & Content else NoContent)

	/** Returns [[net.noresttherein.sugar.vars.IntOpt.NoInt NoInt]] - an empty `IntOpt`.
	  * The difference from the former is the wider type of `IntOpt` itself, rather than `NoInt`.
	  */
	@inline final val empty :IntOpt = new IntOpt(NoContent)

	/** A refinement of [[net.noresttherein.sugar.vars.IntOpt IntOpt]] marking it through a member flag type
	  * as empty. [[net.noresttherein.sugar.vars.IntOpt.NoInt IntOpt.NoInt]] is an instance of this type. */
	type NoInt = IntOpt { type isEmpty = true }

	/** A special, empty instance of [[net.noresttherein.sugar.vars.IntOpt IntOpt]] which conforms to any `IntOpt[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  *
	  * @note This value is of a refined type `IntOpt { type isEmpty = true }`. However, in many circumstances,
	  *       it is preferable to have a basic `IntOpt`. In those cases, you can use `IntOpt.empty`.
	  * @see [[net.noresttherein.sugar.vars.IntOpt.empty]] */
	@inline final val NoInt :NoInt = new IntOpt(NoContent).asInstanceOf[NoInt]


	/** A refinement of [[net.noresttherein.sugar.vars.IntOpt IntOpt]] marking it through a member flag type
	  * as non-empty. [[net.noresttherein.sugar.vars.IntOpt$ IntOpt]] factory object creates instances
	  * narrowed down to this type. */
	type AnInt = IntOpt { type isEmpty = false }

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.IntOpt IntOpt]]. */
	@SerialVersionUID(Ver)
	object AnInt {
		/** Creates a non-empty [[net.noresttherein.sugar.vars.IntOpt IntOpt]] wrapping the given value. */
		@inline def apply(x :Int) :AnInt = new IntOpt(x).asInstanceOf[AnInt]

		/** Matches non-empty [[net.noresttherein.sugar.vars.IntOpt IntOpt]] instances. */
		@inline def unapply(IntOpt :IntOpt) :IntOpt = IntOpt
	}

	/** The for-comprehension facade for `IntOpt[A]`, which does not evaluate the filter predicate until
	  * `map`, `flatMap` or `foreach` is called. */
	final class WithFilter(self :IntOpt, p :Int => Boolean) {
		def map(f :Int => Int) :IntOpt = if (self.x == NoContent) self else new IntOpt(f(self.x.toInt) & Content)
		def map[B](f: Int => B): Maybe[B] = self filter p map f
		def flatMap(f: Int => IntOpt): IntOpt = self filter p flatMap f
		def flatMap[B](f: Int => Maybe[B]): Maybe[B] = self filter p flatMap f
		def foreach[U](f: Int => U): Unit = self filter p foreach f
		def withFilter(q: Int => Boolean): WithFilter = new WithFilter(self, x => p(x) && q(x))
	}



	/** Implicit conversions between `IntOpt` and `IntOption`.
	  * Conversions between `IntOpt` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.conversions conversions]]. */
	@SerialVersionUID(Ver)
	object implicits {
		@inline implicit def IntOptToOption(opt :IntOpt) :Option[Int] = opt.option
		@inline implicit def IntOptToIterable(opt :IntOpt) :Iterable[Int] = opt.toIterable

		@inline implicit def IntOptionToIntOpt(option :Option[Int]) :IntOpt =
			new IntOpt(if (option.isDefined) option.get & Content else NoContent)

		@inline implicit def IntOptToMaybe(opt :IntOpt) :Maybe[Int] = opt.maybe
		@inline implicit def MaybeToIntOpt(opt :Maybe[Int]) :IntOpt =
			new IntOpt(if (opt.isDefined) opt.get & Content else NoContent)

		@inline implicit def IntOptToOpt(IntOpt :IntOpt) :Opt[Int] = IntOpt.opt
		@inline implicit def OptToIntOpt(opt :Opt[Int]) :IntOpt = IntOpt.fromOpt(opt)

		/** Implicitly lifts an `Int` to [[net.noresttherein.sugar.vars.IntOpt IntOpt]]`[T]`. */
		@inline implicit def IntToAnInt(x :Int) :AnInt = new IntOpt(x & Content).asInstanceOf[AnInt]
	}
}

