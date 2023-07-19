package net.noresttherein.sugar.vars


import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.raise
import net.noresttherein.sugar.vars.Fallible.{Failed, Passed}
import net.noresttherein.sugar.vars.BoolOpt.{Known, NoContent, Unknown, WithFilter}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Potential.{Existent, Inexistent}
import net.noresttherein.sugar.witness.Ignored




/** A specialized `Option`-like value class for `Boolean` values which doesn't box the wrapped value. It behaves
  * exactly like `scala.Option[Boolean]`, but does not require creation of an object under any circumstances,
  * and thus yields performance benefits in tight recursion/loops. As a value class, it has no distinct subclasses
  * for empty and non-empty instances, which results in certain differences from `Option`. Aside from the value inlining,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, because
  * it is implemented internally by a `Long`, `BoolOpt` will clash with an overloaded method accepting `Long`.
  *
  * With a special, empty value [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]],
  * this class implements ternary logic, where if any of the arguments is `Unknown`, the result also becomes unknown.
  * Additionally, for this purpose, constants [[net.noresttherein.sugar.vars.BoolOpt.True True]]
  * and [[net.noresttherein.sugar.vars.BoolOpt.False False]] are defined.
  *
  * @note Using a `BoolOpt` via a super trait, such as [[net.noresttherein.sugar.vars.Ref Ref]]
  *       or [[scala.collection.IterableOnce IterableOnce]], or passing it as a type parameter (in particular, using
  *       it as a function parameter or returned value) will however result in boxing
  *       which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  *       composing several `BoolOpt`s' can result in closures being created (as manual nesting of `flatMap` calls
  *       also can).
  *       For this reason its best to either directly use `isEmpty` and `get` in a conditional expression, 
  *       or use the [[net.noresttherein.sugar.vars.BoolOpt.Known$ Known]] matching pattern, which should, 
  *       by modern compilers, be translated to non-boxing byte code.
  * @see [[net.noresttherein.sugar.vars.BoolOpt.Known$]]
  * @see [[net.noresttherein.sugar.vars.BoolOpt.Unknown]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  * @see [[net.noresttherein.sugar.vars.Opt]]
  * @define Ref `BoolOpt`
  */
@SerialVersionUID(Ver)
class BoolOpt private[BoolOpt](private val x :Int) //private[BoolOpt] to allow inlining of its construction
	extends AnyVal with Ref[Boolean] with IterableOnce[Boolean] with Product with Equals with Serializable
{
	/** A flag member type specifying if the `BoolOpt` is full or empty on type level through refinement.
	  * For example, `opt :BoolOpt { type isEmpty = false }` guarantees that `opt.get` will return a value.
	  * Naturally, this being erased type, this guarantee is weaker than `opt :Some[Boolean]`, because any instance
	  * can be successfully cast to a type with any value for this type.
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.Known!]] */
	type isEmpty <: Boolean with Singleton

	@inline override def isFinal = true

	/** Tests if this `BoolOpt` does not contain a value 
	  * (is equal to [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]]). */
	@inline override def isEmpty :Boolean = x == NoContent

	/** Tests if this `BoolOpt` contains a value. If true, `get` will not throw an exception. */
	@inline override def nonEmpty: Boolean = x != NoContent

	/** Tests if this `BoolOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isConst :Boolean = x != NoContent

	/** Tests if this `BoolOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined :Boolean = x != NoContent

	/** Tests if this `BoolOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefinite :Boolean = x != NoContent

	/** Tests if this `BoolOpt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isFinalizable :Boolean = x != NoContent

	@inline override def knownSize :Int = if (x == NoContent) 0 else 1

	/** Returns `1` if the `BoolOpt` carries a value and `0` otherwise. */
	@inline def size :Int = if (x == NoContent) 0 else 1

	@inline override def productArity :Int = if (x == NoContent) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && x != NoContent) x == 1
		else throw new IndexOutOfBoundsException(toString + ".productElement(" + n + ")")


	/** The wrapped value. It is the same as [[net.noresttherein.sugar.vars.BoolOpt.get get]], but this method
	  * is only possible to call on instances for which it is statically known that the value exists based on
	  * [[net.noresttherein.sugar.vars.BoolOpt.isEmpty! isEmpty]] member type.
	  * @return the contained value. */
	@inline def sure(implicit nonEmpty :this.type <:< Known) :Boolean = 
		if (x == NoContent) throw new NoSuchElementException("Unknown.sure")
		else x == 1

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `BoolOpt` is empty. */
	@inline override def get :Boolean =
		if (x == NoContent) throw new NoSuchElementException("Unknown.get")
		else x == 1

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws NoSuchElementException if this `BoolOpt` is empty. */
	@inline override def value :Boolean =
		if (x == NoContent) throw new NoSuchElementException("Unknown.value")
		else x == 1

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `BoolOpt` is empty. */
	@inline override def const :Boolean =
		if (x == NoContent) throw new UnsupportedOperationException("Unknown.const")
		else x == 1

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `BoolOpt` is empty. */
	@inline override def apply() :Boolean =
		if (x == NoContent) throw new UnsupportedOperationException("Unknown.const")
		else x == 1

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse(or: => Boolean) :Boolean =
		if (x == NoContent) or else x == 1

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: Boolean](or: => O) :O =
		if (x == NoContent) or else x == 1

	/** Similarly to [[net.noresttherein.sugar.vars.BoolOpt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault(or: Boolean) :Boolean =
		if (x == NoContent) or else x == 1

	/** Similarly to [[net.noresttherein.sugar.vars.BoolOpt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault[O >: Boolean](or: O) :O =
		if (x == NoContent) or else x == 1


	/** Gets the element in the `BoolOpt` or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :Boolean =
		if (x == NoContent) raise[E](msg) else x == 1

	/** Gets the element in the `BoolOpt` or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag] :Boolean =
		if (x == NoContent) raise[E] else x == 1

	/** Gets the element in this `BoolOpt` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :Boolean =
		if (x == NoContent) throw new NoSuchElementException(msg) else x == 1

	/** Gets the element in this `BoolOpt` or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orThrow orThrow]] */
	@inline def orNoSuch :Boolean =
		if (x == NoContent) throw new NoSuchElementException else x == 1

	/** Gets the element in this `BoolOpt` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :Boolean =
		if (x == NoContent) throw new IllegalArgumentException(msg) else x == 1

	/** Gets the element in this `BoolOpt` or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.orThrow orThrow]] */
	@inline def orIllegal :Boolean =
		if (x == NoContent) throw new IllegalArgumentException else x == 1

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError(msg: => String) :Boolean = {
		assert(x != NoContent, msg)
		x == 1
	}

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError :Boolean = {
		assert(x != NoContent)
		x == 1
	}



	/** Returns the value this `BoolOpt` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse(or: => BoolOpt) :BoolOpt =
		if (x == NoContent) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.BoolOpt.orElse orElse]], returns this `BoolOpt` if it is not empty
	  * and `or` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def ifEmpty(or: BoolOpt) :BoolOpt =
		if (x == NoContent) or else this

	/** Returns this `BoolOpt` if the condition is false and `Unknown` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyIf(condition :Boolean) :BoolOpt =
		new BoolOpt(if (condition) NoContent else x)

	/** Returns this `BoolOpt` if the condition is true and `Unknown` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyUnless(condition :Boolean) :BoolOpt =
		new BoolOpt(if (condition) x else NoContent)

	/** Negates the value of this `BoolOpt`, preserving the unknown status. */
	@inline def unary_- :BoolOpt =
		new BoolOpt(1 - x | x >>> 31)

	/** A lazy, monadic composition with `other` returning the logical disjunction of the two values.
	  * Note that, even if `this` BoolOpt carries `true`, the argument must still be evaluated;
	  * the laziness involves only `Unknown` values. */
	@inline def ||(other: => BoolOpt) :BoolOpt =
		if (x == NoContent) this else new BoolOpt(x | other.x)

	/** A monadic composition with `other` returning the logical disjunction of the two values. */
	@inline def |(other: BoolOpt) :BoolOpt =
		new BoolOpt(x | other.x)

	/** Maps the value of this `BoolOpt` to its logical disjunction with the argument,
	  * returning `Unknown` if this instance carries no value, in which case the argument is not evaluated. */
	@inline def ||(other: => Boolean)(implicit __ :Ignored) :BoolOpt =
		if (x == NoContent | x == 1 || !other) this else new BoolOpt(1)

	/** Maps the value of this `BoolOpt` to its logical disjunction with the argument,
	  * returning `Unknown` if this instance carries no value. */
	@inline def |(other: Boolean) :BoolOpt =
		new BoolOpt(if (x == NoContent | !other) x else 1)

	/** A lazy, monadic composition with `other` returning the logical conjunction of the two values.
	  * Note that, even if `this` BoolOpt carries `false`, the argument must still be evaluated;
	  * the laziness involves only `Unknown` values. */
	@inline def &&(other: => BoolOpt) :BoolOpt =
		if (x == NoContent)
			this
		else {
			val y = other.x
			new BoolOpt(x & y | y >>> 31)
		}

	/** A monadic composition with `other` returning the logical conjunction of the two values. */
	@inline def &(other: BoolOpt) :BoolOpt =
		new BoolOpt(x & other.x | x >>> 31 | other.x >>> 31)

	/** Maps the value of this `BoolOpt` to its logical conjunction with the argument,
	  * returning `Unknown` if this instance carries no value, in which case the argument is not evaluated. */
	@inline def &&(other: => Boolean)(implicit __ :Ignored) :BoolOpt =
		if (x == NoContent | x == 0 || other) this
		else new BoolOpt(0)

	/** Maps the value of this `BoolOpt` to its logical conjunction with the argument,
	  * returning `Unknown` if this instance carries no value. */
	@inline def &(other: Boolean) :BoolOpt =
		new BoolOpt(if (x == NoContent || other) x else 0)

	/** A lazy, monadic composition with `other`, returning `this.get ^ other.get`, providing both values are non empty.
	  * If this instance is `Unknown`, the argument is not evaluated. */
	@inline def ^^(other: => BoolOpt) :BoolOpt =
		if (x == NoContent)
			this
		else {
			val y = other.x
			new BoolOpt((x ^ y) & 1 | y >>> 31)
		}

	/** A monadic composition with `other`, returning `this.get ^ other.get`, providing both values are non empty. */
	@inline def ^(other: BoolOpt) :BoolOpt =
		new BoolOpt((x ^ other.x) & 1 | x >>> 31 | other.x >>> 31)

	/** Calculates `xor` of this `BoolOpt`'s value and the argument,
	  * returning `Unknown` if this instance carries no value, in which case the argument is not evaluated. */
	@inline def ^^(other: => Boolean)(implicit __ :Ignored) :BoolOpt =
		new BoolOpt(if (x == NoContent || !other) x else 1 - x)

	/** Calculates `xor` of this `BoolOpt`'s value and the argument, returning `Unknown` if this instance is empty. */
	@inline def ^(other: Boolean) :BoolOpt =
		new BoolOpt(if (x == NoContent || !other) x else 1 - x)


	/** Returns a new `BoolOpt` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map(f :Boolean => Boolean) :BoolOpt =
		if (x == NoContent) this else new BoolOpt(if (f(x == 1)) 1 else 0)

	/** Returns a new `Opt` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O](f :Boolean => O) :Opt[O] =
		if (x == NoContent) Lack else Got(f(x == 1))

	/** Applies the given function to the content of this `BoolOpt` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline def mapOrElse(f :Boolean => Boolean, or: => Boolean) :Boolean =
		if (x == NoContent) or else f(x == 1)

	/** Applies the given function to the content of this `BoolOpt` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to
	  * `this.toOpt.`[[net.noresttherein.sugar.collections.Opt.mapOrElse mapOrElse]]`(f, or)`, but in one step. */
	@inline def mapOrElse[O](f :Boolean => O, or: => O) :O =
		if (x == NoContent) or else f(x == 1)

	/** Returns the result of applying `f` to the value of this `BoolOpt` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  *
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.BoolOpt.mapOrElse mapOrElse]] instead.
	  *  @param  ifEmpty the expression to evaluate if empty.
	  *  @param  f       the function to apply if nonempty. */
	@inline def fold[O](ifEmpty: => O)(f: Boolean => O) :O =
		if (x == NoContent) ifEmpty else f(x == 1)

	/** The same as [[net.noresttherein.sugar.vars.BoolOpt.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]] is returned instead. */
	@inline def guardMap(f :Boolean => Boolean) :BoolOpt =
		if (x == NoContent) this
		else new BoolOpt(try if (f(x == 1)) 1 else 0 catch { case _ :Exception => NoContent })

	/** The same as [[net.noresttherein.sugar.vars.BoolOpt.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]] is returned instead. */
	@inline def guardMap[O](f :Boolean => O) :Opt[O] =
		if (x == NoContent)
			Lack
		else try {
			Got(f(x == 1))
		} catch {
			case _ :Exception => Lack
		}

	/** Returns the result of applying the given function to the value of this `BoolOpt` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap(f :Boolean => BoolOpt) :BoolOpt =
		if (x == NoContent) this else f(x == 1)

	/** Returns the result of applying the given function to the value of this `BoolOpt` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap[O](f :Boolean => Opt[O]) :Opt[O] =
		if (x == NoContent) Lack else f(x == 1)

	/** Returns an empty `BoolOpt` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed(value :Boolean) :BoolOpt =
		if (x == (if (value) 1 else 0)) new BoolOpt(NoContent) else this

	/** Returns an empty `BoolOpt` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: Boolean](that :IterableOnce[O]) :BoolOpt = that match {
		case _ if x == NoContent => this
		case it :Set[O] => if (it(x == 1)) new BoolOpt(NoContent) else this
		case it :Ranking[O] => if (it.contains(x == 1)) new BoolOpt(NoContent) else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val o = x == 1
			while (i.hasNext)
				if (i.next() == o)
					return new BoolOpt(NoContent)
			this
	}

	/** Returns a new `BoolOpt` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]] otherwise. */
	@inline def filter(p :Boolean => Boolean) :BoolOpt =
		if ((x == NoContent) || p(x == 1)) this else new BoolOpt(NoContent)

	/** Returns a new `BoolOpt` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]] otherwise. */
	@inline def filterNot(p :Boolean => Boolean) :BoolOpt =
		if ((x == NoContent) || !p(x == 1)) this else new BoolOpt(NoContent)

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.BoolOpt.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :Boolean => Boolean) :WithFilter = new WithFilter(this, p)


	/** Tests if this `BoolOpt` is not empty and its value is equal to the given argument. */
	@inline def contains(o :Boolean): Boolean = x == (if (o) 1 else 0)

	/** Tests if this `BoolOpt` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :Boolean => Boolean): Boolean = (x != NoContent) && p(x == 1)

	/** Tests if this `BoolOpt` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :Boolean => Boolean): Boolean = (x == NoContent) || p(x == 1)

	/** Executes the given block for this `BoolOpt`s value if it is not empty. */
	@inline def foreach[O](f :Boolean => O) :Unit = if (x != NoContent) f(x == 1)

	/** Returns an empty `BoolOpt` if this `BoolOpt` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `BoolOpt`. */
	@inline def collect(f :PartialFunction[Boolean, Boolean]) :BoolOpt =
		if (x == NoContent)
			this
		else
			new BoolOpt(f.andThen(x => if (x) 1 else 0).applyOrElse(x == 1, { x :Boolean => NoContent }))

	/** Returns an empty `Opt` if this `BoolOpt` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Opt`. */
	@inline def collect[O](f :PartialFunction[Int, O]) :Opt[O] =
		if (x == NoContent)
			Lack
		else
			f.asInstanceOf[PartialFunction[Boolean, AnyRef]].applyOrElse(x == 1, Opt.NoContent) match {
				case Opt.NoContent   => Lack
				case o :O @unchecked => Got(o)
			}


	/** Returns an `BoolOpt` formed from the contents of `this` and `that` by combining the corresponding elements in a pair.
	  * If either of the two BoolOptions is empty, `Unknown` is returned. */
	@inline def zip(that :BoolOpt) :Opt[(Boolean, Boolean)] = toOpt.zip(that.toOpt)

	/** Returns an `BoolOpt` formed from the contents of `this` and `that` by combining the corresponding elements
	  * in a pair. If either of the two BoolOptions is empty, `Unknown` is returned. */
	@inline def zip[O](that :Opt[O]) :Opt[(Boolean, O)] = toOpt.zip(that)


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline override def iterator :Iterator[Boolean] =
		if (x == NoContent) Iterator.empty else Iterator.single(x == 1)

	/** Returns `Nil` if this `BoolOpt` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[Boolean] = if (x == NoContent) Nil else (x == 1) :: Nil

	/** Returns an empty list if this `BoolOpt` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[Boolean] = if (x == NoContent) Nil else (x == 1) :: Nil

	/** Returns an empty collection if this `BoolOpt` is empty or a singleton with its value otherwise. */
	@inline def toIterable :Iterable[Boolean] = if (x == NoContent) Iterable.empty else Iterable.single(x == 1)

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.BoolOpt.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def option :Option[Boolean] = if (x == NoContent) None else Some(x == 1)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def toOption :Option[Boolean] = if (x == NoContent) None else Some(x == 1)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def constOption :Option[Boolean] = if (x == NoContent) None else Some(x == 1)

	@inline override def opt      :Opt[Boolean] = if (x == NoContent) Lack else Got(x == 1)
	@inline override def toOpt    :Opt[Boolean] = if (x == NoContent) Lack else Got(x == 1)
	@inline override def constOpt :Opt[Boolean] = if (x == NoContent) Lack else Got(x == 1)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.BoolOpt.toUnsure toUnsure]]. */
	@inline override def unsure :Unsure[Boolean] =
		if (x == NoContent) Missing else new Sure(x == 1)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. */
	@inline override def toUnsure: Unsure[Boolean] =
		if (x == NoContent) Missing else new Sure(x == 1)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any.
      * Same as [[net.noresttherein.sugar.vars.BoolOpt.toUnsure toUnsure]]. */
	@inline override def constUnsure :Unsure[Boolean] =
		if (x == NoContent) Missing else new Sure(x == 1)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.BoolOpt.toPotential toPotential]]. */
	@inline override def potential :Potential[Boolean] = if (x == NoContent) Inexistent else Existent(x == 1)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any. */
	@inline override def toPotential :Potential[Boolean] =
		if (x == NoContent) Inexistent else Existent(x == 1)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.BoolOpt.toPotential toPotential]]. */
	@inline override def constPotential :Potential[Boolean] =
		if (x == NoContent) Inexistent else Existent(x == 1)
//
//	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
//	  * This conversion does not require boxing. */
//	@inline override def ?? :Potential[A] =
//		if (x == NoContent) Inexistent else Existent(x.asInstanceOf[A])


	/** Converts this `BoolOpt` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[Boolean, O] =
		if (x == NoContent) Right(right) else Left(x == 1)

	/** Converts this `BoolOpt` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, Boolean] =
		if (x == NoContent) Left(left) else Right(x == 1)


	/** Converts this `BoolOpt` to `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline def toRed[O](blue: => O) :Pill[Boolean, O] =
		if (x == NoContent) Blue(blue) else Red(x == 1)

	/** Converts this `BoolOpt` to `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline def toBlue[O](red: => O) :Pill[O, Boolean] =
		if (x == NoContent) Red(red) else Blue(x == 1)

	/** Converts this `BoolOpt` to `Fallible`, returning the content as `Passed`,
	  * or the value of the given `String` as `Failed` error message if empty. */
	@inline final def toPassed(err : => String) :Fallible[Boolean] =
		if (x == NoContent) Failed(() => err) else Passed(get)

	/** Formats this `BoolOpt` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline override def mkString(prefix :String) :String =
		if (x == NoContent) prefix + "()" else prefix + "(" + x + ")"

	/** Formats this `BoolOpt` as `s"BoolOpt($get)"` or `"BoolOpt()"`. */
	@inline override def mkString :String = if (x == NoContent) "BoolOpt()" else "BoolOpt(" + x + ")"

	@inline override def toString :String = if (x == NoContent) "Unknown" else "Known(" + x + ")"

	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoolOpt]

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :BoolOpt) :Boolean = x != NoContent & other.x != NoContent && x == other.x

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :BoolOpt) :BoolOpt =
		new BoolOpt(
			if (x == NoContent || other.x == NoContent) NoContent
			else if(x == other.x) 1
			else 0
		)

}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]]s.
  * @see [[net.noresttherein.sugar.vars.BoolOpt.Unknown]]
  * @see [[net.noresttherein.sugar.vars.BoolOpt.Known$]]
  */
@SerialVersionUID(Ver)
object BoolOpt {

	/** Converts the given `Option[Boolean]` into a lighter `BoolOpt`, which is erased at runtime. */
	@inline def some_?(value :Option[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** Converts the given `Opt[Boolean]` into a lighter `BoolOpt`, erased at runtime. */
	@inline def got_?(value :Opt[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** Converts the given `Unsure[Boolean]` into a lighter `BoolOpt`, erased at runtime. */
	@inline def sure_?(value :Unsure[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** Converts the given `Potential[Boolean]` into a lighter `BoolOpt` for interoperability. */
	@inline def existent_?(value :Potential[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** Converts the given `Option[Boolean]` into a lighter `BoolOpt`, erased at runtime. */
	@inline def fromOption(value: Option[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** Converts the given `Unsure[Boolean]` into a lighter `BoolOpt`, erased at runtime. */
	@inline def fromUnsure(value: Unsure[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** Converts the given `Potential[Boolean]` into a lighter `BoolOpt` for interoperability. */
	@inline def fromPotential(value :Potential[Boolean]) :BoolOpt =
		new BoolOpt(
			if (value.isEmpty) NoContent
			else if (value.get) 1
			else 0
		)

	/** When a given condition is true, evaluates the `a` argument and returns `Known(a).`
	  * When the condition is false, `a` is not evaluated and `Unknown` is returned. */
	@inline def when(cond: Boolean)(a: => Boolean): BoolOpt =
		new BoolOpt(
			if (!cond) NoContent
			else if (a) 1
			else 0
		)

	/** Unless a given condition is true, this will evaluate the `a` argument and return `Known(a)`.
	  * Otherwise, `a` is not evaluated and `Unknown` is returned. */
	@inline def unless[A](cond: Boolean)(a: => Boolean): BoolOpt =
		new BoolOpt(
			if (cond) NoContent
			else if (a) 1
			else 0
		)

	/** Executes the given lazy expression in a `try-catch` block, returning `Unknown` in case
	  * any exception is caught. Otherwise the value is returned as a `Known` instance as normal. */
	@inline def guard(a : => Boolean) :BoolOpt =
		try { Known(a) } catch {
			case _ :Exception => Unknown
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `Unknown` in case
	  * any exception is caught. Otherwise the result is returned as a `Known` instance as normal. */
	@inline def guard[A](f :A => Boolean)(a :A) :BoolOpt =
		try { Known(f(a)) } catch {
			case _ :Exception => Unknown
		}

	/** Returns the first argument as `Known` if it satisfies the predicate `p`.
	  * @return `Known(value).filter(p)`.
	  */
	@inline def satisfying(value :Boolean)(p :Boolean => Boolean) :BoolOpt =
		new BoolOpt(
			if (!p(value)) NoContent
			else if (value) 1
			else 0
		)


	/** Equals `Known(true)`. */
	final val True :BoolOpt = new BoolOpt(1)

	/** Equals `Known(false)`. */
	final val False :BoolOpt = new BoolOpt(0)

	/** Returns [[net.noresttherein.sugar.vars.BoolOpt.Unknown Unknown]] - an empty `BoolOpt`. */
	@inline final def empty :BoolOpt = Unknown

	/** A refinement of [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]] marking it through a member flag type
	  * as empty. [[net.noresttherein.sugar.vars.BoolOpt.Unknown BoolOpt.Unknown]] is an instance of this type. */
	type Unknown = BoolOpt { type isEmpty = true }

	/** A special, empty instance of [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]] which conforms to any `BoolOpt[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  * @note This value is of the refined type `BoolOpt { isEmpty = true }`. However, in many circumstances,
	  *       it is preferable to use simply `BoolOpt` for proper type inference.
	  *       In those cases you can use `BoolOpt.empty`.
	  * @see [[net.noresttherein.sugar.vars.BoolOpt.empty]] */
	final val Unknown :Unknown = new BoolOpt(NoContent).asInstanceOf[Unknown]

	/** A refinement of [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]] marking it through a member flag type
	  * as non-empty. [[net.noresttherein.sugar.vars.BoolOpt$ BoolOpt]] factory object creates instances
	  * narrowed down to this type. */
	type Known = BoolOpt { type isEmpty = false }

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]]. */
	@SerialVersionUID(Ver)
	object Known {
		/** Creates a non-empty [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]] wrapping the given value. */
		@inline def apply(x :Boolean) :Known = new BoolOpt(if (x) 1 else 0).asInstanceOf[Known]

		/** Matches non-empty [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]] instances. */
		@inline def unapply(BoolOpt :BoolOpt) :BoolOpt = BoolOpt
	}


	/** The for-comprehension facade for `BoolOpt[A]`, which does not evaluate the filter predicate until
	  * `map`, `flatMap` or `foreach` is called. */
	final class WithFilter(self :BoolOpt, p :Boolean => Boolean) {
		def map(f :Boolean => Boolean) :BoolOpt =
			if (self.x == NoContent) self else new BoolOpt(if (f(self.x == 1)) 1 else 0)
		def map[B](f: Boolean => B): Opt[B] = self filter p map f
		def flatMap(f: Boolean => BoolOpt): BoolOpt = self filter p flatMap f
		def flatMap[B](f: Boolean => Opt[B]): Opt[B] = self filter p flatMap f
		def foreach[U](f: Boolean => U): Unit = self filter p foreach f
		def withFilter(q: Boolean => Boolean): WithFilter = new WithFilter(self, x => p(x) && q(x))
	}



	/** Implicit conversions between `BoolOpt` and `BoolOption`.
	  * Conversions between `BoolOpt` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.implicits implicits]]. */
	@SerialVersionUID(Ver)
	object implicits {
		@inline implicit def BoolOptToOption(opt :BoolOpt) :Option[Boolean] = opt.option
		@inline implicit def BoolOptToIterable(opt :BoolOpt) :Iterable[Boolean] = opt.toIterable

		@inline implicit def BoolOptionToBoolOpt(option :Option[Boolean]) :BoolOpt =
			new BoolOpt(
				if (option.isDefined)
					if (option.get) 1 else 0
				else NoContent
			)

		@inline implicit def BoolOptToOpt(opt :BoolOpt) :Opt[Boolean] = opt.opt
		@inline implicit def OptToBoolOpt(opt :Opt[Boolean]) :BoolOpt =
			new BoolOpt(
				if (opt.isDefined)
					if (opt.get) 1 else 0
				else NoContent
			)

		@inline implicit def BoolOptToPotential(BoolOpt :BoolOpt) :Potential[Boolean] = BoolOpt.potential
		@inline implicit def PotentialToBoolOpt(opt :Potential[Boolean]) :BoolOpt = BoolOpt.fromPotential(opt)

		//consider: placing this also in BoolOptional.extensions (or BoolOptional.implicits)
		/** Implicitly lifts a `Boolean` to [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]]`[T]`. */
		@inline implicit def gotBoolean(x :Int) :Known = new BoolOpt(x).asInstanceOf[Known]
	}


	private[BoolOpt] final val NoContent = -1
}
