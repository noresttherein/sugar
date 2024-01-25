package net.noresttherein.sugar.vars


import scala.annotation.switch
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.exceptions.raise
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Potential.{Existent, Inexistent}
import net.noresttherein.sugar.vars.Ternary.{Known, No, NoContent, WithFilter, Yes}
import net.noresttherein.sugar.witness.Ignored




/** A specialized `Option`-like value class for `Boolean` values which doesn't box the wrapped value. It behaves
  * exactly like `scala.Option[Boolean]`, but does not require creation of an object under any circumstances,
  * and thus yields performance benefits in tight recursion/loops. As a value class, it has no distinct subclasses
  * for empty and non-empty instances, which results in certain differences from `Option`. Aside from value inlining,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, because
  * it is implemented internally by a `Int`, `Ternary` will clash with an overloaded method accepting `Int`.
  *
  * With 'empty' value in `Ternary.`[[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]], it lifts the wrapped
  * `Boolean` value to ternary logic, and provides implementation of the same logic operators.
  * Additionally, for this purpose, constants [[net.noresttherein.sugar.vars.Ternary.True True]]
  * and [[net.noresttherein.sugar.vars.Ternary.False False]] are defined, wrapping `true` and `false`, respectively.
  *
  * @note Using a `Ternary` via a super trait, such as [[net.noresttherein.sugar.vars.Ref Ref]]
  *       or [[scala.collection.IterableOnce IterableOnce]], or passing it as a type parameter (in particular, using
  *       it as a function parameter or returned value) will however result in boxing
  *       which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  *       composing several `Ternary`s' can result in closures being created (as manual nesting of `flatMap` calls
  *       also can). For this reason its best to either directly use `isEmpty` and `get` in a conditional expression,
  *       or use the [[net.noresttherein.sugar.vars.Ternary.Known$ Known]] matching pattern, which should,
  *       by modern compilers, be translated to non-boxing byte code.
  * @see [[net.noresttherein.sugar.vars.Ternary.Known$]]
  * @see [[net.noresttherein.sugar.vars.Ternary.Unknown]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  * @see [[net.noresttherein.sugar.vars.Opt]]
  * @define Ref  `Ternary`
  * @define ref  ternary value
  * @define coll optional `Boolean`
  */
@SerialVersionUID(Ver)
class Ternary private[Ternary](private val x :Int) //private[Ternary] to allow inlining of its construction
	extends AnyVal with Ref[Boolean] with IterableOnce[Boolean] with Product with Equals with Serializable
{
	/** A flag member type specifying if the `Ternary` is full or empty on type level through refinement.
	  * For example, `opt :`$Ref` { type isEmpty = false }` guarantees that `opt.get` will return a value.
	  * Naturally, this being erased type, this guarantee is weaker than `opt :Some[Boolean]`, because any instance
	  * can be successfully cast to a type with any value for this type.
	  * @see [[net.noresttherein.sugar.vars.Ternary.Known!]] */
	type isEmpty <: Boolean with Singleton

	/** `True` if this value is either `true` or `false`. Same as `nonEmpty`, `isDefined`, `isDefinite`. */
	@inline def isKnown :Boolean = x != NoContent

	/** True if this value is neither `true` nor `false`
	  * (equals [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]]). Same as `isEmpty`.
	  */
	@inline def isUnknown :Boolean = x == NoContent

	@inline override def isFinal = true

	/** Tests if this `Ternary` does not contain a value
	  * (is equal to [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]]). */
	@inline override def isEmpty :Boolean = x == NoContent

	/** Tests if this `Ternary` contains a value. If true, `get` will not throw an exception. */
	@inline override def nonEmpty: Boolean = x != NoContent

	/** Tests if this `Ternary` contains a value. This is the same as `nonEmpty`. */
	@inline override def isConst :Boolean = x != NoContent

	/** Tests if this `Ternary` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined :Boolean = x != NoContent

	/** Tests if this `Ternary` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefinite :Boolean = x != NoContent

	/** Tests if this `Ternary` contains a value. This is the same as `nonEmpty`. */
	@inline override def isFinalizable :Boolean = x != NoContent

	@inline override def knownSize :Int = if (x == NoContent) 0 else 1

	/** Returns `1` if the `Ternary` carries a value and `0` otherwise. */
	@inline def size :Int = if (x == NoContent) 0 else 1

	@inline override def productArity :Int = if (x == NoContent) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && x != NoContent) x == Yes
		else throw new IndexOutOfBoundsException(mkString + ".productElement(" + n + ")")


	/** The wrapped value. It is the same as [[net.noresttherein.sugar.vars.Ternary.get get]], but this method
	  * is only possible to call on instances for which it is statically known that the value exists based on
	  * [[net.noresttherein.sugar.vars.Ternary.isEmpty! isEmpty]] member type.
	  * @return the contained value. */
	@inline def sure(implicit nonEmpty :this.type <:< Known) :Boolean = x match {
		case Yes => true
		case No  => false
		case _   => throw new NoSuchElementException("Unknown.sure")
	}

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Ternary` is empty. */
	@inline override def get :Boolean = x match {
		case Yes => true
		case No  => false
		case _   => throw new NoSuchElementException("Unknown.get")
	}

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Ternary` is empty. */
	@inline override def value :Boolean = x match {
		case Yes => true
		case No => false
		case _ => throw new NoSuchElementException("Unknown.value")
	}

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws UnsupportedOperationException if this `Ternary` is empty. */
	@inline override def const :Boolean = x match {
		case Yes => true
		case No  => false
		case _   => throw new NoSuchElementException("Unknown.const")
	}

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws UnsupportedOperationException if this `Ternary` is empty. */
	@inline override def apply() :Boolean = x match {
		case Yes => true
		case No => false
		case _ => throw new NoSuchElementException("Unknown.get")
	}

	/** Converts this ternary value to a `Boolean`, or throws an `UnsupportedOperationException` if it is `Unknown`. */
	@inline def toBoolean :Boolean = x match {
		case Yes => true
		case No  => false
		case _   => throw new UnsupportedOperationException("Unknown.toBoolean")
	}

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse(or: => Boolean) :Boolean =
		if (x == NoContent) or else x == Yes

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: Boolean](or: => O) :O =
		if (x == NoContent) or else x == Yes

	/** Similarly to [[net.noresttherein.sugar.vars.Ternary.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault(or: Boolean) :Boolean =
		if (x == NoContent) or else x == Yes

	/** Similarly to [[net.noresttherein.sugar.vars.Ternary.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault[O >: Boolean](or: O) :O =
		if (x == NoContent) or else x == Yes

//
//	/** Gets the `Boolean` in the $Ref or throws the exception given as the argument.
//	  * @see [[net.noresttherein.sugar.vars.Ternary.orNoSuch orNoSuch]]
//	  * @see [[net.noresttherein.sugar.vars.Ternary.orIllegal orIllegal]] */
//	@inline def orThrow(e : => Throwable) :Boolean =
//		if (x == NoContent) throw e else x == True

	/** Gets the element in the `Ternary` or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Ternary.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Ternary.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :Boolean =
		if (x == NoContent) raise[E](msg) else x == Yes

	/** Gets the element in the `Ternary` or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Ternary.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Ternary.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag] :Boolean =
		if (x == NoContent) raise[E] else x == Yes

	/** Gets the element in this `Ternary` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Ternary.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :Boolean =
		if (x == NoContent) throw new NoSuchElementException(msg) else x == Yes

	/** Gets the element in this `Ternary` or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Ternary.orThrow orThrow]] */
	@inline def orNoSuch :Boolean =
		if (x == NoContent) throw new NoSuchElementException else x == Yes

	/** Gets the element in this `Ternary` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Ternary.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :Boolean =
		if (x == NoContent) throw new IllegalArgumentException(msg) else x == Yes

	/** Gets the element in this `Ternary` or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.Ternary.orThrow orThrow]] */
	@inline def orIllegal :Boolean =
		if (x == NoContent) throw new IllegalArgumentException else x == Yes

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError(msg: => String) :Boolean = {
		assert(x != NoContent, msg)
		x == Yes
	}

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError :Boolean = {
		assert(x != NoContent)
		x == Yes
	}



	/** Returns the value this `Ternary` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse(or: => Ternary) :Ternary =
		if (x == NoContent) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.Ternary.orElse orElse]], returns this `Ternary` if it is not empty
	  * and `or` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def ifEmpty(or: Ternary) :Ternary =
		if (x == NoContent) or else this

	/** Returns this `Ternary` if the condition is false and `Unknown` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyIf(condition :Boolean) :Ternary =
		new Ternary(if (condition) NoContent else x)

	/** Returns this `Ternary` if the condition is true and `Unknown` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyUnless(condition :Boolean) :Ternary =
		new Ternary(if (condition) x else NoContent)

	/** Negates the value of this `Ternary`, preserving the unknown status. */
	@inline def unary_! :Ternary =
		new Ternary((x : @switch) match {
			case Yes => No
			case No  => Yes
			case _   => x
		})

	/** A lazy, monadic composition with `other` returning the logical disjunction of the two values.
	  * Note that, even if `this` Ternary carries `true`, the argument must still be evaluated;
	  * the laziness involves only `Unknown` values. */
	@inline def ||(other: => Ternary) :Ternary =
		if (x == NoContent) this else new Ternary(x | other.x)

	/** A monadic composition with `other` returning the logical disjunction of the two values. */
	@inline def |(other: Ternary) :Ternary =
		new Ternary(x | other.x)

	/** Maps the value of this `Ternary` to its logical disjunction with the argument,
	  * returning `Unknown` if this instance carries no value, in which case the argument is not evaluated. */
	@inline def ||(other: => Boolean)(implicit __ :Ignored) :Ternary =
		if (x != No || !other) this else new Ternary(Yes)

	/** Maps the value of this `Ternary` to its logical disjunction with the argument,
	  * returning `Unknown` if this instance carries no value. */
	@inline def |(other: Boolean) :Ternary =
		new Ternary(if (x == NoContent | !other) x else Yes)

	/** A lazy, monadic composition with `other` returning the logical conjunction of the two values.
	  * Note that, even if `this` Ternary carries `false`, the argument must still be evaluated;
	  * the laziness involves only `Unknown` values. */
	@inline def &&(other: => Ternary) :Ternary =
		if (x == NoContent)
			this
		else {
			val y = other.x
			new Ternary(x & y | y >> 31)
		}

	/** A monadic composition with `other` returning the logical conjunction of the two values. */
	@inline def &(other: Ternary) :Ternary =
		new Ternary(x & other.x | x >> 31 | other.x >> 31)

	/** Maps the value of this `Ternary` to its logical conjunction with the argument,
	  * returning `Unknown` if this instance carries no value, in which case the argument is not evaluated. */
	@inline def &&(other: => Boolean)(implicit __ :Ignored) :Ternary =
		if (x != Yes || other) this else new Ternary(No)

	/** Maps the value of this `Ternary` to its logical conjunction with the argument,
	  * returning `Unknown` if this instance carries no value. */
	@inline def &(other: Boolean) :Ternary =
		new Ternary(if (x == NoContent || other) x else No)

	/** A lazy, monadic composition with `other`, returning `this.get ^ other.get`, providing both values are non empty.
	  * If this instance is `Unknown`, the argument is not evaluated. */
	@inline def ^^(other: => Ternary) :Ternary =
		if (x == NoContent)
			this
		else {
			val y = other.x
			new Ternary((x ^ y) & 1 | y >> 31)
		}

	/** A monadic composition with `other`, returning `this.get ^ other.get`, providing both values are non empty. */
	@inline def ^(other: Ternary) :Ternary =
		new Ternary((x ^ other.x) & 1 | x >> 31 | other.x >> 31)

	/** Calculates `xor` of this `Ternary`'s value and the argument,
	  * returning `Unknown` if this instance carries no value, in which case the argument is not evaluated. */
	@inline def ^^(other: => Boolean)(implicit __ :Ignored) :Ternary =
		new Ternary(if (x == NoContent || !other) x else 1 - x)

	/** Calculates `xor` of this `Ternary`'s value and the argument, returning `Unknown` if this instance is empty. */
	@inline def ^(other: Boolean) :Ternary =
		new Ternary(if (x == NoContent || !other) x else 1 - x)


	/** Returns a new `Ternary` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map(f :Boolean => Boolean) :Ternary =
		if (x == NoContent) this else new Ternary(if (f(x == Yes)) Yes else No)

	/** Returns a new `Opt` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O](f :Boolean => O) :Opt[O] =
		if (x == NoContent) Lack else Got(f(x == Yes))

	/** Applies the given function to the content of this `Ternary` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline def mapOrElse(f :Boolean => Boolean, or: => Boolean) :Boolean =
		if (x == NoContent) or else f(x == Yes)

	/** Applies the given function to the content of this `Ternary` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to
	  * `this.toOpt.`[[net.noresttherein.sugar.collections.Opt.mapOrElse mapOrElse]]`(f, or)`, but in one step. */
	@inline def mapOrElse[O](f :Boolean => O, or: => O) :O =
		if (x == NoContent) or else f(x == Yes)

	/** Returns the result of applying `f` to the value of this `Ternary` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  *
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Ternary.mapOrElse mapOrElse]] instead.
	  *  @param  ifEmpty the expression to evaluate if empty.
	  *  @param  f       the function to apply if nonempty. */
	@inline def fold[O](ifEmpty: => O)(f: Boolean => O) :O =
		if (x == NoContent) ifEmpty else f(x == Yes)

	/** The same as [[net.noresttherein.sugar.vars.Ternary.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]] is returned instead. */
	@inline def guardMap(f :Boolean => Boolean) :Ternary =
		if (x == NoContent) this
		else new Ternary(try if (f(x == Yes)) Yes else No catch { case _ :Exception => NoContent })

	/** The same as [[net.noresttherein.sugar.vars.Ternary.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]] is returned instead. */
	@inline def guardMap[O](f :Boolean => O) :Opt[O] =
		if (x == NoContent)
			Lack
		else try {
			Got(f(x == Yes))
		} catch {
			case _ :Exception => Lack
		}

	/** Returns the result of applying the given function to the value of this `Ternary` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap(f :Boolean => Ternary) :Ternary =
		if (x == NoContent) this else f(x == Yes)

	/** Returns the result of applying the given function to the value of this `Ternary` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap[O](f :Boolean => Opt[O]) :Opt[O] =
		if (x == NoContent) Lack else f(x == Yes)

	/** Returns an empty `Ternary` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed(value :Boolean) :Ternary =
		if (x == (if (value) Yes else No)) new Ternary(NoContent) else this

	/** Returns an empty `Ternary` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: Boolean](that :IterableOnce[O]) :Ternary = that match {
		case _ if x == NoContent => this
		case it :Set[O] => if (it(x == 1)) new Ternary(NoContent) else this
		case it :Ranking[O] => if (it.contains(x == 1)) new Ternary(NoContent) else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val o = x == 1
			while (i.hasNext)
				if (i.next() == o)
					return new Ternary(NoContent)
			this
	}

	/** Returns a new `Ternary` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]] otherwise. */
	@inline def filter(p :Boolean => Boolean) :Ternary =
		if ((x == NoContent) || p(x == Yes)) this else new Ternary(NoContent)

	/** Returns a new `Ternary` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]] otherwise. */
	@inline def filterNot(p :Boolean => Boolean) :Ternary =
		if ((x == NoContent) || !p(x == Yes)) this else new Ternary(NoContent)

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.Ternary.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :Boolean => Boolean) :WithFilter = new WithFilter(this, p)


	/** Tests if this `Ternary` is not empty and its value is equal to the given argument. */
	@inline def contains(o :Boolean): Boolean = x == (if (o) Yes else No)

	/** Tests if this `Ternary` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :Boolean => Boolean): Boolean = (x != NoContent) && p(x == Yes)

	/** Tests if this `Ternary` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :Boolean => Boolean): Boolean = (x == NoContent) || p(x == Yes)

	/** Executes the given block for this `Ternary`s value if it is not empty. */
	@inline def foreach[O](f :Boolean => O) :Unit = if (x != NoContent) f(x == Yes)

	/** Returns an empty `Ternary` if this `Ternary` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Ternary`. */
	@inline def collect(f :PartialFunction[Boolean, Boolean]) :Ternary =
		if (x == NoContent)
			this
		else
			new Ternary(f.andThen(y => if (y) Yes else No).applyOrElse(x == Yes, { _ :Boolean => NoContent }))

	/** Returns an empty `Opt` if this `Ternary` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Opt`. */
	@inline def collect[O](f :PartialFunction[Int, O]) :Opt[O] =
		if (x == NoContent)
			Lack
		else
			f.asInstanceOf[PartialFunction[Boolean, AnyRef]].applyOrElse(x == Yes, Opt.NoContent) match {
				case Opt.NoContent   => Lack
				case o :O @unchecked => Got(o)
			}


	/** Returns an `Ternary` formed from the contents of `this` and `that` by combining the corresponding elements in a pair.
	  * If either of the two Ternarys is empty, `Unknown` is returned. */
	@inline def zip(that :Ternary) :Opt[(Boolean, Boolean)] = toOpt.zip(that.toOpt)

	/** Returns an `Ternary` formed from the contents of `this` and `that` by combining the corresponding elements
	  * in a pair. If either of the two Ternarys is empty, `Unknown` is returned. */
	@inline def zip[O](that :Opt[O]) :Opt[(Boolean, O)] = toOpt.zip(that)


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline override def iterator :Iterator[Boolean] =
		if (x == NoContent) Iterator.empty else Iterator.single(x == Yes)

	/** Returns `Nil` if this `Ternary` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[Boolean] = if (x == NoContent) Nil else (x == Yes) :: Nil

	/** Returns an empty list if this `Ternary` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[Boolean] = if (x == NoContent) Nil else (x == Yes) :: Nil

	/** Returns an empty collection if this `Ternary` is empty or a singleton with its value otherwise. */
	@inline def toIterable :Iterable[Boolean] = if (x == NoContent) Iterable.empty else Iterable.single(x == Yes)

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.Ternary.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def option :Option[Boolean] = if (x == NoContent) None else Some(x == Yes)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def toOption :Option[Boolean] = if (x == NoContent) None else Some(x == Yes)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def constOption :Option[Boolean] = if (x == NoContent) None else Some(x == Yes)

	@inline override def opt      :Opt[Boolean] = if (x == NoContent) Lack else Got(x == Yes)
	@inline override def toOpt    :Opt[Boolean] = if (x == NoContent) Lack else Got(x == Yes)
	@inline override def constOpt :Opt[Boolean] = if (x == NoContent) Lack else Got(x == Yes)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.Ternary.toUnsure toUnsure]]. */
	@inline override def unsure :Unsure[Boolean] =
		if (x == NoContent) Missing else new Sure(x == Yes)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. */
	@inline override def toUnsure: Unsure[Boolean] =
		if (x == NoContent) Missing else new Sure(x == Yes)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any.
      * Same as [[net.noresttherein.sugar.vars.Ternary.toUnsure toUnsure]]. */
	@inline override def constUnsure :Unsure[Boolean] =
		if (x == NoContent) Missing else new Sure(x == Yes)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.Ternary.toPotential toPotential]]. */
	@inline override def potential :Potential[Boolean] = if (x == NoContent) Inexistent else Existent(x == Yes)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any. */
	@inline override def toPotential :Potential[Boolean] =
		if (x == NoContent) Inexistent else Existent(x == Yes)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * Same as [[net.noresttherein.sugar.vars.Ternary.toPotential toPotential]]. */
	@inline override def constPotential :Potential[Boolean] =
		if (x == NoContent) Inexistent else Existent(x == Yes)


	/** Converts this `Ternary` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[Boolean, O] =
		if (x == NoContent) Right(right) else Left(x == Yes)

	/** Converts this `Ternary` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, Boolean] =
		if (x == NoContent) Left(left) else Right(x == Yes)


	/** Converts this `Ternary` to `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline def toRed[O](blue: => O) :Pill[Boolean, O] =
		if (x == NoContent) Blue(blue) else Red(x == Yes)

	/** Converts this `Ternary` to `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline def toBlue[O](red: => O) :Pill[O, Boolean] =
		if (x == NoContent) Red(red) else Blue(x == Yes)

	/** Converts this `Ternary` to `Outcome`, returning the content as `Done`,
	  * or the value of the given `String` as `Failed` error message if empty. */
	@inline def outcome(err : => String) :Outcome[Boolean] =
		if (x == NoContent) Failed(() => err) else Done(x == Yes)

	/** Formats this `Ternary` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline override def mkString(prefix :String) :String = x match {
		case No  => prefix + "(false)"
		case Yes => prefix + "(true)"
		case _   => prefix + "()"
	}

	/** Formats this `Ternary` as `s"Ternary($get)"` or `"Ternary()"`. */
	@inline override def mkString :String = x match {
		case No  => "Ternary(false)"
		case Yes => "Ternary(true)"
		case _   => "Ternary()"
	}

	@inline override def toString :String = x match {
		case No  => "False"
		case Yes => "True"
		case _   => "Unknown"
	}

	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[Ternary]

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :Ternary) :Boolean = //x != NoContent & other.x != NoContent && x == other.x
		(x | other.x) != NoContent && x == other.x

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :Ternary) :Ternary =
		new Ternary(~(x ^ other.x) & 1 | x >> 1 | other.x >> 1)

}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.sugar.vars.Ternary Ternary]]s.
  * @see [[net.noresttherein.sugar.vars.Ternary.Unknown]]
  * @see [[net.noresttherein.sugar.vars.Ternary.Known$]]
  */
@SerialVersionUID(Ver)
object Ternary {
	/** Converts the given `Option[Boolean]` into a specialized `Ternary`, which is erased at runtime. */
	@inline def some_?(value :Option[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Opt[Boolean]` into a specialized `Ternary`, erased at runtime. */
	@inline def got_?(value :Opt[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Unsure[Boolean]` into a specialized `Ternary`, erased at runtime. */
	@inline def sure_?(value :Unsure[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Potential[Boolean]` into a specialized `Ternary` for interoperability. */
	@inline def existent_?(value :Potential[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Option[Boolean]` into a specialized `Ternary`, erased at runtime. */
	@inline def fromOption(value: Option[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Opt[Boolean]` into a specialized `Ternary`, erased at runtime. */
	@inline def fromOpt(value: Opt[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Unsure[Boolean]` into a specialized `Ternary`, erased at runtime. */
	@inline def fromUnsure(value: Unsure[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** Converts the given `Potential[Boolean]` into a specialized `Ternary` for interoperability. */
	@inline def fromPotential(value :Potential[Boolean]) :Ternary =
		new Ternary(
			if (value.isEmpty) NoContent
			else if (value.get) Yes
			else No
		)

	/** When a given condition is true, evaluates the `a` argument and returns `Known(a).`
	  * When the condition is false, `a` is not evaluated and `Unknown` is returned. */
	@inline def when(cond: Boolean)(a: => Boolean): Ternary =
		new Ternary(
			if (!cond) NoContent
			else if (a) Yes
			else No
		)

	/** Unless a given condition is true, this will evaluate the `a` argument and return `Known(a)`.
	  * Otherwise, `a` is not evaluated and `Unknown` is returned. */
	@inline def unless[A](cond: Boolean)(a: => Boolean): Ternary =
		new Ternary(
			if (cond) NoContent
			else if (a) Yes
			else No
		)

	/** Executes the given lazy expression in a `try-catch` block, returning `Unknown` in case
	  * any exception is caught. Otherwise the value is returned as a `Known` instance as normal. */
	@inline def guard(a : => Boolean) :Ternary =
		try { Known(a) } catch {
			case _ :Exception => Unknown
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `Unknown` in case
	  * any exception is caught. Otherwise the result is returned as a `Known` instance as normal. */
	@inline def guard[A](f :A => Boolean)(a :A) :Ternary =
		try { Known(f(a)) } catch {
			case _ :Exception => Unknown
		}

	/** Returns the first argument as `Known` if it satisfies the predicate `p`.
	  * @return `Known(value).filter(p)`.
	  */
	@inline def satisfying(value :Boolean)(p :Boolean => Boolean) :Ternary =
		new Ternary(
			if (!p(value)) NoContent
			else if (value) Yes
			else No
		)


	/** Equals `Known(true)`. */
	final val True  :Ternary = new Ternary(1)

	/** Equals `Known(false)`. */
	final val False :Ternary = new Ternary(0)

	/** Returns [[net.noresttherein.sugar.vars.Ternary.Unknown Unknown]] - an empty `Ternary`. */
	@inline final def empty :Ternary = Unknown

	/** A refinement of [[net.noresttherein.sugar.vars.Ternary Ternary]] marking it through a member flag type
	  * as empty. [[net.noresttherein.sugar.vars.Ternary.Unknown Ternary.Unknown]] is an instance of this type. */
	type Unknown = Ternary { type isEmpty = true }

	/** A special, empty instance of [[net.noresttherein.sugar.vars.Ternary Ternary]] which conforms to any `Ternary[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  * Named 'Unknown' because together with constants [[net.noresttherein.sugar.vars.Ternary.True True]] and
	  * [[net.noresttherein.sugar.vars.Ternary False]] for defined `Ternary`, it forms ternary logic.
	  * @note This value is of the refined type `Ternary { isEmpty = true }`. However, in many circumstances,
	  *       it is preferable to use simply `Ternary` for proper type inference.
	  *       In those cases you can use `Ternary.empty`.
	  * @see [[net.noresttherein.sugar.vars.Ternary.empty]] */
	final val Unknown :Unknown = new Ternary(NoContent).asInstanceOf[Unknown]

	/** A refinement of [[net.noresttherein.sugar.vars.Ternary Ternary]] marking it through a member flag type
	  * as non-empty. [[net.noresttherein.sugar.vars.Ternary Ternary]] factory object creates instances
	  * narrowed down to this type. */
	type Known = Ternary { type isEmpty = false }

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.Ternary Ternary]]. */
	@SerialVersionUID(Ver)
	object Known {
		/** Creates a non-empty [[net.noresttherein.sugar.vars.Ternary Ternary]] wrapping the given value. */
		@inline def apply(x :Boolean) :Known = new Ternary(if (x) 1 else 0).asInstanceOf[Known]

		/** Matches non-empty [[net.noresttherein.sugar.vars.Ternary Ternary]] instances. */
		@inline def unapply(Ternary :Ternary) :Ternary = Ternary
	}


	/** The for-comprehension facade for `Ternary[A]`, which does not evaluate the filter predicate until
	  * `map`, `flatMap` or `foreach` is called. */
	final class WithFilter(self :Ternary, p :Boolean => Boolean) {
		def map(f :Boolean => Boolean) :Ternary =
			if (self.x == NoContent) self else new Ternary(if (f(self.x == Yes)) Yes else No)
		def map[B](f: Boolean => B): Opt[B] = self filter p map f
		def flatMap(f: Boolean => Ternary): Ternary = self filter p flatMap f
		def flatMap[B](f: Boolean => Opt[B]): Opt[B] = self filter p flatMap f
		def foreach[U](f: Boolean => U): Unit = self filter p foreach f
		def withFilter(q: Boolean => Boolean): WithFilter = new WithFilter(self, x => p(x) && q(x))
	}


	/** Implicitly lifts a `Boolean` to [[net.noresttherein.sugar.vars.Ternary Ternary]]`[T]`. */
	@inline implicit def BooleanToTernary(x :Boolean) :Ternary = new Ternary(if (x) Yes else No)

	/** Implicit conversions between `Ternary` and `Option` and `Opt`.
	  * Conversions between `Ternary` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.conversions conversions]]. */
	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def TernaryToOption(opt :Ternary) :Option[Boolean] = opt.option
		@inline implicit def TernaryToIterable(opt :Ternary) :Iterable[Boolean] = opt.toIterable

		@inline implicit def OptionToTernary(option :Option[Boolean]) :Ternary =
			new Ternary(
				if (option.isDefined)
					if (option.get) Yes else No
				else NoContent
			)

		@inline implicit def TernaryToOpt(opt :Ternary) :Opt[Boolean] = opt.opt
		@inline implicit def OptToTernary(opt :Opt[Boolean]) :Ternary =
			new Ternary(
				if (opt.isDefined)
					if (opt.get) Yes else No
				else NoContent
			)

		@inline implicit def TernaryToPotential(Ternary :Ternary) :Potential[Boolean] = Ternary.potential
		@inline implicit def PotentialToTernary(opt :Potential[Boolean]) :Ternary = Ternary.fromPotential(opt)

		/** Implicitly lifts a `Boolean` to [[net.noresttherein.sugar.vars.Ternary Ternary]]`[T]`. */
		@inline implicit def BooleanToKnown(x :Boolean) :Known = new Ternary(if (x) Yes else No).asInstanceOf[Known]
	}


	private[Ternary] final val Yes       = 1
	private[Ternary] final val No        = 0
	private[Ternary] final val NoContent = -1
}
