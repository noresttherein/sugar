package net.noresttherein.sugar.vars

import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.exceptions.reflect.raise
import net.noresttherein.sugar.vars.Fallible.{Failed, Passed}
import net.noresttherein.sugar.vars.Opt.{Got, Lack, NoContent, WithFilter, unzip2Lack, unzip3Lack}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Potential.{Existent, Inexistent}




/** An, erased in runtime, option-like monadic wrapper of arbitrary values, which may not be wrapping any legal value.
  * It behaves exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  * recursion/loops. As a value class, it has no distinct subclasses for empty and non-empty instances, which results
  * in certain differences from `Option`. Aside from the obvious lack of creation of an additional object,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, a disadvantage
  * of being erased in runtime is that a method accepting an `Opt[T]` will clash with an overloaded method accepting `T`.
  * Moreover, a generic method accepting/returning an abstract type (parameter) `T` cannot be overridden/implemented
  * by a method accepting an `Opt[O]`, where `O` is a type parameter, an abstract type, or a reference type
  * (this is a limitation of the current Scala compiler).
  *
  * Note that, as this is a value class wrapping any type, boxing of built in value types to their reference wrappers
  * will still occur. In particular, nesting `Opt`s within each other works exactly as with `Option`s
  * (that is, `Lack` is distinguishable from `Got(Lack)`), but the inner `Opt`s will always be reified
  * to instances of `Opt` class, rather than erased to their contents.
  *
  * Using an `Opt` via a super trait, such as [[net.noresttherein.sugar.vars.Ref Ref]]
  * or [[scala.collection.IterableOnce IterableOnce]], or passing it as a type parameter (in particular, using
  * it as a function parameter or returned value), will however result in boxing
  * which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  * composing several `Opt`s' can result in closures being created (as manual nesting of `flatMap` calls also can).
  * For this reason its best to either directly use `isEmpty` and `get` in a conditional expression, or use the
  * [[net.noresttherein.sugar.vars.Opt.Got$ Got]] matching pattern, which should by be translated by the compiler
  * to non-boxing byte code.
  *
  * This library provides a total of five `Option` alternatives:
  *   1. A fully erased type alias [[net.noresttherein.sugar.vars.Potential Potential]]`[A]`
  *      (aliased as [[net.noresttherein.sugar.vars.?? ??]]`[A]`), which takes the concept of this class
  *      one step further, erasing the type information in all contexts: any reference to `Potential[T]` will
  *      translate to `AnyRef` in the byte code, regardless if `T` is abstract or not. It has an advantage over `Opt`
  *      primarily in that returning it from a function or a accepting it as a type parameter does not require
  *      the creation of an `Opt` instance through reference reification. As a bonus, it doesn't cause
  *      'auto generated bridge method conflicts with the method itself' compile error when overriding
  *      a method taking a parameter `T` with a `Potential[S]` (by passing `Potential[S] as T` to the super class).
  *      The disadvantages are the loss of dynamic type information (a `??[T]` will match all match patterns
  *      which would match `T`), higher potential for erasure conflicts, and inability to use
  *      as a return type of an `unapply` method (because it provides only extension methods, rather than class methods
  *      as `Opt` does. This makes it best suited as very short living objects where the `Potential` type
  *      is never upcasted or used as a type argument.
  *   1. This `Opt` is a relatively safer alternative in that it can be used both in erased and non erased contexts,
  *      has fewer restrictions and lower bug potential then the former. This however also makes it no better than
  *      `Option` as a function argument/return type or an element type of an array.
  *   1. A `@specialized` [[net.noresttherein.sugar.vars.Unsure Unsure]] is the most niche of the three,
  *      as it is never erased and always results in a creation of a new object. Unlike `Option` (and the other two
  *      alternatives), it will not result in boxing of built in value types and opaque types mapped to value types.
  *      In these contexts it offers largely the same benefits as `Potential` and `Opt`, because in all three cases
  *      a single boxing will need to happen. It has however an advantage over other two in that this benefit is 'free':
  *      it will cause no erasure related issues (less than even `Option` itself due to specialization).
  *   1. A specialized [[net.noresttherein.sugar.vars.IntOpt IntOpt]], erased in the runtime to a `Long`.
  *   1. A specialized [[net.noresttherein.sugar.vars.BoolOpt BoolOpt]], erased in the runtime to a `Char`.
  *
  * In most scenarios, use of `Potential` is preferable, however it cannot be used as a result of an `unapply` method,
  * which makes this class non redundant.
  * @see [[net.noresttherein.sugar.vars.Opt.Got$]]
  * @see [[net.noresttherein.sugar.vars.Opt.Lack]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  * @define Ref `Opt`
  * @define coll optional value
  */
@SerialVersionUID(Ver)
class Opt[+A] private[Opt] (private val ref :AnyRef) //private[Opt] to allow inlining of its construction
	extends AnyVal with Ref[A] with IterableOnce[A] with Product with Equals with Serializable
{
	/** A flag member type specifying if the `Opt` is full or empty on type level through refinement.
	  * For example, `opt :Opt[T] { type isEmpty = false }` guarantees that `opt.get` will return a value.
	  * Naturally, this being erased type, this guarantee is weaker than `opt :Some[T]`, because any instance
	  * can be successfully cast to a type with any value for this type.
	  * @see [[net.noresttherein.sugar.vars.Opt.Got!]] */
	type isEmpty <: Boolean with Singleton

	@inline override def isFinal = true

	/** Tests if this `Opt` does not contain a value (is equal to [[net.noresttherein.sugar.vars.Opt.Lack Lack]]). */
	@inline override def isEmpty :Boolean = ref eq NoContent

	/** Tests if this `Opt` contains a value. If true, `get` will not throw an exception. */
	@inline override def nonEmpty: Boolean = ref ne NoContent

	/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isConst :Boolean = ref ne NoContent

	/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined :Boolean = ref ne NoContent

	/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefinite :Boolean = ref ne NoContent

	/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isFinalizable :Boolean = ref ne NoContent

	@inline override def knownSize :Int = if (ref eq NoContent) 0 else 1

	/** Returns `1` if the `Opt` carries a value and `0` otherwise. */
	@inline def size :Int = if (ref eq NoContent) 0 else 1

	@inline override def productArity :Int = if (ref eq NoContent) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && (ref ne NoContent)) ref
		else throw new IndexOutOfBoundsException(toString + ".productElement(" + n + ")")


	/** The wrapped value. It is the same as [[net.noresttherein.sugar.vars.Opt.get get]], but this method
	  * is only possible to call on instances for which it is statically known that the value exists based on
	  * [[net.noresttherein.sugar.vars.Opt.isEmpty! isEmpty]] member type.
	  * @return the contained value. */
	@inline def sure(implicit nonEmpty :this.type <:< Got[_]) :A =
		if (ref eq NoContent) throw new NoSuchElementException("Lack.sure")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Opt` is empty. */
	@inline override def get :A =
		if (ref eq NoContent) throw new NoSuchElementException("Lack.get")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws NoSuchElementException if this `Opt` is empty. */
	@inline override def value :A =
		if (ref eq NoContent) throw new NoSuchElementException("Lack.value")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `Opt` is empty. */
	@inline override def const :A =
		if (ref eq NoContent) throw new UnsupportedOperationException("Lack.const")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `Opt` is empty. */
	@inline override def apply() :A =
		if (ref eq NoContent) throw new UnsupportedOperationException("Lack.const")
		else ref.asInstanceOf[A]

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: A](or: => O) :O =
		if (ref eq NoContent) or else ref.asInstanceOf[A]

	/** Similarly to [[net.noresttherein.sugar.vars.Opt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault[O >: A](or: O) :O =
		if (ref eq NoContent) or else ref.asInstanceOf[A]

	/** Assuming that `A` is a nullable type, return `null` if this `Opt` is empty, or the wrapped value otherwise. */
	@inline def orNull[O >: A](implicit isNullable :Null <:< O) :O =
		if (ref eq NoContent) null.asInstanceOf[O] else ref.asInstanceOf[O]
//
//	/** Gets the element in the `Opt` or throws the exception given as the argument.
//	  * @see [[net.noresttherein.sugar.vars.Opt.orNoSuch orNoSuch]]
//	  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
//	@inline def orThrow(e: => Throwable) :A =
//		if (ref eq NoContent) throw e else ref.asInstanceOf[A]

	/** Gets the element in the `Opt` or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Opt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :A =
		if (ref eq NoContent) raise[E](msg) else ref.asInstanceOf[A]

	/** Gets the element in the `Opt` or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Opt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag] :A =
		if (ref eq NoContent) raise[E] else ref.asInstanceOf[A]

	/** Gets the element in this `Opt` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Opt.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :A =
		if (ref eq NoContent) throw new NoSuchElementException(msg) else ref.asInstanceOf[A]

	/** Gets the element in this `Opt` or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Opt.orThrow orThrow]] */
	@inline def orNoSuch :A =
		if (ref eq NoContent) throw new NoSuchElementException else ref.asInstanceOf[A]

	/** Gets the element in this `Opt` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Opt.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :A =
		if (ref eq NoContent) throw new IllegalArgumentException(msg) else ref.asInstanceOf[A]

	/** Gets the element in this `Opt` or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.Opt.orThrow orThrow]] */
	@inline def orIllegal :A =
		if (ref eq NoContent) throw new IllegalArgumentException else ref.asInstanceOf[A]

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError(msg: => String) :A = {
		assert(ref ne NoContent, msg)
		ref.asInstanceOf[A]
	}

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError :A = {
		assert(ref ne NoContent)
		ref.asInstanceOf[A]
	}



	/** Returns the value this `Opt` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse[O >: A](or: => Opt[O]) :Opt[O] =
		if (ref eq NoContent) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.Opt.orElse orElse]], returns this `Opt` if it is not empty
	  * and `or` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def ifEmpty[O >: A](or: Opt[O]) :Opt[O] =
		if (ref eq NoContent) or else this

	/** Returns this `Opt` if the condition is false and `Lack` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyIf(condition :Boolean) :Opt[A] =
		if (condition) Lack else this

	/** Returns this `Opt` if the condition is true and `Lack` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyUnless(condition :Boolean) :Opt[A] =
		if (condition) this else Lack



	/** Returns a new `Opt` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O](f :A => O) :Opt[O] =
		if (ref eq NoContent) new Opt(NoContent)
		else new Opt(f(ref.asInstanceOf[A]).asInstanceOf[AnyRef])

	/** Applies the given function to the content of this `Opt` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline def mapOrElse[O](f :A => O, or: => O) :O =
		if (ref eq NoContent) or else f(ref.asInstanceOf[A])

	/** Returns the result of applying `f` to the value of this `Opt` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  *
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Opt.mapOrElse mapOrElse]] instead.
	  * @param ifEmpty the expression to evaluate if empty.
	  * @param f       the function to apply if nonempty. */
	@inline def fold[O](ifEmpty: => O)(f: A => O) :O =
		if (ref eq NoContent) ifEmpty else f(ref.asInstanceOf[A])

	/** The same as [[net.noresttherein.sugar.vars.Opt.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.Opt.Lack Lack]] is returned instead. */
	@inline def guardMap[O](f :A => O) :Opt[O] =
		if (ref eq NoContent)
			new Opt(NoContent)
		else try {
			new Opt(f(ref.asInstanceOf[A]).asInstanceOf[AnyRef])
		} catch {
			case _ :Exception => new Opt(NoContent)
		}

	/** Returns the result of applying the given function to the value of this `Opt` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap[O](f :A => Opt[O]) :Opt[O] =
		if (ref eq NoContent) new Opt(NoContent)
		else f(ref.asInstanceOf[A])

	/** Flattens `Opt[Opt[O]]` to a single `Opt[O]`. */
	def flatten[O](implicit isOpt :A <:< Opt[O]) :Opt[O] =
		(ref :Any) match {
			case NoContent => new Opt(NoContent)
			case opt :Opt[O @unchecked] => opt
			case _ => new Opt(ref)
		}

	/** Returns an empty `Opt` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed[O >: A](o :O) :Opt[A] =
		if (ref == o) new Opt(NoContent) else this

	/** Returns an empty `Opt` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: A](that :IterableOnce[O]) :Opt[A] = that match {
		case _ if ref eq NoContent => this
		case it :Set[O] => if (it(get)) new Opt(NoContent) else this
		case it :Ranking[O] => if (it.contains(get)) new Opt(NoContent) else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val x = get
			while (i.hasNext)
				if (i.next() == x)
					return new Opt(NoContent)
			this
	}

	/** Returns a new `Opt` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Opt.Lack Lack]] otherwise. */
	@inline def filter(p :A => Boolean) :Opt[A] =
		if ((ref eq NoContent) || p(ref.asInstanceOf[A])) this else new Opt(NoContent)

	/** Returns a new `Opt` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Opt.Lack Lack]] otherwise. */
	@inline def filterNot(p :A => Boolean) :Opt[A] =
		if ((ref eq NoContent) || !p(ref.asInstanceOf[A])) this else new Opt(NoContent)

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.Opt.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :A => Boolean) :WithFilter[A] = new WithFilter[A](this, p)


	/** Tests if this `Opt` is not empty and its value is equal to the given argument. */
	@inline def contains[O >: A](o :O): Boolean = ref == o

	/** Tests if this `Opt` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :A => Boolean): Boolean = (ref ne NoContent) && p(ref.asInstanceOf[A])

	/** Tests if this `Opt` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :A => Boolean): Boolean = (ref eq NoContent) || p(ref.asInstanceOf[A])

	/** Executes the given block for this `Opt`s value if it is not empty. */
	@inline def foreach[O](f :A => O) :Unit = if (ref ne NoContent) f(ref.asInstanceOf[A])

	/** Returns an empty `Opt` if this `Opt` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Opt`. */
	@inline def collect[O](f :PartialFunction[A, O]) :Opt[O] =
		if (ref eq NoContent)
			new Opt(NoContent)
		else
			new Opt(f.asInstanceOf[PartialFunction[A, AnyRef]].applyOrElse(ref.asInstanceOf[A], NoContent))


	/** Returns an `Opt` formed from the contents of `this` and `that` by combining the corresponding elements in a pair.
	  * If either of the two options is empty, `Lack` is returned. */
	@inline def zip[O](that :Opt[O]) :Opt[(A, O)] =
		if ((ref eq NoContent) | (that.ref eq NoContent)) new Opt(NoContent)
		else Got((ref.asInstanceOf[A], that.ref.asInstanceOf[O]))

	/** Converts an `Opt` of a pair into `Opt`s of its first and second elements. */
	@inline def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (Opt[A1], Opt[A2]) =
		if (ref eq NoContent)
			unzip2Lack
		else (
			new Opt[A1](ref.asInstanceOf[(A1, A2)]._1.asInstanceOf[AnyRef]),
			new Opt[A2](ref.asInstanceOf[(A1, A2)]._2.asInstanceOf[AnyRef])
		)

	/** Converts an `Opt` of a triple into three `Opt`s, one containing the element from each position of the triple. */
	@inline def unzip3[A1, A2, A3](implicit asTriple: A <:< (A1, A2, A3)): (Opt[A1], Opt[A2], Opt[A3]) =
		if (ref eq NoContent)
			unzip3Lack
		else (
			new Opt[A1](ref.asInstanceOf[(A1, A2, A3)]._1.asInstanceOf[AnyRef]),
			new Opt[A2](ref.asInstanceOf[(A1, A2, A3)]._2.asInstanceOf[AnyRef]),
			new Opt[A3](ref.asInstanceOf[(A1, A2, A3)]._3.asInstanceOf[AnyRef])
		)


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline override def iterator :Iterator[A] =
		if (ref eq NoContent) Iterator.empty else Iterator.single(ref.asInstanceOf[A])

	/** Returns `Nil` if this `Opt` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[A] = if (ref eq NoContent) Nil else ref.asInstanceOf[A]::Nil

	/** Returns an empty list if this `Opt` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[A] = if (ref eq NoContent) Nil else ref.asInstanceOf[A]::Nil

	/** Returns an empty collection if this `Opt` is empty or a singleton with its value otherwise. */
	@inline def toIterable :Iterable[A] = if (ref eq NoContent) Iterable.empty else Iterable.single(ref.asInstanceOf[A])

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.Opt.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def option :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def toOption :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def constOption :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	@inline override def opt      :Opt[A] = this
	@inline override def toOpt    :Opt[A] = this
	@inline override def constOpt :Opt[A] = this

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in an `Opt`.
	  * Same as [[net.noresttherein.sugar.vars.Opt.toUnsure toUnsure]]. */
	@inline override def unsure :Unsure[A] =
		if (ref eq NoContent) Missing else new Sure(ref.asInstanceOf[A], cachedOpt = this)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in an `Opt`. */
	@inline override def toUnsure :Unsure[A] =
		if (ref eq NoContent) Missing else new Sure(ref.asInstanceOf[A], cachedOpt = this)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in an `Opt`.
      * Same as [[net.noresttherein.sugar.vars.Opt.toUnsure toUnsure]]. */
	@inline override def constUnsure :Unsure[A] =
		if (ref eq NoContent) Missing else new Sure(ref.asInstanceOf[A], cachedOpt = this)

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. Same as [[net.noresttherein.sugar.vars.Opt.toPotential toPotential]]. */
	@inline override def potential :Potential[A] = if (ref eq NoContent) Inexistent else Existent(ref.asInstanceOf[A])

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. */
	@inline override def toPotential :Potential[A] =
		if (ref eq NoContent) Inexistent else Existent(ref.asInstanceOf[A])

	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. Same as [[net.noresttherein.sugar.vars.Opt.toPotential toPotential]]. */
	@inline override def constPotential :Potential[A] =
		if (ref eq NoContent) Inexistent else Existent(ref.asInstanceOf[A])
//
//	/** Conversion to a fully erased `Potential` carrying the same value as this instance, if any.
//	  * This conversion does not require boxing. */
//	@inline override def ?? :Potential[A] =
//		if (ref eq NoContent) Inexistent else Existent(ref.asInstanceOf[A])


	/** Converts this `Opt` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[A, O] =
		if (ref eq NoContent) Right(right) else Left(ref.asInstanceOf[A])

	/** Converts this `Opt` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, A] =
		if (ref eq NoContent) Left(left) else Right(ref.asInstanceOf[A])


	/** Converts this `Opt` to `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline def toRed[O](blue: => O) :Pill[A, O] =
		if (ref eq NoContent) Blue(blue) else Red(ref.asInstanceOf[A])

	/** Converts this `Opt` to `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline def toBlue[O](red: => O) :Pill[O, A] =
		if (ref eq NoContent) Red(red) else Blue(ref.asInstanceOf[A])

	/** Converts this `Opt` to `Fallible`, returning the content as `Passed`,
	  * or the value of the given `String` as `Failed` error message if empty. */
	@inline final def toPassed(err : => String) :Fallible[A] =
		if (ref eq NoContent) Failed(() => err) else Passed(get)

	/** Formats this `Opt` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline override def mkString(prefix :String) :String =
		if (ref eq NoContent) prefix + "()" else prefix + "(" + ref + ")"

	/** Formats this `Opt` as `s"Opt($get)"` or `"Opt()"`. */
	@inline override def mkString :String = if (ref eq NoContent) "Opt()" else "Opt(" + ref + ")"

	@inline override def toString :String = if (ref eq NoContent) "Lack" else "Got(" + ref + ")"

	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[Opt[_]]

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :Opt[_]) :Boolean = (ref ne NoContent) & (other.ref ne NoContent) && ref == other.ref

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :Opt[_]) :Opt[Boolean] =
		if (ref eq NoContent) this.asInstanceOf[Opt[Boolean]]
		else if (other.ref eq NoContent) other.asInstanceOf[Opt[Boolean]]
		else Got(ref == other.ref)
}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.sugar.vars.Opt Opt]]s.
  * @see [[net.noresttherein.sugar.vars.Opt.Lack]]
  * @see [[net.noresttherein.sugar.vars.Opt.Got$]]
  */
@SerialVersionUID(Ver)
object Opt {
	/** Wraps the given object in a purely syntactic option-like object erased in the runtime. */
	@inline final def apply[T](value :T) :Opt[T] =
		if (value == null) Lack else new Opt(value.asInstanceOf[AnyRef])


	/** Converts the given `Option[T]` into a lighter `Opt[T]`, which is erased at runtime. */
	@inline def some_?[T](value :Option[T]) :Opt[T] =
		new Opt(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Unsure[T]` into an `Opt[T]`, erased at runtime. */
	@inline def sure_?[T](value :Unsure[T]) :Opt[T] =
		new Opt(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Potential[T]` into an `Opt[T]` for interoperability. */
	@inline def existent_?[T](value :Potential[T]) :Opt[T] = Existent.unapply(value)

	/** Converts the given `Option[T]` into a lighter `Opt[T]` which is erased at runtime. */
	@inline def fromOption[T](value: Option[T]) :Opt[T] =
		new Opt(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Unsure[T]` into an `Opt[T]`, erased at runtime. */
	@inline def fromUnsure[T](value: Unsure[T]) :Opt[T] =
		new Opt(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Potential[T]` into an `Opt[T]` for interoperability. */
	@inline def fromPotential[T](value :Potential[T]) :Opt[T] = Existent.unapply(value)

	/** When a given condition is true, evaluates the `a` argument and returns `Got(a).`
	  * When the condition is false, `a` is not evaluated and `Lack` is returned. */
	@inline def when[A](cond: Boolean)(a: => A) :Opt[A] =
		new Opt(if (cond) a.asInstanceOf[AnyRef] else NoContent)

	/** Unless a given condition is true, this will evaluate the `a` argument and return `Got(a)`.
	  * Otherwise, `a` is not evaluated and `Lack` is returned. */
	@inline def unless[A](cond: Boolean)(a: => A) :Opt[A] =
		new Opt(if (!cond) a.asInstanceOf[AnyRef] else NoContent)

	/** Executes the given lazy expression in a `try-catch` block, returning `Lack` in case
	  * any exception is caught. Otherwise the value is returned as a `Got` instance as normal. */
	@inline def guard[A](a : => A) :Opt[A] =
		try { Got(a) } catch {
			case _ :Exception => Lack
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `Lack` in case
	  * any exception is caught. Otherwise the result is returned as a `Got` instance as normal. */
	@inline def guard[A, B](f :A => B)(a :A) :Opt[B] =
		try { Got(f(a)) } catch {
			case _ :Exception => Lack
		}

	/** Returns the first argument as `Got` if it satisfies the predicate `p`.
	  * @return `Got(value).filter(p)`.
	  */
	@inline def satisfying[A](value :A)(p :A => Boolean) :Opt[A] =
		if (p(value)) Got(value) else Lack


	/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]] - an empty `Opt`. */
	@inline final def empty[T] :Opt[T] = new Opt(NoContent)

	/** A refinement of [[net.noresttherein.sugar.vars.Opt Opt]] marking it through a member flag type
	  * as empty. [[net.noresttherein.sugar.vars.Opt.Lack Opt.Lack]] is an instance of this type. */
	type Lack = Opt[Nothing] { type isEmpty = true }

	/** A special, empty instance of [[net.noresttherein.sugar.vars.Opt Opt]] which conforms to any `Opt[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  * @note This value is of a refined type `Opt[Nothing] { type isEmpty = true }`. However, in many circumstances,
	  *       it is preferable to have a basic `Opt[T]` for some specific type `T`.
	  *       In those cases you can use `Opt.empty[T]`.
	  * @see [[net.noresttherein.sugar.vars.Opt.empty]] */
	@inline final val Lack :Lack = new Opt(NoContent).asInstanceOf[Lack]

	/** A refinement of [[net.noresttherein.sugar.vars.Opt Opt]] marking it through a member flag type
	  * as non-empty. [[net.noresttherein.sugar.vars.Opt$ Opt]] factory object creates instances
	  * narrowed down to this type. */
	type Got[+T] = Opt[T] { type isEmpty = false }

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.Opt Opt]]. */
	@SerialVersionUID(Ver)
	object Got {
		/** Creates a non-empty [[net.noresttherein.sugar.vars.Opt Opt]] wrapping the given value. */
		@inline def apply[T](x :T) :Got[T] = new Opt(x.asInstanceOf[AnyRef]).asInstanceOf[Got[T]]

		/** Matches non-empty [[net.noresttherein.sugar.vars.Opt Opt]] instances. */
		@inline def unapply[T](opt :Opt[T]) :Opt[T] = opt
	}


	/** The for-comprehension facade for `Opt[A]`, which does not evaluate the filter predicate until
	  * `map`, `flatMap` or `foreach` is called. */
	final class WithFilter[+A](self :Opt[A], p :A => Boolean) {
		def map[B](f: A => B): Opt[B] = self filter p map f
		def flatMap[B](f: A => Opt[B]): Opt[B] = self filter p flatMap f
		def foreach[U](f: A => U): Unit = self filter p foreach f
		def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
	}



//	@inline implicit def optToPotential[A](opt :Opt[A]) :Potential[A] = opt.toPotential
//
	/** Implicit conversions between `Opt` and `Option`.
	  * Conversions between `Opt` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.implicits implicits]]. */
	@SerialVersionUID(Ver)
	object implicits {
		@inline implicit def OptToOption[T](opt :Opt[T]) :Option[T] = opt.option
		@inline implicit def OptToIterable[T](opt :Opt[T]) :Iterable[T] = opt.toIterable

		@inline implicit def OptionToOpt[T](option :Option[T]) :Opt[T] =
			new Opt(if (option.isDefined) option.get.asInstanceOf[AnyRef] else NoContent)

		//consider: placing this also in optional.extensions (or optional.implicits)
		/** Implicitly lifts any value `T` to [[net.noresttherein.sugar.vars.Opt Opt]]`[T]`. */
		@inline implicit def gotAny[T](x :T) :Got[T] = new Opt(x.asInstanceOf[AnyRef]).asInstanceOf[Got[T]]

		@inline implicit def OptToPotential[T](opt :Opt[T]) :Potential[T] = opt.potential
		@inline implicit def PotentialToOpt[T](opt :Potential[T]) :Opt[T] = Opt.fromPotential(opt)
	}

	/** Importing the contents of this object replace all usage of [[Option]]/[[Some]]/[[None]] in the scope with
	  * [[net.noresttherein.sugar.vars.Opt Opt]]/[[net.noresttherein.sugar.vars.Opt.Got Got]]/[[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
	  * which allow seamless interoperability with standard Scala APIs.
	  *
	  * While the idea of using it in production code should be carefully reconsidered,
	  * it is useful for quickly checking during profiling what impact using `Option` vs `Opt` has.
	  *
	  * Other files which reference classes defined in the import's scope may also need to be modified in order
	  * to comply with changed interfaces. */
	@SerialVersionUID(Ver)
	object OptAsOption {
		type Option[T] = Opt[T]
		type Some[T]   = Got[T]

		val Option = Opt
		val Some   = Got
		val None   = Lack
		//same names as in implicits so if both are imported one shadows the other
		@inline implicit def OptToOption[T](opt :Opt[T]) :scala.Option[T] = opt.option
		@inline implicit def OptionToOpt[T](opt :scala.Option[T]) :Opt[T] = some_?(opt)
		@inline implicit def SomeToGot[T](opt :scala.Some[T]) :Got[T] = Got(opt.get)
		@inline implicit def GotToSome[T](opt :Sure[T]) :scala.Some[T] = opt.option.asInstanceOf[scala.Some[T]]

		@inline implicit def NoneToLack(none :scala.None.type) :Lack.type = Lack
		@inline implicit def LackToNone(miss :Lack.type) :scala.None.type = scala.None
	}


	//extends Any => AnyRef out of laziness, allowing it to pass as the argument to applyOrElse
	//is private[vars] so that methods of Opt can be inlined
	// and because some Ref classes play with the erasure and need access to this marker object
	@SerialVersionUID(Ver)
	private[vars] object NoContent extends (Any => AnyRef) with Serializable {
		def apply(ignore :Any) :AnyRef = this
		override def toString = "<undefined>"
	}

	private val unzip2Lack = (Lack, Lack)
	private val unzip3Lack = (Lack, Lack, Lack)
}

