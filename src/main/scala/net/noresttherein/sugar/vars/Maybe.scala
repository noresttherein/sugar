package net.noresttherein.sugar.vars

import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, outOfBounds_!, unsupported_!, raise}
import net.noresttherein.sugar.vars.Maybe.{No, NoContent, WithFilter, Yes, unzip2Lack, unzip3Lack}
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Opt.One




/** An, erased in runtime, option-like monadic wrapper of arbitrary values, which may not be wrapping any legal value.
  * It behaves exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  * recursion/loops. As a value class, it has no distinct subclasses for empty and non-empty instances, which results
  * in certain differences from `Option`. Aside from the obvious lack of creation of an additional object,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, a disadvantage
  * of being erased in runtime is that a method accepting an `Maybe[T]` will clash with an overloaded method
  * accepting `Any` (including any erased, generic type parameter `T`).
  * Moreover, a generic method accepting/returning an abstract type (parameter) `T` cannot be overridden/implemented
  * by a method accepting an `Maybe[O]`, where `O` is a type parameter, an abstract type, or a reference type
  * (this is a limitation of the current Scala compiler).
  *
  * Note that, as this is a value class wrapping any type, boxing of built in value types to their reference wrappers
  * will still occur. In particular, nesting `Maybe`s within each other works exactly as with `Option`s
  * (that is, `No` is distinguishable from `Yes(No)`), but the inner `Maybe`s will always be reified
  * to instances of `Maybe` class, rather than erased to their contents.
  *
  * Using a `Maybe` via a super trait, such as [[net.noresttherein.sugar.vars.Ref Ref]]
  * or [[scala.collection.IterableOnce IterableOnce]], or passing it as a type parameter (in particular, using
  * it as a function parameter or returned value), will however result in boxing
  * which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  * composing several `Maybe`s' can result in closures being created (as manual nesting of `flatMap` calls also can).
  * For this reason its best to either directly use `isEmpty` and `get` in a conditional expression, or use the
  * [[net.noresttherein.sugar.vars.Maybe.Yes Yes]] matching pattern, which should by be translated by the compiler
  * to non-boxing byte code.
  *
  * $optionalTypesInfo
  *
  * In most scenarios, use of `Opt` is preferable, however it cannot be used as a result of an `unapply` method,
  * which makes this class non redundant.
  * @see [[net.noresttherein.sugar.vars.Maybe.Yes]]
  * @see [[net.noresttherein.sugar.vars.Maybe.No]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  * @see [[net.noresttherein.sugar.vars.Nullable]]
  * @define Ref `Maybe`
  * @define ref optional value
  * @define coll optional value
  */ //consider: making all methods return Maybe, not Opt - this way they can be easily pattern match
@SerialVersionUID(Ver)
class Maybe[+A] private[Maybe](private val ref :AnyRef) //private[Maybe] to allow inlining of its construction
	extends AnyVal with Ref[A] with IterableOnce[A] with Product with Equals with Serializable
{
	/** A flag member type specifying if the `Maybe` is full or empty on type level through refinement.
	  * For example, `opt :Maybe[T] { type isEmpty = false }` guarantees that `opt.get` will return a value.
	  * Naturally, this being erased type, this guarantee is weaker than `opt :Some[T]`, because any instance
	  * can be successfully cast to a type with any value for this type.
	  * @see [[net.noresttherein.sugar.vars.Maybe.Yes!]] */
	type isEmpty <: Boolean with Singleton

	@inline override def isFinal = true

	/** Tests if this `Maybe` does not contain a value (is equal to [[net.noresttherein.sugar.vars.Maybe.No No]]). */
	@inline override def isEmpty :Boolean = ref eq NoContent

	/** Tests if this `Maybe` contains a value. If true, `get` will not throw an exception. */
	@inline override def nonEmpty: Boolean = ref ne NoContent

	/** Tests if this `Maybe` contains a value. This is the same as `nonEmpty`. */
	@inline override def isConst :Boolean = ref ne NoContent

	/** Tests if this `Maybe` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined :Boolean = ref ne NoContent

	/** Tests if this `Maybe` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefinite :Boolean = ref ne NoContent

	/** Tests if this `Maybe` contains a value. This is the same as `nonEmpty`. */
	@inline override def isFinalizable :Boolean = ref ne NoContent

	@inline override def knownSize :Int = if (ref eq NoContent) 0 else 1

	/** Returns `1` if the `Maybe` carries a value and `0` otherwise. */
	@inline def size :Int = if (ref eq NoContent) 0 else 1

	@inline override def productArity :Int = if (ref eq NoContent) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && (ref ne NoContent)) ref
		else outOfBounds_!(toString + ".productElement(" + n + ")")


	/** The wrapped value. It is the same as [[net.noresttherein.sugar.vars.Maybe.get get]], but this method
	  * is only possible to call on instances for which it is statically known that the value exists based on
	  * [[net.noresttherein.sugar.vars.Maybe.isEmpty! isEmpty]] member type.
	  * @return the contained value. */
	@inline def sure(implicit nonEmpty :this.type <:< Yes[_]) :A =
		if (ref eq NoContent) noSuch_!("No.sure")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Maybe` is empty. */
	@inline override def get :A =
		if (ref eq NoContent) noSuch_!("No.get")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws NoSuchElementException if this `Maybe` is empty. */
	@inline override def value :A =
		if (ref eq NoContent) noSuch_!("No.value")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `Maybe` is empty. */
	@inline override def const :A =
		if (ref eq NoContent) unsupported_!("No.const")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `Maybe` is empty. */
	@inline override def apply() :A =
		if (ref eq NoContent) unsupported_!("No.const")
		else ref.asInstanceOf[A]

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: A](or: => O) :O =
		if (ref eq NoContent) or else ref.asInstanceOf[A]

	/** Similarly to [[net.noresttherein.sugar.vars.Maybe.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault[O >: A](or: O) :O =
		if (ref eq NoContent) or else ref.asInstanceOf[A]

	/** Assuming that `A` is a nullable type, return `null` if this `Maybe` is empty, or the wrapped value otherwise. */
	@inline def orNull[O >: A](implicit isNullable :Null <:< O) :O =
		if (ref eq NoContent) null.asInstanceOf[O] else ref.asInstanceOf[O]
//
//	/** Gets the element in the `Maybe` or throws the exception given as the argument.
//	  * @see [[net.noresttherein.sugar.vars.Maybe.orNoSuch orNoSuch]]
//	  * @see [[net.noresttherein.sugar.vars.Maybe.orIllegal orIllegal]] */
//	@inline def orThrow(e: => Throwable) :A =
//		if (ref eq NoContent) throw e else ref.asInstanceOf[A]

	/** Gets the element in the `Maybe` or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Maybe.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Maybe.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :A =
		if (ref eq NoContent) raise[E](msg) else ref.asInstanceOf[A]

	/** Gets the element in the `Maybe` or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Maybe.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Maybe.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag] :A =
		if (ref eq NoContent) raise[E] else ref.asInstanceOf[A]

	/** Gets the element in this `Maybe` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Maybe.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :A =
		if (ref eq NoContent) noSuch_!(msg) else ref.asInstanceOf[A]

	/** Gets the element in this `Maybe` or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Maybe.orThrow orThrow]] */
	@inline def orNoSuch :A =
		if (ref eq NoContent) noSuch_! else ref.asInstanceOf[A]

	/** Gets the element in this `Maybe` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Maybe.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :A =
		if (ref eq NoContent) illegal_!(msg) else ref.asInstanceOf[A]

	/** Gets the element in this `Maybe` or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.Maybe.orThrow orThrow]] */
	@inline def orIllegal :A =
		if (ref eq NoContent) illegal_! else ref.asInstanceOf[A]

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



	/** Returns the value this `Maybe` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse[O >: A](or: => Maybe[O]) :Maybe[O] =
		if (ref eq NoContent) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.Maybe.orElse orElse]], returns this `Maybe` if it is not empty
	  * and `or` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def ifEmpty[O >: A](or: Maybe[O]) :Maybe[O] =
		if (ref eq NoContent) or else this

	/** Returns this `Maybe` if the condition is false and `No` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyIf(condition :Boolean) :Maybe[A] =
		if (condition) No else this

	/** Returns this `Maybe` if the condition is true and `No` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyUnless(condition :Boolean) :Maybe[A] =
		if (condition) this else No



	/** Returns a new `Maybe` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O](f :A => O) :Maybe[O] =
		if (ref eq NoContent) new Maybe(NoContent)
		else new Maybe(f(ref.asInstanceOf[A]).asInstanceOf[AnyRef])

	/** Applies the given function to the content of this `Maybe` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline def mapOrElse[O](f :A => O, or: => O) :O =
		if (ref eq NoContent) or else f(ref.asInstanceOf[A])

	/** Returns the result of applying `f` to the value of this `Maybe` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  *
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Maybe.mapOrElse mapOrElse]] instead.
	  * @param ifEmpty the expression to evaluate if empty.
	  * @param f       the function to apply if nonempty. */
	@inline def fold[O](ifEmpty: => O)(f: A => O) :O =
		if (ref eq NoContent) ifEmpty else f(ref.asInstanceOf[A])

	//consider: returning Outcome instead (here and in other Option-like types)
	/** The same as [[net.noresttherein.sugar.vars.Maybe.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.Maybe.No No]] is returned instead. */
	@inline def guardMap[O](f :A => O) :Maybe[O] =
		if (ref eq NoContent)
			new Maybe(NoContent)
		else try {
			new Maybe(f(ref.asInstanceOf[A]).asInstanceOf[AnyRef])
		} catch {
			case _ :Exception => new Maybe(NoContent)
		}

	/** Returns the result of applying the given function to the value of this `Maybe` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap[O](f :A => Maybe[O]) :Maybe[O] =
		if (ref eq NoContent) new Maybe(NoContent)
		else f(ref.asInstanceOf[A])

	/** Flattens `Maybe[Maybe[O]]` to a single `Maybe[O]`. */
	def flatten[O](implicit isMaybe :A <:< Maybe[O]) :Maybe[O] =
		(ref :Any) match {
			case NoContent => new Maybe(NoContent)
			case opt :Maybe[O @unchecked] => opt
			case _ => new Maybe(ref)
		}

	/** Returns an empty `Maybe` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed[O >: A](o :O) :Maybe[A] =
		if (ref == o) new Maybe(NoContent) else this

	/** Returns an empty `Maybe` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: A](that :IterableOnce[O]) :Maybe[A] = that match {
		case _ if ref eq NoContent => this
		case it :Set[O] => if (it(get)) new Maybe(NoContent) else this
		case it :Ranking[O] => if (it.contains(get)) new Maybe(NoContent) else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val x = get
			while (i.hasNext)
				if (i.next() == x)
					return new Maybe(NoContent)
			this
	}

	/** Returns a new `Maybe` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Maybe.No No]] otherwise. */
	@inline def filter(p :A => Boolean) :Maybe[A] =
		if ((ref eq NoContent) || p(ref.asInstanceOf[A])) this else new Maybe(NoContent)

	/** Returns a new `Maybe` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Maybe.No No]] otherwise. */
	@inline def filterNot(p :A => Boolean) :Maybe[A] =
		if ((ref eq NoContent) || !p(ref.asInstanceOf[A])) this else new Maybe(NoContent)

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.Maybe.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :A => Boolean) :WithFilter[A] = new WithFilter[A](this, p)


	/** Tests if this `Maybe` is not empty and its value is equal to the given argument. */
	@inline def contains[O >: A](o :O): Boolean = ref == o

	/** Tests if this `Maybe` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :A => Boolean): Boolean = (ref ne NoContent) && p(ref.asInstanceOf[A])

	/** Tests if this `Maybe` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :A => Boolean): Boolean = (ref eq NoContent) || p(ref.asInstanceOf[A])

	/** Executes the given block for this `Maybe`s value if it is not empty. */
	@inline def foreach[O](f :A => O) :Unit = if (ref ne NoContent) f(ref.asInstanceOf[A])

	/** Returns an empty `Maybe` if this `Maybe` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Maybe`. */
	@inline def collect[O](f :PartialFunction[A, O]) :Maybe[O] =
		if (ref eq NoContent)
			new Maybe(NoContent)
		else
			new Maybe(f.asInstanceOf[PartialFunction[A, AnyRef]].applyOrElse(ref.asInstanceOf[A], NoContent))


	/** Returns a `Maybe` formed from the contents of `this` and `that` by combining the corresponding elements in a pair.
	  * If either of the two options is empty, `No` is returned. */
	@inline def zip[O](that :Maybe[O]) :Maybe[(A, O)] =
		if ((ref eq NoContent) | (that.ref eq NoContent)) new Maybe(NoContent)
		else Yes((ref.asInstanceOf[A], that.ref.asInstanceOf[O]))

	/** Converts a `Maybe` of a pair into `Maybe`s of its first and second elements. */
	@inline def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (Maybe[A1], Maybe[A2]) =
		if (ref eq NoContent)
			unzip2Lack
		else (
			new Maybe[A1](ref.asInstanceOf[(A1, A2)]._1.asInstanceOf[AnyRef]),
			new Maybe[A2](ref.asInstanceOf[(A1, A2)]._2.asInstanceOf[AnyRef])
		)

	/** Converts a `Maybe` of a triple into three `Maybe`s, one containing the element from each position of the triple. */
	@inline def unzip3[A1, A2, A3](implicit asTriple: A <:< (A1, A2, A3)): (Maybe[A1], Maybe[A2], Maybe[A3]) =
		if (ref eq NoContent)
			unzip3Lack
		else (
			new Maybe[A1](ref.asInstanceOf[(A1, A2, A3)]._1.asInstanceOf[AnyRef]),
			new Maybe[A2](ref.asInstanceOf[(A1, A2, A3)]._2.asInstanceOf[AnyRef]),
			new Maybe[A3](ref.asInstanceOf[(A1, A2, A3)]._3.asInstanceOf[AnyRef])
		)


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline override def iterator :Iterator[A] =
		if (ref eq NoContent) Iterator.empty else Iterator.single(ref.asInstanceOf[A])

	/** Returns `Nil` if this `Maybe` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[A] = if (ref eq NoContent) Nil else ref.asInstanceOf[A]::Nil

	/** Returns an empty list if this `Maybe` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[A] = if (ref eq NoContent) Nil else ref.asInstanceOf[A]::Nil

	/** Returns an empty collection if this `Maybe` is empty or a singleton with its value otherwise. */
	@inline def toIterable :Iterable[A] = if (ref eq NoContent) Iterable.empty else Iterable.single(ref.asInstanceOf[A])

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.Maybe.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def option :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def toOption :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def constOption :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	@inline override def maybe      :Maybe[A] = this
	@inline override def toMaybe    :Maybe[A] = this
	@inline override def maybeConst :Maybe[A] = this

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in a `Maybe`.
	  * Same as [[net.noresttherein.sugar.vars.Maybe.toUnsure toUnsure]]. */
	@inline override def unsure :Unsure[A] =
		if (ref eq NoContent) Missing else new Sure(ref.asInstanceOf[A], cachedOpt = toOpt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in a `Maybe`. */
	@inline override def toUnsure :Unsure[A] =
		if (ref eq NoContent) Missing else new Sure(ref.asInstanceOf[A], cachedOpt = toOpt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in a `Maybe`.
      * Same as [[net.noresttherein.sugar.vars.Maybe.toUnsure toUnsure]]. */
	@inline override def unsureConst :Unsure[A] =
		if (ref eq NoContent) Missing else new Sure(ref.asInstanceOf[A], cachedOpt = toOpt)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. Same as [[net.noresttherein.sugar.vars.Maybe.toOpt toOpt]]. */
	@inline override def opt :Opt[A] = if (ref eq NoContent) None else One(ref.asInstanceOf[A])

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. */
	@inline override def toOpt :Opt[A] =
		if (ref eq NoContent) None else One(ref.asInstanceOf[A])

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. Same as [[net.noresttherein.sugar.vars.Maybe.toOpt toOpt]]. */
	@inline override def constOpt :Opt[A] =
		if (ref eq NoContent) None else One(ref.asInstanceOf[A])
//
//	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
//	  * This conversion does not require boxing. */
//	@inline override def ?? :Opt[A] =
//		if (ref eq NoContent) None else One(ref.asInstanceOf[A])


	/** Converts this `Maybe` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[A, O] =
		if (ref eq NoContent) Right(right) else Left(ref.asInstanceOf[A])

	/** Converts this `Maybe` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, A] =
		if (ref eq NoContent) Left(left) else Right(ref.asInstanceOf[A])


	/** Converts this `Maybe` to `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline def toRed[O](blue: => O) :Pill[A, O] =
		if (ref eq NoContent) Blue(blue) else Red(ref.asInstanceOf[A])

	/** Converts this `Maybe` to `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline def toBlue[O](red: => O) :Pill[O, A] =
		if (ref eq NoContent) Red(red) else Blue(ref.asInstanceOf[A])

	/** Converts this `Maybe` to `Outcome`, returning the content as `Done`,
	  * or the value of the given `String` as `Failed` error message if empty. */
	@inline def doneOr(err : => String) :Outcome[A] =
		if (ref eq NoContent) Failed(() => err) else Done(get)

	/** Converts this `Maybe` to `Outcome`, returning the content as `Done`,
	  * or the value of the given `Throwable` as an error if empty. */
	@inline final def doneOr(err :Throwable) :Outcome[A] =
		if (ref eq NoContent) Failed(err) else Done(get)

	/** Formats this `Maybe` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline override def mkString(prefix :String) :String =
		if (ref eq NoContent) prefix + "()" else prefix + "(" + ref + ")"

	/** Formats this `Maybe` as `s"Maybe($get)"` or `"Maybe()"`. */
	@inline override def mkString :String = if (ref eq NoContent) "Maybe()" else "Maybe(" + ref + ")"

	@inline override def toString :String = if (ref eq NoContent) "No" else "Yes(" + ref + ")"

	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[Maybe[_]]

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :Maybe[_]) :Boolean = (ref ne NoContent) & (other.ref ne NoContent) && ref == other.ref

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :Maybe[_]) :Maybe[Boolean] =
		if (ref eq NoContent) this.asInstanceOf[Maybe[Boolean]]
		else if (other.ref eq NoContent) other.asInstanceOf[Maybe[Boolean]]
		else Yes(ref == other.ref)
}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.sugar.vars.Maybe Maybe]]s.
  * @see [[net.noresttherein.sugar.vars.Maybe.No]]
  * @see [[net.noresttherein.sugar.vars.Maybe.Yes]]
  */
@SerialVersionUID(Ver)
object Maybe {
	/** Wraps the given object in a purely syntactic option-like object erased in the runtime. */
	@inline final def apply[T](value :T) :Maybe[T] =
		if (value == null) No else new Maybe(value.asInstanceOf[AnyRef])


	/** Converts the given `Option[T]` into a lighter `Maybe[T]`, which is erased at runtime. */
	@inline def some_?[T](value :Option[T]) :Maybe[T] =
		new Maybe(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Unsure[T]` into an `Maybe[T]`, erased at runtime. */
	@inline def sure_?[T](value :Unsure[T]) :Maybe[T] =
		new Maybe(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Opt[T]` into an `Maybe[T]` for interoperability. */
	@inline def one_?[T](value :Opt[T]) :Maybe[T] = One.unapply(value)

	/** Converts the given `Nullable[T]` into a `Opt[T]`, erased at runtime. */
	@inline def nonNull_?[T <: AnyRef](value :Nullable[T]) :Opt[T] = value.toOpt

	//Would be nice to rename these all methods to from[T](_), but,
	// for consistency, we should also do it for other option-like types,
	// and from[T](Maybe[T]) clashes with from[T](Opt[T]).
	/** Converts the given `Option[T]` into a lighter `Maybe[T]` which is erased at runtime. */
	@inline def fromOption[T](value: Option[T]) :Maybe[T] =
		new Maybe(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Unsure[T]` into an `Maybe[T]`, erased at runtime. */
	@inline def fromUnsure[T](value: Unsure[T]) :Maybe[T] =
		new Maybe(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Opt[T]` into an `Maybe[T]` for interoperability. */
	@inline def fromOpt[T](value :Opt[T]) :Maybe[T] = One.unapply(value)

	/** Converts the given `Nullable[T]` into a `Opt[T]`, erased at runtime. */
	@inline def fromNullable[T <: AnyRef](value :Nullable[T]) :Opt[T] = value.toOpt

	/** When a given condition is true, evaluates the `a` argument and returns `Yes(a).`
	  * When the condition is false, `a` is not evaluated and `No` is returned. */
	@inline def when[T](cond: Boolean)(a: => T) :Maybe[T] =
		new Maybe(if (cond) a.asInstanceOf[AnyRef] else NoContent)

	/** Unless a given condition is true, this will evaluate the `a` argument and return `Yes(a)`.
	  * Otherwise, `a` is not evaluated and `No` is returned. */
	@inline def unless[T](cond: Boolean)(a: => T) :Maybe[T] =
		new Maybe(if (!cond) a.asInstanceOf[AnyRef] else NoContent)

	/** Executes the given lazy expression in a `try-catch` block, returning `No` in case
	  * any exception is caught. Otherwise the value is returned as a `Yes` instance as normal. */
	@inline def guard[T](a : => T) :Maybe[T] =
		try Yes(a) catch {
			case _ :Exception => No
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `No` in case
	  * any exception is caught. Otherwise the result is returned as a `Yes` instance as normal. */
	@inline def guard[A, B](f :A => B)(a :A) :Maybe[B] =
		try Yes(f(a)) catch {
			case _ :Exception => No
		}

	/** Returns the first argument as `Yes` if it satisfies the predicate `p`.
	  * @return `Yes(value).filter(p)`. */
	@inline def satisfying[T](value :T)(p :T => Boolean) :Maybe[T] =
		if (p(value)) Yes(value) else No


	/** Returns [[net.noresttherein.sugar.vars.Maybe.No No]] - an empty `Maybe`. */
	@inline final def empty[T] :Maybe[T] = new Maybe(NoContent)

	//todo: remove this type and make No simply a Maybe[Nothing]
	/** A refinement of [[net.noresttherein.sugar.vars.Maybe Maybe]] marking it through a member flag type
	  * as empty. [[net.noresttherein.sugar.vars.Maybe.No Maybe.No]] is an instance of this type. */
	type No = Maybe[Nothing] { type isEmpty = true }

	/** A special, empty instance of [[net.noresttherein.sugar.vars.Maybe Maybe]] which conforms to any `Maybe[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  * @note This value is of a refined type `Maybe[Nothing] { type isEmpty = true }`. However, in many circumstances,
	  *       it is preferable to have a basic `Maybe[T]` for some specific type `T`.
	  *       In those cases you can use `Maybe.empty[T]`.
	  * @see [[net.noresttherein.sugar.vars.Maybe.empty]] */ //consider: renaming it to Lack
	@inline final val No :No = new Maybe(NoContent).asInstanceOf[No]

	//todo: remove this type and make Yes return simply Maybe.
	/** A refinement of [[net.noresttherein.sugar.vars.Maybe Maybe]] marking it through a member flag type
	  * as non-empty. [[net.noresttherein.sugar.vars.Maybe Maybe]] factory object creates instances
	  * narrowed down to this type. */
	type Yes[+T] = Maybe[T] { type isEmpty = false }

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.Maybe Maybe]]. */
	@SerialVersionUID(Ver)
	object Yes { //consider: renaming it to Got
		/** Creates a non-empty [[net.noresttherein.sugar.vars.Maybe Maybe]] wrapping the given value. */
		@inline def apply[T](x :T) :Yes[T] = new Maybe(x.asInstanceOf[AnyRef]).asInstanceOf[Yes[T]]

		/** Matches non-empty [[net.noresttherein.sugar.vars.Maybe Maybe]] instances. */
		@inline def unapply[T](opt :Maybe[T]) :Maybe[T] = opt
	}


	/** The for-comprehension facade for `Maybe[A]`, which does not evaluate the filter predicate until
	  * `map`, `flatMap` or `foreach` is called. */
	final class WithFilter[+A](self :Maybe[A], p :A => Boolean) {
		def map[B](f: A => B): Maybe[B] = self filter p map f
		def flatMap[B](f: A => Maybe[B]): Maybe[B] = self filter p flatMap f
		def foreach[U](f: A => U): Unit = self filter p foreach f
		def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
	}



	/** Implicit conversions between `Maybe` and `Option`.
	  * Conversions between `Maybe` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.conversions conversions]]. */
	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def MaybeToOption[T](opt :Maybe[T]) :Option[T] = opt.option
		@inline implicit def MaybeToIterable[T](opt :Maybe[T]) :Iterable[T] = opt.toIterable

		@inline implicit def OptionToMaybe[T](option :Option[T]) :Maybe[T] =
			new Maybe(if (option.isDefined) option.get.asInstanceOf[AnyRef] else NoContent)

		/** Implicitly lifts any value `T` to [[net.noresttherein.sugar.vars.Maybe Maybe]]`[T]`. */
		@inline implicit def anyToYes[T](x :T) :Yes[T] = new Maybe(x.asInstanceOf[AnyRef]).asInstanceOf[Yes[T]]

		@inline implicit def MaybeToOpt[T](opt :Maybe[T]) :Opt[T] = opt.opt
		@inline implicit def OptToMaybe[T](opt :Opt[T]) :Maybe[T] = Maybe.fromOpt(opt)

		@inline implicit def MaybeToNullable[T <: AnyRef](opt :Maybe[T]) :Nullable[T] = Nullable.fromMaybe(opt)
		@inline implicit def NullableToMaybe[T <: AnyRef](opt :Nullable[T]) :Maybe[T] = opt.toMaybe
	}

	/** Importing the contents of this object replace all usage of [[Option]]/[[Some]]/[[None]] in the scope with
	  * [[net.noresttherein.sugar.vars.Maybe Maybe]]/[[net.noresttherein.sugar.vars.Maybe.Yes Yes]]/[[net.noresttherein.sugar.vars.Maybe.No No]].
	  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
	  * which allow seamless interoperability with standard Scala APIs.
	  *
	  * While the idea of using it in production code should be carefully reconsidered,
	  * it is useful for quickly checking during profiling what impact using `Option` vs `Maybe` has.
	  *
	  * Other files which reference classes defined in the import's scope may also need to be modified in order
	  * to comply with changed interfaces. */
	@SerialVersionUID(Ver)
	object OptAsOption {
		type Option[T] = Maybe[T]
		type Some[T]   = Yes[T]

		val Option = Maybe
		val Some   = Yes
		val None   = No
		//same names as in conversions so if both are imported one shadows the other
		@inline implicit def MaybeToOption[T](opt :Maybe[T]) :scala.Option[T] = opt.option
		@inline implicit def OptionToMaybe[T](opt :scala.Option[T]) :Maybe[T] = some_?(opt)
		@inline implicit def SomeToYes[T](opt :scala.Some[T]) :Yes[T] = Yes(opt.get)
		@inline implicit def GotToYes[T](opt :Sure[T]) :scala.Some[T] = opt.option.asInstanceOf[scala.Some[T]]

		@inline implicit def NoneToNo(none :scala.None.type) :No.type = No
		@inline implicit def NoToNone(miss :No.type) :scala.None.type = scala.None
	}


	/** The value wrapped by an empty `Maybe`. In order to avoid ambiguity, it must not be used for any other purpose.
	  * We don't use Ref.undefined, because that placeholder may actually be standing in for a `Maybe`,
	  * leading to ambiguity between `None`/`No` and 'not computed'.
	  * Extends Any => AnyRef out of laziness, allowing it to pass as the argument to `applyOrElse`
	  * to implement 'apply partial function to `this.get`, but if the former is undefined at the latter,
	  * the result is empty' (`NoContent.apply` returns itself).
	  * It is `private[vars]` so that methods of Maybe can be inlined
	  * and because some `Ref` classes play with the erasure and need access to this marker object. */
	@SerialVersionUID(Ver)
	private[vars] object NoContent extends (Any => AnyRef) with Serializable {
		def apply(ignore :Any) :AnyRef = this
		override def toString = "<empty>"
	}

	private val unzip2Lack = (No, No)
	private val unzip3Lack = (No, No, No)
}

