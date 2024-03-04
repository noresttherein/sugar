package net.noresttherein.sugar.vars

import scala.reflect.ClassTag

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.exceptions.{illegal_!, noSuch_!, outOfBounds_!, raise, unsupported_!}
import net.noresttherein.sugar.null_!
import net.noresttherein.sugar.vars.Maybe.Yes
import net.noresttherein.sugar.vars.Nullable.{FallbackToNull, NonNull, Null, unzip2Lack, unzip3Lack}
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Unsure.conversions.sureAny




/** A non-boxing, option-like monadic wrapper of reference types, erased to the wrapped type in runtime.
  * Empty instance ([[net.noresttherein.sugar.vars.Nullable.Null IsNull]]) is encoded as a `null` value;
  * this means it is impossible to create a 'full' [[net.noresttherein.sugar.vars.Nullable.NonNull NonNull]]
  * instance containing a `null` value, and `NonNull(null)` will throw a `NullPointerException`.
  *
  * It behaves exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  * recursion/loops. As a value class, it has no distinct subclasses for empty and non-empty instances, which results
  * in certain differences from `Option`. Aside from the obvious lack of creation of an additional object,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, a disadvantage
  * of being erased in runtime is that a method accepting a `Nullable[T]` will clash with an overloaded method
  * accepting `T`. Moreover, a generic method accepting/returning an abstract type (parameter) `T`
  * cannot be overridden/implemented by a method accepting an `Nullable[O]`, where `O` is a type parameter,
  * an abstract type, or a reference type (this is a limitation of the current Scala compiler).
  *
  * Using a `Nullable` via a super trait, such as [[net.noresttherein.sugar.vars.Ref Ref]]
  * or [[scala.collection.IterableOnce IterableOnce]], or passing it as a type parameter (in particular, using
  * it as a function parameter or returned value), will however result in boxing
  * which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  * composing several `Nullable`s' can result in closures being created (as manual nesting of `flatMap` calls also can).
  * For this reason its best to either directly use `isEmpty` and `get` in a conditional expression, or use the
  * [[net.noresttherein.sugar.vars.Nullable.NonNull NonNull]] matching pattern, which should by be translated
  * by the compiler to non-boxing byte code.
  *
  * $optionalTypesInfo
  *
  * As `Nullable` is a value class, and its type parameters are reference types, `Nullable` cannot nest.
  * The limited disambiguouity potential this affords, together with the fact that it will never result in boxing
  * unless used in position of an abstract type, allows implicit conversion `T => Nullable[T]` to be enabled
  * by default, which serves as another differentiator from `Maybe`.
  *
  * @note This class is provided primarily for situations where a method accepting `Maybe` or `Opt` would clash
  *       with an overloaded method with the same erasure, due to usage of an abstract type as a parameter
  *       in the same position. In most other scenarios, use of `Maybe` or `Opt` is preferable.
  * @see [[net.noresttherein.sugar.vars.Nullable.NonNull]]
  * @see [[net.noresttherein.sugar.vars.Nullable.Null]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  * @define Ref `Nullable`
  * @define ref optional value
  * @define coll optional value
  */
@SerialVersionUID(Ver)
class Nullable[+A <: AnyRef] private[Nullable](private val ref :A) //private[Nullable] to allow inlining of its construction
	extends AnyVal with Ref[A] with IterableOnce[A] with Product with Equals with Serializable
{
	@inline override def isFinal = true

	/** Tests if this `Nullable` does not contain a value (is equal to [[net.noresttherein.sugar.vars.Nullable.Null Null]]). */
	@inline override def isEmpty :Boolean = ref eq null

	/** Tests if this `Nullable` contains a value. If true, `get` will not throw an exception. */
	@inline override def nonEmpty: Boolean = ref ne null

	/** Tests if this `Nullable` contains a value. This is the same as `nonEmpty`. */
	@inline override def isConst :Boolean = ref ne null

	/** Tests if this `Nullable` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined :Boolean = ref ne null

	/** Tests if this `Nullable` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefinite :Boolean = ref ne null

	/** Tests if this `Nullable` contains a value. This is the same as `nonEmpty`. */
	@inline override def isFinalizable :Boolean = ref ne null

	@inline override def knownSize :Int = if (ref eq null) 0 else 1

	/** Returns `1` if the `Nullable` carries a value and `0` otherwise. */
	@inline def size :Int = if (ref eq null) 0 else 1

	@inline override def productArity :Int = if (ref eq null) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && (ref ne null)) ref
		else outOfBounds_!(toString + ".productElement(" + n + ")")


	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Nullable` is empty. */
	@inline override def get :A =
		if (ref eq null) noSuch_!("Null.get") else ref

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws NoSuchElementException if this `Nullable` is empty. */
	@inline override def value :A =
		if (ref eq null) noSuch_!("Null.value") else ref

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `Nullable` is empty. */
	@inline override def const :A =
		if (ref eq null) unsupported_!("Null.const") else ref

	/** Forces extraction of the value.
		* @return contained value, if one exists.
		* @throws UnsupportedOperationException if this `Nullable` is empty. */
	@inline override def apply() :A =
		if (ref eq null) unsupported_!("Null.const") else ref

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: A](or: => O) :O =
		if (ref eq null) or else ref

	/** Similarly to [[net.noresttherein.sugar.vars.Nullable.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def orDefault[O >: A](or: O) :O =
		if (ref eq null) or else ref

	/** Assuming that `A` is a nullable type, return `null` if this `Nullable` is empty, or the wrapped value otherwise. */
	@inline def orNull[O >: A](implicit isNullable :Null <:< O) :O =
		if (ref eq null) null.asInstanceOf[O] else ref
//
//	/** Gets the element in the `Nullable` or throws the exception given as the argument.
//	  * @see [[net.noresttherein.sugar.vars.Nullable.orNoSuch orNoSuch]]
//	  * @see [[net.noresttherein.sugar.vars.Nullable.orIllegal orIllegal]] */
//	@inline def orThrow(e: => Throwable) :A =
//		if (ref eq null) throw e else ref

	/** Gets the element in the `Nullable` or throws the exception given as the type parameter with the given message.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E(msg) }`.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Nullable.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Nullable.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :A =
		if (ref eq null) raise[E](msg) else ref

	/** Gets the element in the `Nullable` or throws the exception given as the type parameter.
	  * Note that this method uses reflection to find and call the exception constructor and will not be as efficient
	  * as `this getOrElse { throw new E }`.
	  * @tparam E an exception class which must provide either a public default constructor,
	  *           a constructor accepting a single `String` argument,
	  *           or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Nullable.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Nullable.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag] :A =
		if (ref eq null) raise[E] else ref

	/** Gets the element in this `Nullable` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Nullable.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :A =
		if (ref eq null) noSuch_!(msg) else ref

	/** Gets the element in this `Nullable` or throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Nullable.orThrow orThrow]] */
	@inline def orNoSuch :A =
		if (ref eq null) noSuch_! else ref

	/** Gets the element in this `Nullable` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Nullable.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :A =
		if (ref eq null) illegal_!(msg) else ref

	/** Gets the element in this `Nullable` or throws an [[IllegalArgumentException]].
	  * @see [[net.noresttherein.sugar.vars.Nullable.orThrow orThrow]] */
	@inline def orIllegal :A =
		if (ref eq null) illegal_! else ref

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError(msg: => String) :A = {
		assert(ref ne null, msg)
		ref
	}

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError :A = {
		assert(ref ne null)
		ref
	}



	/** Returns the value this `Nullable` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse[O >: A <: AnyRef](or: => Nullable[O]) :Nullable[O] =
		if (ref eq null) or else this

	/** Similarly to [[net.noresttherein.sugar.vars.Nullable.orElse orElse]], returns this `Nullable` if it is not empty
	  * and `or` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param or the value to return if this instance is empty. */
	@inline def ifEmpty[O >: A <: AnyRef](or: Nullable[O]) :Nullable[O] =
		if (ref eq null) or else this

	/** Returns this `Nullable` if the condition is false and `Null` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyIf(condition :Boolean) :Nullable[A] =
		if (condition) Null else this

	/** Returns this `Nullable` if the condition is true and `Null` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
	@inline def orEmptyUnless(condition :Boolean) :Nullable[A] =
		if (condition) this else Null



	/** Returns a new `Nullable` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O <: AnyRef](f :A => O) :Nullable[O] =
		if (ref eq null) this.asInstanceOf[Nullable[O]] else new Nullable(f(ref))

	/** Applies the given function to the content of this `Nullable` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse or`, but in one step. */
	@inline def mapOrElse[O](f :A => O, or: => O) :O =
		if (ref eq null) or else f(ref)

	/** Returns the result of applying `f` to the value of this `Nullable` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  *
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Nullable.mapOrElse mapOrElse]] instead.
	  * @param ifEmpty the expression to evaluate if empty.
	  * @param f       the function to apply if nonempty. */
	@inline def fold[O](ifEmpty: => O)(f: A => O) :O =
		if (ref eq null) ifEmpty else f(ref)

	//consider: returning Outcome instead (here and in other Option-like types)
	/** The same as [[net.noresttherein.sugar.vars.Nullable.map map]], but exceptions thrown by the function
	  * are caught and [[net.noresttherein.sugar.vars.Nullable.Null Null]] is returned instead. */
	@inline def guardMap[O <: AnyRef](f :A => O) :Nullable[O] =
		if (ref eq null)
			Null
		else try {
			new Nullable(f(ref))
		} catch {
			case _ :Exception => Null
		}

	/** Returns the result of applying the given function to the value of this `Nullable` if it is not empty,
	  * or `this` if `this.isEmpty`. */
	@inline def flatMap[O <: AnyRef](f :A => Nullable[O]) :Nullable[O] =
		if (ref eq null) Null else f(ref)

	/** Flattens `Nullable[Nullable[O]]` to a single `Nullable[O]`. */
	def flatten[O <: AnyRef](implicit isNullable :A <:< Nullable[O]) :Nullable[O] =
		(ref :Any) match {
			case null                        => Null
			case opt :Nullable[O @unchecked] => opt
			case _                           => new Nullable(ref.asInstanceOf[O])
		}

	/** Returns an empty `Nullable` if `this.contains(o)`, or `this` otherwise. */
	@inline def removed[O >: A](o :O) :Nullable[A] =
		if (ref == o) Null else this

	/** Returns an empty `Nullable` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
	def removedAll[O >: A](that :IterableOnce[O]) :Nullable[A] = that match {
		case _ if ref eq null => this
		case it :Set[O] => if (it(get)) Null else this
		case it :Ranking[O] => if (it.contains(get)) Null else this
		case it :Iterable[O] if it.isEmpty => this
		case _ =>
			val i = that.iterator
			val x = get
			while (i.hasNext)
				if (i.next() == x)
					return Null
			this
	}

	/** Returns a new `Nullable` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Nullable.Null Null]] otherwise. */
	@inline def filter(p :A => Boolean) :Nullable[A] =
		if ((ref eq null) || p(ref)) this else Null

	/** Returns a new `Nullable` containing this value if it is not empty and its value falsifies the given predicate,
	  * or [[net.noresttherein.sugar.vars.Nullable.Null Null]] otherwise. */
	@inline def filterNot(p :A => Boolean) :Nullable[A] =
		if ((ref eq null) || !p(ref)) this else Null

	/** Equivalent to `this.`[[net.noresttherein.sugar.vars.Nullable.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :A => Boolean) :Nullable[A] =
		if ((ref eq null) || p(ref)) this else Null


	/** Tests if this `Nullable` is not empty and its value is equal to the given argument. */
	@inline def contains[O >: A](o :O): Boolean = ref == o

	/** Tests if this `Nullable` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :A => Boolean): Boolean = (ref ne null) && p(ref)

	/** Tests if this `Nullable` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :A => Boolean): Boolean = (ref eq null) || p(ref)

	/** Executes the given block for this `Nullable`s value if it is not empty. */
	@inline def foreach[O](f :A => O) :Unit = if (ref ne null) f(ref)

	/** Returns an empty `Nullable` if this `Nullable` is empty or the partial function `f` is not defined for its value,
	  * otherwise applies it and wraps the result it in a new `Nullable`. */
	@inline def collect[O <: AnyRef](f :PartialFunction[A, O]) :Nullable[O] =
		if (ref eq null)
			Null
		else
			new Nullable(f.applyOrElse(ref, FallbackToNull.asInstanceOf[Any => O]))


	/** Returns a `Nullable` formed from the contents of `this` and `that` by combining the corresponding elements in a pair.
	  * If either of the two options is empty, `Null` is returned. */
	@inline def zip[O <: AnyRef](that :Nullable[O]) :Nullable[(A, O)] =
		if ((ref eq null) | (that.ref eq null)) new Nullable(null)
		else NonNull((ref, that.ref))

	/** Converts a `Nullable` of a pair into `Nullable`s of its first and second elements. */
	@inline def unzip[A1 <: AnyRef, A2 <: AnyRef](implicit asPair: A <:< (A1, A2)): (Nullable[A1], Nullable[A2]) =
		if (ref eq null)
			unzip2Lack
		else (
			new Nullable[A1](ref.asInstanceOf[(A1, A2)]._1),
			new Nullable[A2](ref.asInstanceOf[(A1, A2)]._2)
		)

	/** Converts a `Nullable` of a triple into three `Nullable`s, one containing the element from each position of the triple. */
	@inline def unzip3[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef]
	                  (implicit asTriple: A <:< (A1, A2, A3)): (Nullable[A1], Nullable[A2], Nullable[A3]) =
		if (ref eq null)
			unzip3Lack
		else (
			new Nullable[A1](ref.asInstanceOf[(A1, A2, A3)]._1),
			new Nullable[A2](ref.asInstanceOf[(A1, A2, A3)]._2),
			new Nullable[A3](ref.asInstanceOf[(A1, A2, A3)]._3)
		)


	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline override def iterator :Iterator[A] =
		if (ref eq null) Iterator.empty else Iterator.single(ref)

	/** Returns `Nil` if this `Nullable` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[A] = if (ref eq null) Nil else ref::Nil

	/** Returns an empty list if this `Nullable` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[A] = if (ref eq null) Nil else ref::Nil

	/** Returns an empty collection if this `Nullable` is empty or a singleton with its value otherwise. */
	@inline def toIterable :Iterable[A] = if (ref eq null) Iterable.empty else Iterable.single(ref)

	/** Conversion to standard Scala [[scala.Option]]. Same as [[net.noresttherein.sugar.vars.Nullable.toOption toOption]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def option :Option[A] = if (ref eq null) None else Some(ref)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def toOption :Option[A] = if (ref eq null) None else Some(ref)

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def constOption :Option[A] = if (ref eq null) None else Some(ref)

	@inline override def maybe      :Maybe[A] = if (ref eq null) Maybe.No else Maybe.Yes(ref)
	@inline override def toMaybe    :Maybe[A] = if (ref eq null) Maybe.No else Maybe.Yes(ref)
	@inline override def maybeConst :Maybe[A] = if (ref eq null) Maybe.No else Maybe.Yes(ref)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in a `Nullable`.
	  * Same as [[net.noresttherein.sugar.vars.Nullable.toUnsure toUnsure]]. */
	@inline override def unsure :Unsure[A] =
		if (ref eq null) Missing else new Sure(ref, cachedOpt = toOpt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in a `Nullable`. */
	@inline override def toUnsure :Unsure[A] =
		if (ref eq null) Missing else new Sure(ref, cachedOpt = toOpt)

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in a `Nullable`.
      * Same as [[net.noresttherein.sugar.vars.Nullable.toUnsure toUnsure]]. */
	@inline override def unsureConst :Unsure[A] =
		if (ref eq null) Missing else new Sure(ref, cachedOpt = toOpt)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. Same as [[net.noresttherein.sugar.vars.Nullable.toOpt toOpt]]. */
	@inline override def opt :Opt[A] = if (ref eq null) None else One(ref)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. */
	@inline override def toOpt :Opt[A] = if (ref eq null) None else One(ref)

	/** Conversion to a fully erased `Opt` carrying the same value as this instance, if any.
	  * This conversion does not require boxing. Same as [[net.noresttherein.sugar.vars.Nullable.toOpt toOpt]]. */
	@inline override def constOpt :Opt[A] = if (ref eq null) None else One(ref)


	/** Converts this `Nullable` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[A, O] =
		if (ref eq null) Right(right) else Left(ref)

	/** Converts this `Nullable` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, A] =
		if (ref eq null) Left(left) else Right(ref)


	/** Converts this `Nullable` to `Pill`, returning the content as `Red`, or the value of the given expression
	  * as `Blue` if empty. */
	@inline def toRed[O](blue: => O) :Pill[A, O] =
		if (ref eq null) Blue(blue) else Red(ref)

	/** Converts this `Nullable` to `Pill`, returning the content as `Blue`, or the value of the given expression
	  * as `Red` if empty. */
	@inline def toBlue[O](red: => O) :Pill[O, A] =
		if (ref eq null) Red(red) else Blue(ref)

	/** Converts this `Nullable` to `Outcome`, returning the content as `Done`,
	  * or the value of the given `String` as `Failed` error message if empty. */
	@inline def doneOr(err : => String) :Outcome[A] =
		if (ref eq null) Failed(() => err) else Done(get)

	/** Converts this `Nullable` to `Outcome`, returning the content as `Done`,
	  * or the value of the given `Throwable` as an error if empty. */
	@inline final def doneOr(err :Throwable) :Outcome[A] =
		if (ref eq null) Failed(err) else Done(get)

	/** Formats this `Nullable` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
	@inline override def mkString(prefix :String) :String =
		if (ref eq null) prefix + "()" else prefix + "(" + ref + ")"

	/** Formats this `Nullable` as `s"Nullable($get)"` or `"Nullable()"`. */
	@inline override def mkString :String = if (ref eq null) "Nullable()" else "Nullable(" + ref + ")"

	@inline override def toString :String = if (ref eq null) "Null" else "NonNull(" + ref + ")"

	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[Nullable[_]]

	/** Compares the contents for equality, with the result being false if any of the operands are empty. */
	@inline def same(other :Nullable[_]) :Boolean = (ref ne null) & (other.ref.asAnyRef ne null) && ref == other.ref

	/** Returns `for (a <- this; b <- other) yield a == b`. */
	@inline def sameOpt(other :Nullable[_]) :Ternary =
		if (ref eq null) Ternary.Unknown
		else if (other.ref.asAnyRef eq null) Ternary.Unknown
		else Ternary(ref == other.ref)
}




/** Companion object providing factory methods and extractors working
  * with [[net.noresttherein.sugar.vars.Nullable Nullable]]s.
  * @see [[net.noresttherein.sugar.vars.Nullable.Null]]
  * @see [[net.noresttherein.sugar.vars.Nullable.NonNull]]
  */
@SerialVersionUID(Ver)
object Nullable {
	/** Wraps the given object in a purely syntactic option-like object erased in the runtime. */
	@inline final def apply[T <: AnyRef](value :T) :Nullable[T] = new Nullable(value)

	/** Converts the given `Option[T]` into a lighter `Nullable[T]`, which is erased at runtime. */
	@inline def some_?[T <: AnyRef](value :Option[T]) :Nullable[T] = fromOption(value)

	/** Converts the given `Opt[T]` into an `Nullable[T]` for interoperability. */
	@inline def one_?[T <: AnyRef](value :Opt[T]) :Nullable[T] = fromMaybe(value.toMaybe)

	/** Converts the given `Unsure[T]` into an `Nullable[T]`, erased at runtime. */
	@inline def sure_?[T <: AnyRef](value :Unsure[T]) :Nullable[T] = fromUnsure(value)

	/** Converts the given `Maybe[T]` into an `Nullable[T]`, erased at runtime. */
	@inline def yes_?[T <: AnyRef](value :Maybe[T]) :Nullable[T] = fromMaybe(value)

	/** Returns `Nullable()` */
	@inline def fromRef[T <: AnyRef](value :Ref[T]) :Nullable[T] = fromMaybe(value.toMaybe)

	/** Converts the given `Option[T]` into a lighter `Nullable[T]` which is erased at runtime. */
	def fromOption[T <: AnyRef](value: Option[T]) :Nullable[T] = value match {
		case Some(null) => null_!("Cannot convert Some(null) to non empty Nullable")
		case Some(v)    => new Nullable(v)
		case _          => Null
	}

	/** Converts the given `Opt[T]` into an `Nullable[T]` for interoperability. */
	def fromOpt[T <: AnyRef](value :Opt[T]) :Nullable[T] = value.toMaybe match {
		case Yes(null) => null_!("Cannot convert One(null) to non empty Nullable")
		case Yes(v)    => new Nullable(v)
		case _         => Null
	}

	/** Converts the given `Option[T]` into a lighter `Nullable[T]` which is erased at runtime. */
	def fromMaybe[T <: AnyRef](value: Maybe[T]) :Nullable[T] = value match {
		case Yes(null) => null_!("Cannot convert Yes(null) to non empty Nullable")
		case Yes(v)    => new Nullable(v)
		case _         => Null
	}

	/** Converts the given `Unsure[T]` into an `Nullable[T]`, erased at runtime. */
	def fromUnsure[T <: AnyRef](value: Unsure[T]) :Nullable[T] = value match {
		case Sure(null) => null_!("Cannot convert Sure(null) to non empty Nullable")
		case Sure(v)    => new Nullable(v)
		case _          => Null
	}

	/** When a given condition is true, evaluates the `a` argument and returns `Nullable(a).`
	  * When the condition is false, `a` is not evaluated and `Null` is returned. */
	@inline def when[T <: AnyRef](cond: Boolean)(a: => T) :Nullable[T] =
		new Nullable(if (cond) a else null.asInstanceOf[T])

	/** Unless a given condition is true, this will evaluate the `a` argument and return `Nullable(a)`.
	  * Otherwise, `a` is not evaluated and `Null` is returned. */
	@inline def unless[T <: AnyRef](cond: Boolean)(a: => T) :Nullable[T] =
		new Nullable(if (!cond) a else null.asInstanceOf[T])

	/** Executes the given lazy expression in a `try-catch` block, returning `Null` in case
	  * any exception is caught. Otherwise the value is returned as a `NonNull` instance as normal. */
	@inline def guard[T <: AnyRef](a : => T) :Nullable[T] =
		try new Nullable(a) catch {
			case _ :Exception => Null
		}

	/** Applies the given function to the second argument in a `try-catch` block, returning `Null` in case
	  * any exception is caught. Otherwise, returns `Nullable(f(a))`. */
	@inline def guard[A, B <: AnyRef](f :A => B)(a :A) :Nullable[B] =
		try new Nullable(f(a)) catch {
			case _ :Exception => Null
		}

	/** Returns the first argument as `NonNull` if it satisfies the predicate `p`.
	  * @return `NonNull(value).filter(p)`. */
	@inline def satisfying[T <: AnyRef](value :T)(p :T => Boolean) :Nullable[T] =
		if (p(value)) new Nullable(value) else Null


	/** Returns [[net.noresttherein.sugar.vars.Nullable.Null Null]] - an empty `Nullable`. */
	@inline final def empty[T <: AnyRef] :Nullable[T] = new Nullable(null.asInstanceOf[T])

	/** A special, empty instance of [[net.noresttherein.sugar.vars.Nullable Nullable]] which conforms to any `Nullable[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  * @note This value is of a refined type `Nullable[Nothing] { type isEmpty = true }`. However, in many circumstances,
	  *       it is preferable to have a basic `Nullable[T]` for some specific type `T`.
	  *       In those cases you can use `Nullable.empty[T]`.
	  * @see [[net.noresttherein.sugar.vars.Nullable.empty]] */
	@inline final val Null :Nullable[Nothing] = new Nullable(null).asInstanceOf[Nullable[Nothing]]

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.Nullable Nullable]]. */
	@SerialVersionUID(Ver)
	object NonNull { //consider: renaming it to Got
		/** Creates a non-empty [[net.noresttherein.sugar.vars.Nullable Nullable]] wrapping the given value. */
		@inline def apply[T <: AnyRef](x :T) :Nullable[T] =
			if (x eq null) null_!("Cannot construct NonNull(null)")
			else new Nullable(x)

		/** Matches non-empty [[net.noresttherein.sugar.vars.Nullable Nullable]] instances. */
		@inline def unapply[T <: AnyRef](opt :Nullable[T]) :Maybe[T] = opt.maybe
	}


	/** Lifts any reference type to a `Nullable`. */
	@inline implicit def AnyRefToNullable[T <: AnyRef](any :T) :Nullable[T] = new Nullable(any)

	/** Implicit conversions between `Nullable` and `Option`.
	  * Conversions between `Nullable` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.conversions conversions]];
	  * conversions to and from [[net.noresttherein.sugar.vars.Opt! Opt]]
	  * and [[net.noresttherein.sugar.vars.Maybe Maybe]] are likewise located in `conversions` objects
	  * within their companion objects.
	  *
	  * Additionally, [[net.noresttherein.sugar.vars.Nullable.conversions.AnyRefToNullable AnyRefToNullable]]
	  * will lift any reference value to `Nullable` (empty if `null`, full otherwise).
	  */
	@SerialVersionUID(Ver)
	object conversions {
		@inline implicit def NullableToOption[T <: AnyRef](opt :Nullable[T]) :Option[T] = opt.option
		@inline implicit def NullableToIterable[T <: AnyRef](opt :Nullable[T]) :Iterable[T] = opt.toIterable

		@inline implicit def OptionToNullable[T <: AnyRef](option :Option[T]) :Nullable[T] =
			new Nullable(if (option.isDefined) option.get else null.asInstanceOf[T])

		@inline implicit def NullableToOpt[T <: AnyRef](opt :Nullable[T]) :Opt[T] = opt.opt
		@inline implicit def OptToNullable[T <: AnyRef](opt :Opt[T]) :Nullable[T] = Nullable.fromOpt(opt)
	}

	private val unzip2Lack = (Null, Null)
	private val unzip3Lack = (Null, Null, Null)
	private val FallbackToNull = (any :Any) => null
}

