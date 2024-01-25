	package net.noresttherein.sugar

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

import net.noresttherein.sugar
import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Potential.{Existent, Inexistent, NonExistent}
import net.noresttherein.sugar.exceptions.SugaredThrowable




/** A home to a wide class hierarchy of value wrappers: [[net.noresttherein.sugar.vars.InOut in/out]]
  * method parameters, [[net.noresttherein.sugar.vars.Atomic atomic]]
  * and [[net.noresttherein.sugar.vars.ThreadLocal thread local]] variables,
  * several [[net.noresttherein.sugar.vars.Lazy lazy]] `val` implementations,
  * [[net.noresttherein.sugar.vars.Opt various]] `Option` [[net.noresttherein.sugar.vars.Unsure alternatives]],
  * [[net.noresttherein.sugar.vars.Channel synchronization]] tools
  * and [[net.noresttherein.sugar.vars.SignalVar conditional variables]],
  * [[net.noresttherein.sugar.vars.Watched observed]] variables,
  * disposable [[java.lang.ref.Reference]] [[net.noresttherein.sugar.vars.DisposableRef adapters]] and others.
  *
  * @define Potential [[net.noresttherein.sugar.vars.Potential! Potential]]
  * @define Existent [[net.noresttherein.sugar.vars.Potential.Existent$ Existent]]
  * @define Inexistent [[net.noresttherein.sugar.vars.Potential.Inexistent$ Inexistent]]
  * @define Outcome [[net.noresttherein.sugar.vars.Outcome! Outcome]]
  * @define Done [[net.noresttherein.sugar.vars.Outcome.Done Done]]
  * @define Failed [[net.noresttherein.sugar.vars.Outcome.Failed Failed]]
  * @define Pill [[net.noresttherein.sugar.vars.Pill! Pill]]
  * @define Blue [[net.noresttherein.sugar.vars.Pill.Blue$ Blue]]
  * @define Red  [[net.noresttherein.sugar.vars.Pill.Red$ Red]]
  */
package object vars extends vars.Rank1PotentialImplicits {

//	type Wish[+A]
//	type Granted[+A] <: Wish[A]
//	val Denied :Wish[Nothing] = ???
//
//	type Optional[+A]
//	type Defined[+A] <: Optional[A]
//	val Undefined = ???
//
//	type Space[+A]
//	type Filled[+A] <: Space[A]
//	val Blank :Space[Nothing]

	private[vars] final val Ver = sugar.Ver

	/** An erased variant of [[scala.Option]], with API defined by extension methods
	  * in [[net.noresttherein.sugar.vars.PotentialExtension PotentialExtension]].
	  * A `Potential[A]` can be have three forms:
	  *   1. $Inexistent, an [[net.noresttherein.sugar.vars.PotentialExtension.isEmpty empty]] instance corresponding
	  *      to [[scala.None]];
	  *   1. $Existent[A], erased to `A` (which may be a boxed version of a value type, both inbuilt or a value class);
	  *   1. $Existent[A], wrapped - used to differentiate `Existent(Inexistent)` from `Inexistent`.
	  * Outside of nesting `Potential` instances within each other, no boxing takes place for reference types
	  * under any circumstances, in particular when creating an `Array[Potential[A]]` or `Seq[Potential[A]]`,
	  * using it as an argument or return value of a function, or, in general, substituting it for an abstract type
	  * (including type parameters). It is for this reason well suited to monadic composition of computations
	  * through `flatMap`, avoiding boxing of the result of each step. The type is not `@specialized`,
	  * so boxing of value types to their Java wrappers, as well as lifting of value classes
	  * to their reference representations, will still occur: all values and arguments
	  * of type `Potential[A]` are erased to `AnyRef` by the compiler. Code
	  * {{{
	  *     val qoluae = Potential(42)
	  * }}}
	  * will compile to `new Integer(42)`, unless aggressively inlined by the compiler.
	  *
	  * The downside is the same as with opaque types, a `Potential[A]` selector in pattern matching may match
	  * patterns for values of `A` itself.
	  *
	  * Implicit conversions exists to and from `Opt[A]` in order to improve interoperability; they do not involve
	  * any boxing in either direction, except for the case `Got(Inexistent) :Existent[_]`.
	  * @see [[net.noresttherein.sugar.vars.Opt]]
	  * @see [[net.noresttherein.sugar.vars.Unsure]]
	  */ //consider: we could use >: None; but then how to rename Existent? Stuff?
	type Potential[+A] //>: None.type// >: Existent[A] //other names: Hope >: Vain/? or Lucky/NoLuck; Maybe; Perhaps; Yield > One | None

	/** An alias for $Potential`[A]`, a fully erased variant of [[scala.Option]] with an API defined
	  * by [[net.noresttherein.sugar.vars.PotentialExtension PotentialExtension]] as extension methods.
	  * @see [[net.noresttherein.sugar.vars.Potential.Existent$]]
	  * @see [[net.noresttherein.sugar.vars.Potential.Inexistent]]
	  * @see [[net.noresttherein.sugar.vars.Opt]]
	  * @see [[net.noresttherein.sugar.vars.Unsure]]
	  */
	type ??[+A] = Potential[A]


	/** The API of $Potential in the form of extension methods.
	  * @define Ref `Potential`
	  * @define coll potential value
	  */
	implicit class PotentialExtension[A](private val self :Potential[A]) extends AnyVal {

		/** Tests if this `Potential` does not contain a value
		  * (is equal to [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]]). */
		@inline def isEmpty: Boolean = self.asInstanceOf[AnyRef] eq NonExistent

		/** Tests if this `Potential` is defined. If true, `get` will not throw an exception. */
		@inline def nonEmpty: Boolean = self.asInstanceOf[AnyRef] ne NonExistent

		/** Tests if this `Potential` contains a value. This is the same as `nonEmpty`. */
		@inline def isConst: Boolean = self.asInstanceOf[AnyRef] ne NonExistent

		/** Tests if this `Potential` contains a value. This is the same as `nonEmpty`. */
		@inline def isDefined: Boolean = self.asInstanceOf[AnyRef] ne NonExistent

		/** Tests if this `Potential` contains a value. This is the same as `nonEmpty`. */
		@inline def isDefinite :Boolean = self.asInstanceOf[AnyRef] ne NonExistent

		/** Returns `1` if the `Potential` carries a value and `0` otherwise. */
		@inline def size :Int = if (self.asInstanceOf[AnyRef] eq NonExistent) 0 else 1

		/** Forces extraction of the value.
		  * @return contained value, if one exists.
		  * @throws NoSuchElementException if this `Potential` is empty. */
		@inline def get :A = (self :Any) match {
			case NonExistent                    => throw new NoSuchElementException("Inexistent.get")
			case exists :Existent[A @unchecked] => exists.value
			case _                              => self.asInstanceOf[A]
		}

		/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
		@inline def getOrElse[O >: A](or: => O) :O =
			if (self.asInstanceOf[AnyRef] eq NonExistent) or else get

		/** Similarly to [[net.noresttherein.sugar.vars.PotentialExtension.getOrElse getOrElse]],
		  * returns the value if non-empty and `alt` otherwise. The difference is that the alternative value
		  * is not lazily computed and guarantees no closure will be created,
		  * at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def orDefault[O >: A](or: O) :O =
			if (self.asInstanceOf[AnyRef] eq NonExistent) or else get

		/** Assuming that `A` is a nullable type, return `null` if this `Potential` is `Inexistent`,
		  * or the wrapped value otherwise. */
		@inline def orNull[O >: A](implicit isNullable :Null <:< O) :O =
			if (self.asInstanceOf[AnyRef] eq NonExistent) null.asInstanceOf[O] else get

//
//		/** Gets the element in the $Ref or throws the exception given as the argument.
//		  * @see [[net.noresttherein.sugar.vars.PotentialExtension.orNoSuch orNoSuch]]
//		  * @see [[net.noresttherein.sugar.vars.PotentialExtension.orIllegal orIllegal]] */
//		@inline def orThrow(e : => Throwable) :A =
//			if (self.asInstanceOf[AnyRef] eq NonExistent) throw e else get

		/** Gets the element in this `Potential` or throws the exception given as the type parameter
		  * with the given message.
		  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
		  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  * @see [[net.noresttherein.sugar.vars.PotentialExtension.orNoSuch orNoSuch]]
		  * @see [[net.noresttherein.sugar.vars.PotentialExtension.orIllegal orIllegal]] */
		@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :A =
			if (self.asInstanceOf[AnyRef] eq NonExistent) raise[E](msg) else get

		/** Gets the element in this `Potential` or throws a [[NoSuchElementException]] with the given message.
		  * @see [[net.noresttherein.sugar.vars.PotentialExtension.orThrow orThrow]] */
		@inline def orNoSuch(msg: => String) :A =
			if (self.asInstanceOf[AnyRef] eq NonExistent) throw new NoSuchElementException(msg) else get

		/** Gets the element in this `Potential` or throws an [[IllegalArgumentException]] with the given message.
		  * @see [[net.noresttherein.sugar.vars.PotentialExtension.orThrow orThrow]] */
		@inline def orIllegal(msg: => String) :A =
			if (self.asInstanceOf[AnyRef] eq NonExistent) throw new IllegalArgumentException(msg) else get

		/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
		@inline def orError(msg: => String) :A = {
			assert(self.asInstanceOf[AnyRef] ne NonExistent, msg)
			get
		}


		/** Returns the value this `Potential` if it is not empty, or the lazily computed alternative otherwise. */
		@inline def orElse[O >: A](or: => Potential[O]) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) or else self

		/** Similarly to [[net.noresttherein.sugar.vars.PotentialExtension.orElse orElse]], returns this `Potential`
		  *  if it is not empty and `or` otherwise. The difference is that the alternative value is not lazily computed
		  *  and guarantees no closure would be be created, at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def ifEmpty[O >: A](or: Potential[O]) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) or else self

		/** Returns this `Potential` if the condition is false and `Lack` if it is true. This is equivalent
		  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
		@inline def orEmptyIf(condition :Boolean) :Potential[A] =
			if (condition) Inexistent else self

		/** Returns this `Potential` if the condition is true and `Lack` if it is false. This is equivalent
		  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
		@inline def orEmptyUnless(condition :Boolean) :Potential[A] =
			if (condition) self else Inexistent


		/** Returns a new `Potential` which is empty ''iff'' this value is empty, or one containing
		  * the result of applying the given function to its value otherwise. */
		@inline def map[O](f :A => O) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Inexistent else Existent(f(get))

		/** Applies the given function to the content of this `Potential` and returns the result
		  * or the provided alternative if this instance is empty.
		  * Equivalent to `this map f getOrElse or`, but in one step. */
		@inline def mapOrElse[O](f :A => O, or: => O) :O =
			if (self.asInstanceOf[AnyRef] eq NonExistent) or else f(get)

		/** Returns the result of applying `f` to the value of this `Potential` if it is non empty,
		  * or the result of evaluating expression `ifEmpty` otherwise.
		  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
		  * one with another, but its name might be misleading. Consider using
		  * [[net.noresttherein.sugar.vars.PotentialExtension.mapOrElse mapOrElse]] instead.
		  *  @param  ifEmpty the expression to evaluate if empty.
		  *  @param  f       the function to apply if nonempty. */
		@inline def fold[O](ifEmpty: => O)(f: A => O): O =
			if (self.asInstanceOf[AnyRef] eq NonExistent) ifEmpty else f(get)

		/** The same as [[net.noresttherein.sugar.vars.PotentialExtension.map map]], but exceptions thrown
		  * by the function are caught and $Inexistent is returned instead.
		  */
		@inline def guardMap[O](f :A => O) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent)
				Inexistent
			else try {
				Existent(f(get))
			} catch {
				case _ :Exception => Inexistent
			}

		/** Returns the result of applying the given function to the value of this `Potential` if it is not empty,
		  * or `this` if `this.isEmpty`. */
		@inline def flatMap[O](f :A => Potential[O]) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Inexistent else f(get)

		/** Flattens `Potential[Potential[O]]` to a single `Potential[O]`. */
		@inline def flatten[O](implicit isPotential :A <:< Potential[O]) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Inexistent else get

		/** Returns `Inexistent` if `this.contains(o)`, or `this` otherwise. */
		@inline def removed[O >: A](o :O) :Potential[A] =
			if ((self.asInstanceOf[AnyRef] eq NonExistent) || get == o) Inexistent else self

		/** Returns `Inexistent` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
		def removedAll[O >: A](that :IterableOnce[O]) :Potential[A] = that match {
			case _ if self.asInstanceOf[AnyRef] eq NonExistent => self
			case it :Set[O]     => if (it(get)) Inexistent else self
			case it :Ranking[O] => if (it.contains(get)) Inexistent else self
			case it :Iterable[O] if it.isEmpty => self
			case _ =>
				val i = that.iterator
				val x = get
				while (i.hasNext)
					if (i.next() == x)
						return Inexistent
				self
		}

		/** Returns a new `Potential` containing this value if it is not empty and its value satisfies
		  * the given predicate, or [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] otherwise. */
		@inline def filter(p :A => Boolean) :Potential[A] =
			if ((self.asInstanceOf[AnyRef] eq NonExistent) || p(get)) self else Inexistent

		/** Returns a new `Potential` containing this value if it is not empty and its value falsifies
		  * the given predicate, or [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] otherwise. */
		@inline def filterNot(p :A => Boolean) :Potential[A] =
			if ((self.asInstanceOf[AnyRef] eq NonExistent) || !p(get)) self else Inexistent

		/** Equivalent to `this.`[[net.noresttherein.sugar.vars.PotentialExtension.filter filter]]`(p)` -
		  * a variant for use in for-comprehensions. Note that as this implementation is performance oriented,
		  * it evaluates the predicate immediately, unlikely standard methods of [[scala.collection.Iterable Iterable]]. */
//		@inline def withFilter(p :A => Boolean) :WithFilter[A] = new WithFilter[A](self, p)
		@inline def withFilter(p :A => Boolean) :Potential[A] =
			if ((self.asInstanceOf[AnyRef] eq NonExistent) || p(get)) self else Inexistent

		/** Tests if this `Potential` is not empty and its value is equal to the given argument. */
		@inline def contains[O >: A](o :O): Boolean = (self.asInstanceOf[AnyRef] ne NonExistent) && get == o

		/** Tests if this `Potential` is not empty and its value satisfies the given predicate. */
		@inline def exists(p :A => Boolean): Boolean = (self.asInstanceOf[AnyRef] ne NonExistent) && p(get)

		/** Tests if this `Potential` contains no value or its value satisfies the given predicate. */
		@inline def forall(p :A => Boolean): Boolean = (self.asInstanceOf[AnyRef] eq NonExistent) || p(get)

		/** Executes the given block for this `Potential`'s value if it is not empty. */
		@inline def foreach[O](f :A => O) :Unit = if (self.asInstanceOf[AnyRef] ne NonExistent) f(get)

		/** Returns `Inexistent` if this `Potential` is empty or the partial function `f` is not defined for its value,
		  * otherwise applies it and wraps the result it in a new `Opt`. */
		@inline def collect[O](f :PartialFunction[A, O]) :Potential[O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent)
				Inexistent
			else
				f.applyOrElse(get, Opt.NoContent.asInstanceOf[A => O]) match {
					case Opt.NoContent => Inexistent
					case result => Existent(result)
				}


		/** Returns a `Potential` formed from the contents of `this` and `that` by combining the corresponding elements
		  *  in a pair. If either of the two options is empty, `Inexistent` is returned. */
		@inline def zip[O](that :Potential[O]) :Potential[(A, O)] =
			if ((self.asInstanceOf[AnyRef] eq NonExistent) | (that.asInstanceOf[AnyRef] eq NonExistent))
				Inexistent
			else Existent((get, that.get))

		/** Converts an `Potential` of a pair into `Potential`s of its first and second elements. */
		@inline def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (Potential[A1], Potential[A2]) =
			Potential.unzip(asPair.liftCo[Potential](self))

		/** Converts an `Potential` of a triple into three `Potential`s, one containing the element
		  * from each position of the triple. */
		@inline def unzip3[A1, A2, A3](implicit asTriple: A <:< (A1, A2, A3)): (Potential[A1], Potential[A2], Potential[A3]) =
			Potential.unzip3(asTriple.liftCo[Potential](self))


		/** An iterator returning this value as the only element if `this.nonEmpty`. */
		@inline def iterator :Iterator[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Iterator.empty else Iterator.single(get)

		/** Returns `Nil` if this `Potential` is empty or or `this.get::Nil` otherwise. */
		@inline def toList :List[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) Nil else get::Nil

		/** Returns an empty list if this `Potential` is empty or a single element list with its value otherwise. */
		@inline def toSeq :Seq[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) Nil else get::Nil

		/** Returns an empty collection if this `Potential` is empty or a singleton with its value otherwise. */
		@inline def toIterable :Iterable[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Iterable.empty else Iterable.single(get)

		/** Same as [[net.noresttherein.sugar.vars.PotentialExtension.toOption toOption]]. */
		@inline def option :Option[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) None else Some(get)

		/** Same as [[net.noresttherein.sugar.vars.PotentialExtension.toOpt toOpt]]. */
		@inline def opt :Opt[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) Lack else Got(get)

		/** Same as [[net.noresttherein.sugar.vars.PotentialExtension.toUnsure toUnsure]]. */
		@inline def unsure :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent)
				Missing else {
				val a = get
				new Sure(a, cachedOpt = Got(a))
			}

		/** Conversion to standard Scala [[scala.Option]].
		  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
		@inline def toOption :Option[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) None else Some(get)

		/** Conversion to a value class `Opt`. The difference is that `Opt` will be reified to a runtime object
		  * wrapping its value when used as a value of its supertype or an abstract type (for example, a type parameter).
		  * The benefit is that, unlike `Potential`, it loses no information and can be pattern matched,
		  * but at the cost of boxing. There is an implicit conversion to the same effect.
		  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
		@inline def toOpt :Opt[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) Lack else Got(get)

		/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
		  * is specialized for value types, this type is not, and the result will not be specialized. Neither will it
		  * require boxing though, as any value type was promoted to a reference wrapper when creating this `Potential`.
		  */
		@inline def toUnsure :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent)
				Missing
			else {
				val a = get
				new Sure(a, cachedOpt = Got(a))
			}

		/** Same as [[net.noresttherein.sugar.vars.PotentialExtension.toOption toOption]]
		  * (for [[net.noresttherein.sugar.vars.Ref Ref]] interoperability).
		  */
		@inline def constOption :Option[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) None else Some(get)

		/** Same as [[net.noresttherein.sugar.vars.PotentialExtension.toOpt toOpt]]
		  * (for [[net.noresttherein.sugar.vars.Ref Ref]] interoperability).
		  */
		@inline def constOpt :Opt[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) Lack else Got(get)

		/** Same as [[net.noresttherein.sugar.vars.PotentialExtension.toUnsure toUnsure]]
		  * (for [[net.noresttherein.sugar.vars.Ref Ref]] interoperability).
		  */
		@inline def constUnsure :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent)
				Missing else {
				val a = get
				new Sure(a, cachedOpt = Got(a))
			}


		/** Converts this `Potential` to `Either`, returning the content as `Left`,
		  *  or the value of the given expression as `Right` if empty. */
		@inline def toLeft[O](right: => O) :Either[A, O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Right(right) else Left(get)

		/** Converts this `Potential` to `Either`, returning the content as `Right`,
		  *  or the value of the given expression as `Left` if empty. */
		@inline def toRight[O](left: => O) :Either[O, A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Left(left) else Right(get)

		/** Converts this `Potential` to $Pill, returning the content as $Red,
		  *  or the value of the given expression as $Blue if empty. */
		@inline def toRed[O](blue: => O) :Pill[A, O] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Blue(blue) else Red(get)

		/** Converts this `Potential` to $Pill, returning the content as $Blue,
		  *  or the value of the given expression as $Red if empty. */
		@inline def toBlue[O](red: => O) :Pill[O, A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Red(red) else Blue(get)

		/** Converts this `Potential` to $Outcome, returning the content as $Done,
		  *  or the given `String` as $Failed error message if empty. */
		@inline def outcome(err: => String) :Outcome[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Failed(() => err) else Done(get)

		/** Converts this `Potential` to $Outcome, returning the content as $Done,
		  *  or the given `Throwable` as $Failed error if empty. */
		@inline def outcome(err :Throwable) :Outcome[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent) Failed(err) else Done(get)


		/** Formats this `Potential` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
		@inline def mkString(prefix :String) :String =
			if (self.asInstanceOf[AnyRef] eq NonExistent) prefix + "()" else prefix + "(" + self + ")"

		/** Formats this `Potential` as `s"Potential($get)"` or `"Potential()"`. */
		@inline def mkString :String =
			if (self.asInstanceOf[AnyRef] eq NonExistent) "Potential()" else "Potential(" + self + ")"

		/** Compares the contents for equality, with the result being false if any of the operands are empty. */
		@inline def same(other :Potential[_]) :Boolean =
			(self.asInstanceOf[AnyRef] ne NonExistent) & (other.asInstanceOf[AnyRef] ne NonExistent) && self == other

		/** Returns `for (a <- this; b <- other) yield a == b`. */
		@inline def sameOpt(other :Potential[_]) :Potential[Boolean] =
			(if (self.asInstanceOf[AnyRef] eq NonExistent) self
			 else if (other.asInstanceOf[AnyRef] eq NonExistent) other
			 else (self == other)
			).asInstanceOf[Potential[Boolean]]
	}




	/** An alternative wrapper over one of two values: either `Pill.`${Red}`[Red]`, or `Pill`.${Blue}[Blue],
	  * with the latter type not visible to the application.
	  * It is a variant of `Either` optimized for usage in which one of the cases (corresponding to `Right` in `Either`)
	  * is much more common than the other (i.e., '`Left`'), such as in popular monadic usage of `Right`
	  * ($Blue in this case) for a result of a computation, while reserving `Left` ($Red) for error information
	  * passed unchanged when [[net.noresttherein.sugar.vars.PillExtension.map mapping]]
	  * or [[net.noresttherein.sugar.vars.PillExtension.flatMap flat mapping]] a `Pill`.
	  *
	  * In most situations, the `Blue[B]` case is represented as an erased value of `B`, meaning that
	  * monadic usage of `Pill` doesn't involve creating a new object with each `map`/`flatMap` operation.
	  * This is changed only if `Pill` is nested, i.e. a `Blue(Blue(_))` or `Blue(Red(_))` instance is
	  * created. Values of `Red` are represented normally, by wrapping them in an instance of
	  * [[net.noresttherein.sugar.vars.Pill.Red! Red]], but in the intended usage pattern, are passed unchanged
	  * by monadic operations, also avoiding creation of a new instance.
	  *
	  * Due to this design, the `Blue[_]` and `Red[_]` cases cannot be matched by type, but only by the
	  * [[net.noresttherein.sugar.vars.Pill.Blue.unapply Blue.unapply]] and
	  * [[net.noresttherein.sugar.vars.Pill.Red.unapply Red.unapply]] match patterns.
	  *
	  * The interface of this type is provided as extension methods by
	  * [[net.noresttherein.sugar.vars.PillExtension PillExtension]].
	  * @see [[net.noresttherein.sugar.vars.Outcome Outcome]]
	  */ //the fact that R stands both for Red and Right, but Red is used as Left is a bit confusing
	type Pill[+Red, +Blue] >: Pill.Red[Red]

	/** Extension methods providing the interface of $Pill.
	  * @tparam R the type carried by the 'red' case, corresponding to the `Left` side of an `Either`.
	  * @tparam B the type carried by the 'blue' case, corresponding to the `Right` side of an `Either`.
	  */
	implicit class PillExtension[+R, +B](private val self :Pill[R, B]) extends AnyVal {
		/** Checks if this $Pill is the $Blue (successful) case containing the result `B` of an operation. */
		@inline def isBlue :Boolean = !self.isInstanceOf[Red[_]]
		/** Checks if this $Pill is the $Red (unsuccessful) case containing some information `R` about the failure. */
		@inline def isRed :Boolean = self.isInstanceOf[Red[_]]

		/** Swaps the meaning of $Red and $Blue: a `Red(r)` becomes `Blue(r)`, while `Blue(b)` becomes `Red(b)`. */
		@inline def swap :Pill[B, R] = self match {
			case red :Red[R @unchecked] => Blue(red.value)
			case _                      => Red(get)
		}
		/** Forces extraction of the $Blue result.
		  * @return contained value, if `this` is $Blue.
		  * @throws NoSuchElementException if this $Pill is $Red. */
		def get :B = (self :Any) match {
			case red  :Red[_]             => throw new NoSuchElementException(red.toString)
			case blue :Blue[B @unchecked] => blue.value
			case _                        => self.asInstanceOf[B]
		}
		/** Returns the result if it is $Blue, or the lazily computed alternative passed as an argument otherwise. */
		@inline def getOrElse[B1 >: B](or: => B1) :B1 = self match {
			case _ :Red[_] => or
			case _         => get
		}
		/** Similarly to [[net.noresttherein.sugar.vars.PillExtension.getOrElse getOrElse]], returns the result
		  * of this $Pill if it is $Blue, or `alt` if it is $Red. The difference is that the alternative value
		  * is not lazily computed and guarantees no closure will be created,
		  * at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is a failure. */
		@inline def orDefault[B1 >: B](or :B1) :B1 = self match {
			case _ :Red[_] => or
			case _         => get
		}
		/** Assuming that `A` is a nullable type, return `null` if this $Pill is $Red,
		  * or a wrapped result of $Blue otherwise. */
		@inline def orNull[B1 >: B](implicit isNullable :B1 <:< Null) :B1 = self match {
			case _ :Red[_] => null.asInstanceOf[B1]
			case _         => get
		}

		/** Returns the result if it is $Blue, or throws an exception given as the type parameter with
		  * `(this :`$Red`[R]).value.toString` as the error message.
		  * Note that this method uses reflection to find and call the exception constructor
		  * and will not be as efficient as
		  * {{{
		  *     this match {
		  *         case Blue(blue) => blue
		  *         case Red(red) => throw new E(red.toString)
		  *     }
		  * }}}
		  * @tparam E an exception class which must provide either a publicly available constructor accepting
		  *           a single `String` argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orNoSuch orNoSuch]]
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orIllegal orIllegal]] */
		@inline def orThrow[E <: Throwable :ClassTag] :B = self match {
			case red :Red[_] => raise[E](red.value.toString)
			case _           => get
		}
		/** Gets the element in this $Pill if it is $Blue, or throws a [[NoSuchElementException]]
		  * with [[net.noresttherein.sugar.vars.Pill.Red.value value]]`.toString` as the error message if $Red.
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orThrow orThrow]] */
		@inline def orNoSuch :B = self match {
			case red :Red[_] => throw new NoSuchElementException(red.value.toString)
			case _           => get
		}
		/** Gets the element in this $Pill if it is $Blue, or throws an [[IllegalArgumentException]]
		  * with [[net.noresttherein.sugar.vars.Pill.Red.value value]]`.toString` as the error message if $Red.
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orThrow orThrow]] */
		@inline def orIllegal :B = self match {
			case red :Red[_] => throw new IllegalArgumentException(red.value.toString)
			case _           => get
		}
		/** Asserts that this instance is $Blue and returns its contents, throwing an [[AssertionError]]
		  * with `this.toString` as the error message if $Red. */
		@inline def orError :B = {
			assert(!self.isInstanceOf[Red[_]], self.toString)
			get
		}

		/** Returns this $Pill if it is $Blue, or the lazily computed alternative otherwise. */
		@inline def orElse[R1 >: R, B1 >: B](or: => Pill[R1, B1]) :Pill[R1, B1] = self match {
			case _ :Red[_] => or
			case _         => self
		}

		/** Similarly to [[net.noresttherein.sugar.vars.PillExtension.orElse orElse]], returns this $Pill
		  * if it is $Blue, or `alt` otherwise. The difference is that the alternative value is not lazily computed
		  * and guarantees no closure would be be created, at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def ifRed[R1 >: R, B1 >: B](or :Pill[R1, B1]) :Pill[R1, B1] = self match {
			case _ :Red[_] => or
			case _         => self
		}



		/** Returns $Blue with the result of applying the given function to the result of this $Pill,
		  * or this instance ''iff'' it is $Red. */
		@inline def map[O](f :B => O) :Pill[R, O] = self match {
			case red :Red[R @unchecked] => red
			case _                      => Blue(f(get))
		}
		/** Applies the given function to the value of this $Pill if it is $Blue, or returns `alt`
		  * if it is $Red. Equivalent to `this map f getOrElse alternative`, but in one step. */
		@inline def mapOrElse[O](f :B => O, or: => O) :O = self match {
			case _ :Red[_] => or
			case _         => f(get)
		}
		/** Applies the first function argument to this `Pill`'s value if it is $Red,
		  * or the second function if it is $Blue. */
		@inline def fold[O](ifRed :R => O, ifBlue :B => O) :O = self match {
			case red :Red[R @unchecked] => ifRed(red.value)
			case _                      => ifBlue(get)
		}
		/** Returns the result of applying the given function to the value of this $Pill if it is $Blue,
		  * or `this` if it is $Red. */
		@inline def flatMap[R1 >: R, O](f :B => Pill[R1, O]) :Pill[R1, O] = self match {
			case red :Red[R @unchecked] => red
			case _                      => f(get)
		}
		/** Flattens `Pill[R, Pill[R, O]]` to a single `Pill[R, O]`. */
		@inline def flatten[R1 >: R, O](implicit isAlt :B <:< Pill[R1, O]) :Pill[R1, O] =
			self match {
				case red :Red[R @unchecked] => red
				case _                      => get
			}
		/** Flattens `Pill[Pill[O, B], B]]` to `Pill[O, B]`: returns the value of this $Pill if it is $Red,
		  * or itself if it is $Blue. This is similar to [[net.noresttherein.sugar.vars.PillExtension.flatten flatten]],
		  * but works on the `Red` (`Left`) side.
		  */
		@inline def joinRed[R1 >: R, B1 >: B, O](implicit redIsAlt :R1 <:< Pill[O, B1]) :Pill[O, B1] =
			self match {
				case red :Red[R @unchecked] => red.value
				case _                      => self.asInstanceOf[Pill[O, B1]]
			}
		/** Flattens `Pill[R, Pill[R, O]]` to `Pill[R, O]`: returns the value of this $Pill if it is $Red,
		  * or itself if it is $Blue. This is equivalent to [[net.noresttherein.sugar.vars.PillExtension.flatten flatten]],
		  * but allows to widen the type of the red pill.
		  */
		@inline def joinBlue[R1 >: R, B1 >: B, O](implicit blueIsAlt :B1 <:< Pill[R1, O]) :Pill[R1, O] =
			self match {
				case red :Red[R @unchecked] => red
				case _                      => get
			}
		/** Same as [[net.noresttherein.sugar.vars.PillExtension.joinRed joinRed]]. Exists for compatibility with `Either`. */
		@inline def joinLeft[R1 >: R, B1 >: B, O](implicit redIsAlt :R1 <:< Pill[O, B1]) :Pill[O, B1] = joinRed
		/** Same as [[net.noresttherein.sugar.vars.PillExtension.joinBlue joinBlue]]. Exists for compatibility with `Either`. */
		@inline def joinRight[R1 >: R, B1 >: B, O](implicit blueIsAlt :B1 <:< Pill[R1, O]) :Pill[R1, O] = joinBlue

		/** Returns `this` if $Blue and `p(get)` holds, or ${Red}(red) otherwise. */
		def filterOrElse[R1 >: R](p: B => Boolean, red: => R1): Pill[R1, B] = self match {
			case _ :Red[_]   => Red(red)
			case _ if p(get) => self
			case _           => Red(red)
		}

		/** Tests if this $Pill is $Blue with a result equal to the given argument. */
		@inline def contains[B1 >: B](o :B1): Boolean = o match {
			case _ :Red[_] => false
			case _         => get == o
		}
		/** Tests if this $Pill is $Blue with a result satisfying the given predicate. */
		@inline def exists(p :B => Boolean): Boolean = self match {
			case _ :Red[_] => false
			case _         => p(get)
		}
		/** Tests if this $Pill $Red or $Blue with a value not satisfying the given predicate. */
		@inline def forall(p :B => Boolean): Boolean = self match {
			case _ :Red[_] => true
			case _         => p(get)
		}
		/** Executes the given block for this $Pill's value if it is $Blue. */
		@inline def foreach[O](f :B => O) :Unit =
			if (!self.isInstanceOf[Red[_]])
				f(get)

		/** Converts this value to an `Opt` if it is $Blue, losing the information by replacing
		  * $Red with [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
		  */
		@inline def toOpt :Opt[B] = self match {
			case _ :Red[_] => Lack
			case _         => Got(get)
		}
		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isBlue` or `None` otherwise. */
		@inline def toOption :Option[B] = self match {
			case _ :Red[_] => None
			case _         => Some(get)
		}
		/** Converts this value to a `Potential` if it is $Blue, losing the information by replacing
		  * $Red with [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]].
		  */
		@inline def toPotential :Potential[B] = self match {
			case _ :Red[_] => Inexistent
			case _         => Existent(get)
		}
		/** Conversion to an `Unsure` carrying the value of this instance if it is $Blue.
		  * Note that the result will not be `specialized` for value types, but neither will it require boxing,
		  * as $Blue already contains boxed values.
		  */
		@inline def toUnsure :Unsure[B] = self match {
			case _ :Red[_] => Missing
			case _         => Sure(get)
		}
		/** Conversion to [[scala.Either]], with $Red returned as [[scala.Right Right]] and $Blue as [[scala.Left Left]].
		  */
		@inline def toEither :Either[R, B] = self match {
			case red :Red[R @unchecked] => Left(red.value)
			case _                      => Right(get)
		}
		/** Returns a [[Seq]] containing `this.get` (if $Blue), or an empty `Seq` otherwise. */
		@inline def toSeq :Seq[B] = self match {
			case _ :Red[_] => Nil
			case _         => get::Nil
		}
		/** Turns a $Red into a [[Failure]] and a $Blue into a [[Success]]. */
		@inline def toTry(implicit ev :R <:< Throwable) :Try[B] = self match {
			case red :Red[R @unchecked] => Failure(red.value)
			case _                      => Success(get)
		}

		/** Returns `true` if both operands are $Blue and their values are equal. */
		@inline def blueEquals(other :Pill[_, _]) :Boolean =
			!self.isInstanceOf[Red[_]] & !other.isInstanceOf[Red[_]]  && self == other

		/** Returns `true` if both operands are $Red and their values are equal. */
		@inline def redEquals(other :Pill[_, _]) :Boolean = (self, other) match {
			case (a :Red[_], b :Red[_]) => a.value == b.value
			case _                      => false
		}

		/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]] if any of the operands are $Red,
		  * and `this == other` otherwise.
		  */
		@inline def blueEqualsOpt(other :Pill[_, _]) :Opt[Boolean] =
			if (self.isInstanceOf[Red[_]] | other.isInstanceOf[Red[_]]) Lack
			else Got(self == other)

		/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]] if any of the operands are $Blue,
		  * and `this == other` otherwise.
		  */
		@inline def redEqualsOpt(other :Pill[_, _]) :Opt[Boolean] = (self, other) match {
			case (a :Red[_], b :Red[_]) => Got(a.value == b.value)
			case _                      => Lack
		}
	}



	/** A variant of a non boxing `Either`, with instances of two categories: $Done`[A]` containing a value of `A`,
	  * and $Failed with an exception, or an error message. In order to avoid the creation of `Right` (successful)
	  * instance each time in a monadic composition of chained operations on an `Either`, a `Done` is encoded
	  * as its erased (and boxed) contents, i.e. the value itself. A `Failed` is encoded as an instance of `Throwable`:
	  * this allows both to pass arbitrary `Throwable` as errors without additional boxing,
	  * as well as wrap a lazily evaluated `String` in a special `Failed` instance.
	  * This solution brings three limitations:
	  *   1. Nesting `Outcome` within another `Outcome` must resort to boxing in order to differentiate between
	  *      `Done(Failed)` and `Failed`. This is however transparent to the application.
	  *   1. Returning a `Done[Throwable]` requires boxing the exception to differentiate the case from `Failed`.
	  *   1. No `Done` (i.e. `Right`) type exist, because it is erased,
	  *      and construction and checking must happen using `apply` and `unapply` methods of singleton
	  *      object $Done. [[net.noresttherein.sugar.vars.OutcomeExtension Extension]] methods are provided,
	  *      mirroring the relevant part of functionality of `Either` and `Option`.
	  */
	//consider: Renaming to Yield. The advantage lies in that methods could be named yieldXxx (and in a shorter name),
	// but `Done` and `Failed` are not good name matches for its subtypes.
	type Outcome[+A] >: Failed //can be also named Fail

	/** Extension methods providing the full interface of $Outcome. */
	implicit class OutcomeExtension[A](private val self :Outcome[A]) extends AnyVal {
		/** Checks if this $Outcome is $Done (successful) result containing a value. */
		@inline def isPassed :Boolean = !self.isInstanceOf[Throwable @unchecked]
		/** Checks if this $Outcome is $Failed containing an error message. */
		@inline def isFailed :Boolean = self.isInstanceOf[Throwable @unchecked]

		/** Extracts the value if this instance is `Done`, or throws the exception carried by `Failed` otherwise. */
		def apply() :A = (self :Any) match {
			case pass :Done[A @unchecked] => pass.value
			case fail :Throwable          => throw fail
			case _                        => self.asInstanceOf[A]
		}

		/** Forces extraction of the result.
		  * @return contained value, if `this` is $Done.
		  * @throws NoSuchElementException if this $Outcome is $Failed. */
		def get :A = (self :Any) match {
			case pass :Done[A @unchecked] => pass.value
			case fail :Throwable          => throw new NoSuchElementException(fail)
			case _                        => self.asInstanceOf[A]
		}

		/** Returns the result if it is $Done, or the lazily computed alternative passed as an argument otherwise. */
		@inline def getOrElse[O >: A](or : => O) :O = self match {
			case _ :Throwable => or
			case _            => get
		}

		/** Similarly to [[net.noresttherein.sugar.vars.OutcomeExtension.getOrElse getOrElse]], returns the result
		  * of this $Pill if it is $Done, or `alt` if it is $Failed. The difference is that the alternative value
		  * is not lazily computed and guarantees no closure will be created,
		  * at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is a failure. */
		@inline def orDefault[O >: A](or :O) :O = self match {
			case _ :Throwable => or
			case _            => get
		}

		/** Assuming that `A` is a nullable type, return `null` if this $Outcome is $Failed,
		  * or a wrapped result of $Done otherwise. */
		@inline def orNull[O >: A](implicit isNullable :O <:< Null) :O = self match {
			case _ :Throwable => null.asInstanceOf[O]
			case _            => get
		}

		/** Returns the result if it is $Done, or throws an exception given as the type parameter
		  * with the exception carried by [[net.noresttherein.sugar.vars.Outcome.Failed Failed]] as its cause otherwise.
		  * Note that this method uses reflection to find and call the exception constructor
		  * and will not be as efficient as
		  * {{{
		  *     this match {
		  *         case Done(value) => value
		  *         case Failed(err) => throw new E(err)
		  *     }
		  * }}}
		  * @tparam E an exception class which must provide either a publicly available constructor accepting
		  *           a single `String` argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  * @see [[net.noresttherein.sugar.vars.OutcomeExtension.orNoSuch orNoSuch]]
		  * @see [[net.noresttherein.sugar.vars.OutcomeExtension.orIllegal orIllegal]] */
		@inline def orThrow[E <: Throwable :ClassTag] :A = self match {
			case fail :Failed    => raise[E](fail.msg)
			case fail :Throwable => raise[E](fail)
			case _               => get
		}

		/** Gets the element in this $Outcome if it is $Done, or throws a [[NoSuchElementException]]
		  * with the exception carried by [[net.noresttherein.sugar.vars.Outcome.Failed Failed]] as its cause otherwise.
		  * @see [[net.noresttherein.sugar.vars.OutcomeExtension.orThrow orThrow]] */
		@inline def orNoSuch :A = self match {
			case fail :Failed    => throw new NoSuchElementException(fail.msg)
			case fail :Throwable => throw new NoSuchElementException(fail)
			case _ => get
		}

		/** Gets the element in this $Outcome if it is $Done, or throws an [[IllegalArgumentException]]
		  * with the exception carried by [[net.noresttherein.sugar.vars.Outcome.Failed Failed]] as its cause otherwise.
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orThrow orThrow]] */
		@inline def orIllegal :A = self match {
			case fail :Failed    => throw new IllegalArgumentException(fail.msg)
			case fail :Throwable => throw new IllegalArgumentException(fail)
			case _ => get
		}

		/** Asserts that this instance is $Done and returns its contents, throwing an [[AssertionError]]
		  * with `this.toString` as the error message if $Failed. */
		@inline def orError :A = {
			assert(!self.isInstanceOf[Throwable], self.toString)
			get
		}

		/** Returns this $Outcome if it is $Done, or the lazily computed alternative otherwise. */
		@inline def orElse[O >: A](or : => Outcome[O]) :Outcome[O] = self match {
			case _ :Throwable => or
			case _            => self
		}

		/** Similarly to [[net.noresttherein.sugar.vars.OutcomeExtension.orElse orElse]], returns this $Outcome
		  * if it is $Done, or `alt` otherwise. The difference is that the alternative value is not lazily computed
		  * and guarantees no closure would be be created, at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def ifFailed[O >: A](or :Outcome[O]) :Outcome[O] = self match {
			case _ :Throwable => or
			case _            => self
		}


		/** Returns $Done with the result of applying the given function to the result of this $Outcome,
		  * or this instance ''iff'' it is $Failed. */
		@inline def map[O](f :A => O) :Outcome[O] = self match {
			case fail :Throwable => fail.asInstanceOf[Outcome[O]]
			case _               => Done(f(get))
		}

		/** Applies the given function to the value of this $Outcome if it is $Done, or returns `alt`
		  * if it is $Failed. Equivalent to `this map f getOrElse alternative`, but in one step. */
		@inline def mapOrElse[O](f :A => O, or : => O) :O = self match {
			case _ :Throwable => or
			case _            => f(get)
		}

		/** Applies the first function argument to this `Outcome`'s value if it is $Failed,
		  * or the second function if it is $Done. */
		@inline def fold[O](ifFailed :Throwable => O, ifPassed :A => O) :O = self match {
			case fail :Throwable => ifFailed(fail)
			case _               => ifPassed(get)
		}

		/** The same as [[net.noresttherein.sugar.vars.OutcomeExtension.map map]], but exceptions thrown
		  * by the function are caught and $Failed with the exception's error message is returned.
		  */
		@inline def guardMap[O](f :A => O) :Outcome[O] = self match {
			case fail :Throwable =>
				fail.asInstanceOf[Outcome[O]]
			case _ =>
				try Done(f(get)) catch {
					case e :Exception => Failed(e)
				}
		}

		/** Returns the result of applying the given function to the value of this $Outcome if it is $Done,
		  * or `this` if it is $Failed. */
		@inline def flatMap[O](f :A => Outcome[O]) :Outcome[O] = self match {
			case fail :Throwable => fail.asInstanceOf[Outcome[O]]
			case _               => f(get)
		}

		/** Flattens `Outcome[Outcome[O]]` to a single `Outcome[O]`. */
		@inline def flatten[O](implicit isAlt :A <:< Outcome[O]) :Outcome[O] = self match {
			case fail :Throwable => fail.asInstanceOf[Outcome[O]]
			case _               => get
		}

		/** Returns `this` if it is $Done and `p(get)` holds, or ${Failed}(error) otherwise. */
		def filterOrElse(p :A => Boolean, error : => Throwable) :Outcome[A] =
			if (self.isInstanceOf[Throwable] || !p(get)) Failed(error) else self

		/** Returns `this` if it is $Done and `p(get)` holds, or ${Failed}(error) otherwise. */
		def filterOrFailed(p :A => Boolean, error : => String) :Outcome[A] =
			if (self.isInstanceOf[Throwable] || !p(get)) Failed(() => error) else self

		/** Tests if this $Outcome is $Done with a result equal to the given argument. */
		@inline def contains[O >: A](o :O) :Boolean = o match {
			case _ :Throwable => false
			case _            => get == o
		}

		/** Tests if this $Outcome is $Done with a result satisfying the given predicate. */
		@inline def exists(p :A => Boolean) :Boolean = self match {
			case _ :Throwable => false
			case _            => p(get)
		}

		/** Tests if this $Outcome is $Failed, or $Done with a value not satisfying the given predicate. */
		@inline def forall(p :A => Boolean) :Boolean = self match {
			case _ :Throwable => true
			case _            => p(get)
		}

		/** Executes the given block for this $Pill's value if it is $Blue. */
		@inline def foreach[O](f :A => O) :Unit =
			if (!self.isInstanceOf[Throwable])
				f(get)

		/** Converts this value to an `Opt` if it is $Done, losing the information by replacing
		  * $Failed with [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
		  */
		@inline def toOpt :Opt[A] = self match {
			case _ :Throwable => Lack
			case _            => Got(get)
		}

		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isPassed` or `None` otherwise. */
		@inline def toOption :Option[A] = self match {
			case _ :Throwable => None
			case _            => Some(get)
		}

		/** Converts this value to a `Potential` if it is $Done, losing the information by replacing
		  * $Failed with [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]].
		  */
		@inline def toPotential :Potential[A] = self match {
			case _ :Throwable => Inexistent
			case _            => Existent(get)
		}

		/** Conversion to an `Unsure` carrying the value of this instance if it is $Done.
		  * Note that the result will not be `specialized` for value types, but neither will it require additional boxing,
		  * as $Done already contains boxed values.
		  */
		@inline def toUnsure :Unsure[A] = self match {
			case _ :Throwable => Missing
			case _            => Sure(get)
		}

		/** Conversion to [[scala.Either]], with $Failed returned as [[scala.Right Right]] and $Done as [[scala.Left Left]]. */
		@inline def toEither :Either[Throwable, A] = self match {
			case fail :Throwable => Left(fail)
			case _               => Right(get)
		}

		/** Returns a [[Seq]] containing `this.get` (if $Done), or an empty `Seq` otherwise. */
		@inline def toSeq :Seq[A] = self match {
			case _ :Throwable => Nil
			case _            => get :: Nil
		}

		/** Turns a $Failed to a [[Failure]] and a $Done to a [[Success]]. */
		@inline def toTry :Try[A] = self match {
			case fail :Throwable => Failure(fail)
			case _               => Success(get)
		}

		/** Compares the contents for equality, with the result being false if any of the operands are $Failed. */
		@inline def same(other :Outcome[_]) :Boolean =
			!self.isInstanceOf[Throwable] & !other.isInstanceOf[Throwable] && self == other

		/** Returns `Got(this == other)` if both operands are $Done, or `Lack` otherwise. */
		@inline def sameOpt(other :Outcome[_]) :Opt[Boolean] =
			if (self.isInstanceOf[Throwable] | other.isInstanceOf[Throwable]) Lack
			else Got(self == other)
	}
}






package vars {

	import net.noresttherein.sugar.exceptions.SugaredException

	private[sugar] sealed abstract class Rank1PotentialImplicits {
		@inline implicit def optFromPotential[T](opt :Potential[T]) :Opt[T] = Existent.unapply(opt)
		@inline implicit def potentialFromOpt[T](opt :Opt[T]) :Potential[T] = Potential.got_?(opt)
	}



	/** A companion and factory of $Potential, a very lightweight alternative to [[Option]].
	  * @see [[net.noresttherein.sugar.vars.Potential.Existent]]
	  * @see [[net.noresttherein.sugar.vars.Potential.Inexistent]]
	  *
	  * @define Potential  [[net.noresttherein.sugar.vars.Potential! Potential]]
	  * @define Existent   [[net.noresttherein.sugar.vars.Existent$ Existent]]
	  * @define Inexistent [[net.noresttherein.sugar.vars.Inexistent$ Inexistent]]
	  */
	@SerialVersionUID(Ver)
	object Potential { //Synonyms: Yield; Maybe/Yes/No; Hope/Lucky/NoLuck; Wish
		/** Creates an $Existent instance wrapping the value unless it is null, in which case it returns $Inexistent.
		  * This call will not box the value unless it is already an instance of `Potential`.
		  */
		def apply[A](value :A) :Potential[A] = value match {
			case null                        => Inexistent
			case Inexistent | _ :Existent[_] => new Existent(value).asInstanceOf[Potential[A]]
			case _                           => value.asInstanceOf[Potential[A]]
		}

		/** Converts the given `Option[T]` into a lighter `Potential[T]` which is erased at runtime. */
		@inline def some_?[A](value :Option[A]) :Potential[A] = value match {
			case Some(a) => Existent(a)
			case _       => Inexistent
		}

		/** Converts the given `Opt[T]` into a `Potential[T]`. */
		@inline def got_?[A](value :Opt[A]) :Potential[A] = value match {
			case Got(a) => Existent(a)
			case _      => Inexistent
		}

		/** Converts the given `Unsure[T]` into a `Potential[T]`, erased at runtime. */
		@inline def sure_?[A](value :Unsure[A]) :Potential[A] = value match {
			case Sure(a) => Existent(a)
			case _       => Inexistent
		}

		/** Converts the given `Option[T]` into a lighter `Potential[T]` which is erased at runtime. */
		@inline def fromOption[A](value :Option[A]) :Potential[A] = value match {
			case Some(a) => Existent(a)
			case _       => Inexistent
		}

		/** Converts the given `Opt[T]` into a `Potential[T]`. */
		@inline def fromOpt[A](value :Opt[A]) :Potential[A] = value match {
			case Got(a) => Existent(a)
			case _      => Inexistent
		}

		/** Converts the given `Unsure[T]` into a `Potential[T]`, erased at runtime. */
		@inline def fromUnsure[A](value :Unsure[A]) :Potential[A] = value match {
			case Sure(a) => Existent(a)
			case _       => Inexistent
		}

		/** Returns [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]] - an empty `Potential`. */
		@inline final def empty[T] :Potential[T] = Inexistent

		/** When a given condition is true, evaluates the `a` argument and returns `Existent(a).`
		  * When the condition is false, `a` is not evaluated and `Inexistent` is returned.
		  */
		@inline def when[A](cond: Boolean)(a: => A): Potential[A] =
			if (cond) Existent(a) else Inexistent

		/** Unless a given condition is true, this will evaluate the `a` argument and return `Existent(a)`.
		  * Otherwise, `a` is not evaluated and `Inexistent` is returned. */
		@inline def unless[A](cond: Boolean)(a: => A): Potential[A] =
			if (!cond) Existent(a) else Inexistent

		/** Executes the given lazy expression in a `try-catch` block, returning `Inexistent` in case
		  * any exception is caught. Otherwise the value is returned in an `Existent` instance as normal. */
		@inline def guard[A](a: => A) :Potential[A] =
			try Existent(a) catch {
				case _ :Exception => Inexistent
			}

		/** Applies the given function to the second argument in a `try-catch` block, returning `Inexistent` in case
		  * any exception is caught. Otherwise the value is returned in an `Existent` instance as normal. */
		@inline def guard[A, B](f: A => B)(a :A) :Potential[B] =
			try Existent(f(a)) catch {
				case _ :Exception => Inexistent
			}

		/** Returns the first argument in `Existent` if it satisfies the predicate `p`.
		  * @return `Existent(value).filter(p)`.
		  */
		@inline def satisfying[A](value :A)(p :A => Boolean) :Potential[A] =
			if (p(value)) Existent(value) else Inexistent

		/** Extracts the value from the `Potential`, if available.
		  * @return `Existent.unapply(value)`.
		  */
		@inline final def unapply[A](value :Potential[A]) :Opt[A] = Existent.unapply(value)

		/** A factory of 'full' (`Some`) instances of `Potential`.  */
		@SerialVersionUID(Ver) //Synonyms: Extant, Stuff, Lucky, One, Given
		object Existent {
			def apply[A](value :A) :Potential[A] = value match {
				case Inexistent | _ :Existent[_] => new Existent(value).asInstanceOf[Potential[A]]
				case _                           => value.asInstanceOf[Potential[A]]
			}

			def unapply[A](opt :Potential[A]) :Opt[A] = opt match {
				case Inexistent                     => Lack
				case exists :Existent[A @unchecked] => Got(exists.value)
				case _                              => Got(opt.asInstanceOf[A])
			}
		}

		//We don't want anyone to manually wrap a value, as it will not symmetrically equal an erased Potential.
		@SerialVersionUID(Ver)
		private[vars] class Existent[+A](val value :A) extends Serializable {
			override def equals(that :Any) :Boolean = that match {
				case exists :Existent[_] => value == exists.value
				case _                   => false
			}
			override def hashCode :Int = value.hashCode
			override def toString :String = "Existent(" + value + ")"
		}


		/** The only 'empty' value of `Potential`. */ //Synonyms: NoLuck, scala.None
		final val Inexistent :Potential[Nothing] = NonExistent.asInstanceOf[Potential[Nothing]]

		@SerialVersionUID(Ver)
		private[vars] object NonExistent extends (Any => AnyRef) with Serializable {
			override def apply(v1 :Any) = this
			override def toString = "Inexistent"
		}
//
//		/** The for-comprehension facade for `Potential[A]`, which does not evaluate the filter predicate until
//		  * `map`, `flatMap` or `foreach` is called.
//		  */
//		final class WithFilter[+A](self :Potential[A], p :A => Boolean) {
//			@inline def map[B](f: A => B): Potential[B] = self filter p map f
//			@inline def flatMap[B](f: A => Potential[B]): Potential[B] = self filter p flatMap f
//			@inline def foreach[U](f: A => U): Unit = self filter p foreach f
//			@inline def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
//		}



		/** Optional implicit conversions to/from `Opt`, `Option` and `Iterable`.
		  * They involve boxing and are placed here for explicit importing.
		  */
		@SerialVersionUID(Ver)
		object implicits {
			/** An implicit conversion that converts a $Potential to an iterable value. */
			@inline implicit def PotentialToIterable[A](opt :Potential[A]) :Iterable[A] = opt match {
				case Existent(v) => v::Nil
				case _           => Nil
			}

			/** An implicit conversion from a `Potential[A]` to an `Option[A]`.
			  * The results are cached, so repeated conversions of the same instance do not result in boxing.
			  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit.
			  */
			@inline implicit def PotentialToOption[T](opt :Potential[T]) :Option[T] = opt match {
				case Existent(v) => Some(v)
				case _           => None
			}

			/** A nomen omen optional implicit conversion of an `Option[A]` to a `Potential[A]`.
			  * @see [[net.noresttherein.sugar.optional.extensions.OptionExtension]]
			  */
			//consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
			@inline implicit def OptionToPotential[A](opt :Option[A]) :Potential[A] = some_?(opt)

			/** An implicit conversion from an `Potential[A]` to an `Opt[A]`.
			  * The results are cached, so repeated conversions of the same instance do not result in boxing.
			  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit.
			  */
			@inline implicit def PotentialToOpt[T](value :Potential[T]) :Opt[T] = value match {
				case Existent(v) => Got(v)
				case _           => Lack
			}

			/** A nomen omen optional implicit conversion of an `Opt[A]` to a `Potential[A]`. */
			//consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
			@inline implicit def OptToPotential[A](opt :Opt[A]) :Potential[A] = got_?(opt)

			/** Wraps any object in a [[net.noresttherein.sugar.vars.Potential Potential]] monad. */
			@inline implicit def existentAny[A](existent :A) :Potential[A] = Existent(existent)
		}


		/** Importing the contents of this object replace all usage of [[Option]]/[[Some]]/[[None]] in the scope with
		  * [[net.noresttherein.sugar.vars.Potential Potential]]/[[net.noresttherein.sugar.vars.Potential.Existent Existent]]/[[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]].
		  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
		  * which allow seamless interoperability with standard Scala APIs.
		  *
		  * While the idea of using it in production code should be carefully reconsidered,
		  * it is useful for quickly checking during profiling what impact using `Option` vs `Potential` has.
		  *
		  * Other files which reference classes defined in the import's scope may also need to be modified in order
		  * to comply with changed interfaces. */
		@SerialVersionUID(Ver)
		object PotentialAsOption {
			type Option[T] = Potential[T]

			val Option = Potential
			val Some   = Existent
//			val None   = Inexistent
			//same names as in implicits so if both are imported one shadows the other
			@inline implicit def PotentialFromOption[T](opt :Potential[T]) :scala.Option[T] = opt.option
			@inline implicit def OptionToPotential[T](opt :scala.Option[T]) :Potential[T] = fromOption(opt)

//			@inline implicit def NoneToInexistent(none :scala.None.type) :Inexistent.type = Inexistent
//			@inline implicit def InexistentToNone(miss :Inexistent.type) :scala.None.type = scala.None
		}



		private[vars] def unzip[A, B](pair :Potential[(A, B)]) :(Potential[A], Potential[B]) =
			if (pair.asInstanceOf[AnyRef] eq NonExistent)
				unzip2Inexistent
			else {
				val (a, b) = pair.get
				(Existent(a), Existent(b))
			}

		private[vars] def unzip3[A, B, C](triplet :Potential[(A, B, C)]) :(Potential[A], Potential[B], Potential[C]) =
			if (triplet.asInstanceOf[AnyRef] eq NonExistent)
				unzip3Inexistent
			else {
				val (a, b, c) = triplet.get
				(Existent(a), Existent(b), Existent(c))
			}

		private val unzip2Inexistent = (Inexistent, Inexistent)
		private val unzip3Inexistent = (Inexistent, Inexistent, Inexistent)
	}




	@SerialVersionUID(Ver)
	object Defined {
		@inline def apply[T](value :T) :Potential[T] = if (value == null) Inexistent else Existent(value)

		@inline def unapply[T](ref :Ref[T]) :Opt[T] = ref.toOpt
		@inline def unapply[T](ref :Potential[T]) :Opt[T] = Existent.unapply(ref)
	}

	@SerialVersionUID(Ver)
	object Undefined {
		@inline def apply() :Potential[Nothing] = Inexistent

		@inline def unapply(ref :Ref[_]) :Boolean = !ref.isDefined
		@inline def unapply(ref :Potential[_]) :Boolean = ref == Inexistent
	}




	/** Companion object to $Pill, containing conversion methods as well as factories for both cases:
	  * [[net.noresttherein.sugar.vars.Pill.Blue$ Blue]] and [[net.noresttherein.sugar.vars.Pill.Red$ Red]].
	  *
	  * @define Pill       [[net.noresttherein.sugar.vars.Pill! Pill]]
	  * @define Blue       [[net.noresttherein.sugar.vars.Pill.Blue$ Blue]]
	  * @define Red        [[net.noresttherein.sugar.vars.Pill.Red$ Red]]
	  */
	@SerialVersionUID(Ver)
	object Pill {
		/** Converts `Left` to $Red and `Right` to $Blue. */
		def fromEither[R, B](either :Either[R, B]) :Pill[R, B] = either match {
			case Left(red)   => Red(red)
			case Right(blue) => Blue(blue)
		}
		/** Converts $Failed to $Red and $Done to $Blue */
		def fromOutcome[O](either :Outcome[O]) :Pill[Throwable, O] = (either :Any) match {
			case fail :Throwable           => new Red(fail)
			case pass :Done[O @unchecked]  => Blue(pass.value) //remembere that pass.value might be a Red or Blue
			//remember to reify Done[_, Outcome[_, _]] to Blue
			case _    :Red[_] | _ :Blue[_] => new Blue(either).asInstanceOf[Pill[Throwable, O]]
			case _                         => either.asInstanceOf[Pill[Throwable, O]] //erased Blue
		}
//		@inline implicit def pillFromOutcome[O](outcome :Outcome[O]) :Pill[Throwable, O] = fromOutcome(outcome)


		/** A factory and matching pattern for [[net.noresttherein.sugar.vars.Pill! Pill]] instances
		  * representing a successful result of a computation.
		  */
		@SerialVersionUID(Ver)
		object Blue {
			def apply[B](value :B) :Pill[Nothing, B] = value match {
				case _ :Red[_] | _ :Blue[_] => new Blue(value).asInstanceOf[Pill[Nothing, B]]
				case _                      => value.asInstanceOf[Pill[Nothing, B]]
			}

			def unapply[B](alt :Pill[Any, B]) :Opt[B] = alt match {
				case _    :Red[_]             => Lack
				case blue :Blue[B @unchecked] => Got(blue.value)
				case _                        => Got(alt.asInstanceOf[B])
			}
		}

		@SerialVersionUID(Ver) //not a case class to avoid unwanted apply method
		private[vars] class Blue[+B](val value :B) extends Serializable {
			//It might look like equals doesn't handle the equality between reified and erased Blue,
			//but we are always careful to create reified `Blue` if and only if the wrapped value is Blue or Red,
			//so it impossible for two values which nominally should be equal to not compare equal here.
			override def equals(that :Any) :Boolean = that match {
				case blue :Blue[_] => value == blue.value
				case _             => false
			}
			override def hashCode :Int = value.hashCode
			override def toString = "Blue(" + value + ")"
		}

		/** A factory and matching pattern for [[net.noresttherein.sugar.vars.Pill! Pill]] instances
		  * representing a failed result (containing error information).
		  */
		@SerialVersionUID(Ver)
		object Red {
			def apply[R](value :R) :Pill[R, Nothing] = new Red(value)

			def unapply[R](alt :Pill[R, Any]) :Opt[R] = alt match {
				case red :Red[R @unchecked] => Got(red.value)
				case _                      => Lack
			}
		}

		/** The unsuccessful result of a $Pill, carrying error information. It conforms to `Pill[R, Nothing]`,
		  * so it can be carried over while mapping or flat mapping the $Blue case.
		  */
		@SerialVersionUID(Ver)
		private[vars] final case class Red[+R](value :R)


		/** Extra implicit conversions to and from [[scala.Either Either]]
		  * and [[net.noresttherein.sugar.vars.Outcome Outcome]], off by default.
		  * Note: conversions to `Outcome` are also available under
		  * [[net.noresttherein.sugar.vars.Outcome Outcome]]`.`[[net.noresttherein.sugar.vars.Outcome.implicits implicits]],
		  * importing from both objects all definitions will lead to implicit conversion ambiguity.
		  */
		@SerialVersionUID(Ver)
		object implicits {
			@inline implicit def PillFromEither[A, B](either :Either[A, B]) :Pill[A, B] = fromEither(either)
			@inline implicit def PillToEither[A, B](pill :Pill[A, B]) :Either[A, B] = pill.toEither
			@inline implicit def PillFromOutcome[A](outcome :Outcome[A]) :Pill[Throwable, A] = fromOutcome(outcome)
			@inline implicit def StringPillToOutcome[A](pill :Pill[String, A]) :Outcome[A] =
				Outcome.fromStringPill(pill)
			@inline implicit def ThrowablePillToOutcome[A](pill :Pill[Throwable, A]) :Outcome[A] =
				Outcome.fromPill(pill)
		}
	}



	/** A factory of $Outcome, a very lightweight and specialized combination of [[scala.Either Either]]
	  * and [[scala.util.Try Try]] designed to only carry an error message in a $Failed.
	  * @see [[net.noresttherein.sugar.vars.Outcome.Done]]
	  * @see [[net.noresttherein.sugar.vars.Outcome.Failed]]
	  * @define Outcome [[net.noresttherein.sugar.vars.Outcome! Outcome]]
	  * @define Done    [[net.noresttherein.sugar.vars.Outcome.Done Done]]
	  * @define Failed  [[net.noresttherein.sugar.vars.Outcome.Failed Failed]]
	  */
	@SerialVersionUID(Ver)
	object Outcome { //other names: Return; Result; Outcome; Attempt; Action/Act; Do; Compute; Upshot; Issue; Yield

		/** Executes the given expression, and returns it in a $Done if it is not null, or $Failed otherwise.
		  * All exceptions are caught and returned as as `Failed(e)`.
		  */
		@inline def apply[O](value : => O) :Outcome[O] =
			try {
				val v = value
				if (v == null) Failed("null") else Done(v)
			} catch {
				case e :Exception => Failed(e)
			}

		/** Executes the given lazy expression in a `try-catch` block, returning `Failed` in case
		  * any exception is caught. Otherwise the value is returned as a `Done` instance as normal. */
		@inline def guard[A](a : => A) :Outcome[A] =
			try Done(a) catch {
				case e :Exception => Failed(e)
			}

		/** Applies the given function to the second argument in a `try-catch` block, returning `Failed` in case
		  * any exception is caught. Otherwise the result is returned as a `Done` instance as normal. */
		@inline def guard[A, B](f :A => B)(a :A) :Outcome[B] =
			try Done(f(a)) catch {
				case e :Exception => Failed(e)
			}

		/** Converts `Left` to $Failed and `Right` to $Done. */
		def fromStringEither[O](either :Either[String, O]) :Outcome[O] = either match {
			case Left(error)                            => new EagerFailed(error)
			case Right(x @ (_ :Throwable | _ :Done[_])) => new Done(x).asInstanceOf[Outcome[O]]
			case Right(value)                           => value.asInstanceOf[Outcome[O]]
		}
		/** Converts `Left` to $Failed and `Right` to $Done. */
		def fromEither[O](either :Either[Throwable, O]) :Outcome[O] = either match {
			case Left(error)                            => Failed(error)
			case Right(x @ (_ :Throwable | _ :Done[_])) => new Done(x).asInstanceOf[Outcome[O]]
			case Right(value)                           => value.asInstanceOf[Outcome[O]]
		}
		/** Converts $Red to $Failed and $Blue to $Done. */
		def fromStringPill[O](pill :Pill[String, O]) :Outcome[O] = (pill :Any) match {
			case red  :Red[String @unchecked] => new EagerFailed(red.value)
			case blue :Blue[O @unchecked]     => Done(blue.value) //blue.value may be an exception or Done!
			//remember to reify Pill[String, Outcome[_,_]] if needed
			case _    :Throwable | _ :Done[_] => new Done(pill).asInstanceOf[Outcome[O]]
			case _                            => pill.asInstanceOf[Outcome[O]] //erased Blue
		}
		/** Converts $Red to $Failed and $Blue to $Done. */
		def fromPill[O](pill :Pill[Throwable, O]) :Outcome[O] = (pill :Any) match {
			case red  :Red[Throwable @unchecked] => red.value.asInstanceOf[Outcome[O]]
			case blue :Blue[O @unchecked]        => Done(blue.value) //blue.value may be an exception or Done!
			//remember to reify Pill[Throwable, Outcome[_,_]] if needed
			case _    :Throwable | _ :Done[_]    => new Done(pill).asInstanceOf[Outcome[O]]
			case _                               => pill.asInstanceOf[Outcome[O]] //erased Blue
		}


		/** Factory and matching pattern for [[net.noresttherein.sugar.vars.Outcome! Outcome]] instances
		  * representing a passed result (containing a value).
		  */ //consider: another name: Done; Complete; Made; Finished; OK
		@SerialVersionUID(Ver)
		object Done {
			/** Creates an instance of $Done wrapping the value.
			  * This call does not perform any actual boxing unless `value` is already a `Outcome`
			  * (in order to distinguish `Done(Failed())` from `Failed()`.
			  */
			def apply[T](value :T) :Outcome[T] = value match {
				case _ :Throwable | _ :Done[_] => new Done(value).asInstanceOf[Outcome[T]]
				case _                         => value.asInstanceOf[Outcome[T]]
			}
			/** If `exam` is $Done, extracts its value. */
			def unapply[T](exam :Outcome[T]) :Opt[T] = (exam :Any) match {
				case _    :Throwable          => Lack
				case pass :Done[T @unchecked] => Got(pass.value)
				case _                        => Got(exam.asInstanceOf[T])
			}
		}

		/** A reified `Done` case used when `value` is a `Failed` or `Done` instance.
		  * Used to differentiate between `Done(Failed)` and `Failed`.
		  */
		@SerialVersionUID(Ver)
		private[vars] class Done[+T](val value :T) extends Serializable {
			//it might look like equals doesn't handle the equality between reified and erased Done,
			//but we are always careful to create reified `Done` if and only if the wrapped value is Done or Failed,
			//so it impossible for two values which nominally should be equal to not compare equal here.
			override def equals(that :Any) :Boolean = that match {
				case other :Done[_] => value == other.value
				case _ => false
			}
			override def hashCode :Int = value.hashCode
			override def toString :String = "Done(" + value + ")"
		}


		/** Factory and matching pattern for [[net.noresttherein.sugar.vars.Outcome! Outcome]] instances
		  * representing a failed result (containing an error message).
		  */ //consider: other names. Abort/Aborted. Stop.
		@SerialVersionUID(Ver)
		object Failed {
			/** A `Failed` instance with an empty message. */
			def apply() :Outcome[Nothing] = failed

			/** A failed instance with the given error message. */
			def apply(error :String) :Outcome[Nothing] = new EagerFailed(error)

			/** A `Failed` with a lazily evaluated message. The method is designed for function literal arguments:
			  * as `Failed` is a SAM type, function literals will be promoted to a `Failed` instance,
			  * which is then promptly returned. Example:
			  * {{{
			  *     Failed(() => "Oh-oh.")
			  * }}}
			  */
			@inline def apply(error :Failed) :Outcome[Nothing] = error

			/** A `Failed` wrapping the given exception and sharing its error message. */
			@inline def apply(e :Throwable) :Outcome[Nothing] = e.asInstanceOf[Outcome[Nothing]]


			/** Extracts the message from the argument `Outcome` if it is `Failed`.
			  * Note that this will evaluate a lazy message.
			  */
			def unapply(exam :Outcome[Any]) :Opt[Throwable] = exam match {
				case fail :Throwable => Got(fail)
				case _               => Lack
			}
			private[this] val failed = new EagerFailed("")
		}

		/** The default unsuccessful result of $Outcome, carrying an error message. It conforms to `Outcome[Nothing]`,
		  * so it can be carried over while mapping or flat mapping the $Done case.
		  *
		  * ''Not'' all cases of `Failed` are an instance of this class!
		  * Use pattern matching with the companion object to check if a given `Outcome` has failed.
		  *
		  * This is a SAM type, so `() => String` function literals are automatically promoted to a `Failed`
		  * implementation wherever a `Failed` type is expected, for example in
		  * [[net.noresttherein.sugar.vars.Outcome.Failed.apply Failed.apply]].
		  * This ensures that error message itself is lazily built, avoiding a potentially large creation cost
		  * unless really needed, that the created closure is serializable, and that only one object is created
		  * (rather than a closure for `=> String` and `Failed` itself).
		  */ //It can't be package protected, because we use it for SAM type function literal conversion.
		abstract class Failed extends Exception with SugaredException {
			/** Evaluates the error message. Each call may result in reevaluation - prefer using
			  * [[net.noresttherein.sugar.vars.Outcome.Failed.msg msg]]. The method is named `apply()`
			  * so that `() => "error"` literals are automatically SAM-convertible to a `Outcome` if `Failed` type
			  * is expected.
			  */
			protected def apply() :String

			/** The error message detailing the cause why the $Outcome computation failed. */
			override lazy val msg :String = apply()
			def canEqual(that :Any) :Boolean = that.isInstanceOf[Failed]
			override def equals(that :Any) :Boolean = that match {
				case other :Failed => (this eq other) || msg == other.msg
				case _             => false
			}
			override def hashCode :Int = msg.hashCode
			override def className :String = "Failed"
		}

		@SerialVersionUID(Ver)
		private class EagerFailed(message :String, cause :Throwable = null) extends Failed {
			override def apply() :String = message
			override def addInfo(msg :String) :SugaredThrowable = new EagerFailed(message, this)
		}



		/** Extra implicit conversions to $Pill and [[scala.Either Either]], which are not in the scope by default
		  * in order to avoid unintended boxing, but might be useful in code which deals with a lot of both types.
		  */
		@SerialVersionUID(Ver)
		object conversions {
			@inline implicit def OutcomeFromStringEither[O](either :Either[String, O]) :Outcome[O] =
				fromStringEither(either)
			@inline implicit def OutcomeFromThrowableEither[O](either :Either[Throwable, O]) :Outcome[O] =
				fromEither(either)
			implicit def OutcomeToStringEither[O](outcome :Outcome[O]) :Either[String, O] = (outcome :Any) match {
				case fail :Throwable =>
					val msg = fail.getMessage
					Left(if (msg == null) "" else msg)
				case pass :Done[O @unchecked] => Right(pass.value)
				case _                        => Right(outcome.asInstanceOf[O])
			}
			@inline implicit def OutcomeToThrowableEither[O](outcome :Outcome[O]) :Either[Throwable, O] =
				outcome.toEither

			@inline implicit def OutcomeFromThrowablePill[O](pill :Pill[Throwable, O]) :Outcome[O] =
				fromPill(pill)
			@inline implicit def OutcomeFromStringPill[O](pill :Pill[String, O]) :Outcome[O] =
				fromStringPill(pill)
			@inline implicit def OutcomeToThrowablePill[O](outcome :Outcome[O]) :Pill[Throwable, O] =
				Pill.fromOutcome(outcome)
			implicit def OutcomeToStringPill[O](outcome :Outcome[O]) :Pill[String, O] = (outcome :Any) match {
				case fail :Throwable =>
					val msg = fail.getMessage
					Red(if (msg == null) "" else msg)
				case pass :Done[O @unchecked] => Blue(pass.value)
				case _                        => Blue(outcome.asInstanceOf[O])
			}
		}
	}
}
