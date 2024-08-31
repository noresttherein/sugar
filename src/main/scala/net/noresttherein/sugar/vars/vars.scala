	package net.noresttherein.sugar

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

import net.noresttherein.sugar
import net.noresttherein.sugar.collections.Ranking
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.exceptions.{SugaredException, SugaredThrowable}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Nullable.{NonNull, Null}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Outcome.{Done, Failed}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Term.{Expression, Value}




/** A home to a wide class hierarchy of value wrappers: [[net.noresttherein.sugar.vars.InOut in/out]]
  * method parameters, [[net.noresttherein.sugar.vars.Atomic atomic]]
  * and [[net.noresttherein.sugar.vars.ThreadLocal thread local]] variables,
  * several [[net.noresttherein.sugar.vars.Lazy lazy]] `val` implementations,
  * [[net.noresttherein.sugar.vars.Maybe various]] `Option` [[net.noresttherein.sugar.vars.Unsure alternatives]],
  * [[net.noresttherein.sugar.vars.Channel synchronization]] tools
  * and [[net.noresttherein.sugar.vars.SignalVar conditional variables]],
  * [[net.noresttherein.sugar.vars.Watched observed]] variables,
  * disposable [[java.lang.ref.Reference]] [[net.noresttherein.sugar.vars.DisposableRef adapters]] and others.
  *
  * @define Opt [[net.noresttherein.sugar.vars.Opt! Opt]]
  * @define One [[net.noresttherein.sugar.vars.Opt.One One]]
  * @define None [[scala.None None]]
  * @define Outcome [[net.noresttherein.sugar.vars.Outcome! Outcome]]
  * @define Done [[net.noresttherein.sugar.vars.Outcome.Done Done]]
  * @define Failed [[net.noresttherein.sugar.vars.Outcome.Failed Failed]]
  * @define Pill [[net.noresttherein.sugar.vars.Pill! Pill]]
  * @define Blue [[net.noresttherein.sugar.vars.Pill.Blue$ Blue]]
  * @define Red  [[net.noresttherein.sugar.vars.Pill.Red$ Red]]
  */
package object vars extends vars.varsTypeClasses {

	private[vars] final val Ver = sugar.Ver

	/** An erased variant of [[scala.Option]], with API defined by extension methods
	  * in [[net.noresttherein.sugar.vars.OptExtension OptExtension]].
	  * an `Opt[A]` can have three forms:
	  *   1. $None, serving the same role as in `scala.Option`,
	  *   1. $Done[A], erased to `A` (which may be a boxed version of a value type, both inbuilt or a value class);
	  *   1. $Done[A], wrapped - used to differentiate `One(None)` from `None`.
	  *
	  * Outside nesting `Opt` instances within each other (or `Option` within `Opt`),
	  * no boxing takes place for reference types under any circumstances, in particular when creating
	  * an `Array[Opt[A]]` or `Seq[Opt[A]]`, using it as an argument or return value of a function, or,
	  * in general, substituting it for an abstract type (including type parameters). It is thus well suited
	  * for monadic composition of computations through `flatMap`, avoiding boxing of the result of each step.
	  * The type is not `@specialized`, so boxing of value types to their Java wrappers, as well as lifting
	  * of value classes to their reference representations, will still occur: all values and arguments
	  * of type `Opt[A]` are erased to `AnyRef` by the compiler. Code
	  * {{{
	  *     val qoluae = Opt(42)
	  * }}}
	  * will compile to `Integer.valueOf(42)`, unless aggressively inlined by the compiler.
	  *
	  * The downsides are:
	  *   - cannot be used as a type pattern in pattern matching
	  *   - cannot be used as a return type of `unapply` methods (but see [[net.noresttherein.sugar.vars.Maybe Maybe]]).
	  *
	  * Implicit conversions exists to and from `Maybe[A]` in order to improve interoperability; they do not involve
	  * any boxing in either direction, except for the case `Yes(None) :One[_]`.
	  * @see [[net.noresttherein.sugar.vars.Maybe]]
	  * @see [[net.noresttherein.sugar.vars.Unsure]]
	  * @see [[net.noresttherein.sugar.vars.Nullable]]
	  * @see [[net.noresttherein.sugar.vars.IntOpt]]
	  * @see [[net.noresttherein.sugar.vars.Ternary]]
	  */
	//other names: Hope/Lucky/NoLuck(Vain); Perhaps; Wish/Granted/Denied; Optional/Defined/Undefined; Space/Filled/Blank
	type Opt[+A] >: None.type <: AnyRef

	/** An alias for $Opt`[A]`, a fully erased variant of [[scala.Option]] with an API defined
	  * by [[net.noresttherein.sugar.vars.OptExtension OptExtension]] as extension methods.
	  * @see [[net.noresttherein.sugar.vars.Opt.One]]
	  * @see [[scala.None]]
	  * @see [[net.noresttherein.sugar.vars.Maybe]]
	  * @see [[net.noresttherein.sugar.vars.Unsure]]
	  * @see [[net.noresttherein.sugar.vars.Nullable]]
	  * @see [[net.noresttherein.sugar.vars.IntOpt]]
	  * @see [[net.noresttherein.sugar.vars.Ternary]]
	  */
	type ??[+A] = Opt[A]


	/** The API of $Opt in the form of extension methods.
	  * @define Ref `Opt`
	  * @define coll potential value
	  * @define Opt [[net.noresttherein.sugar.vars.Opt! Opt]]
	  */
	implicit class OptExtension[A](private val self :Opt[A]) extends AnyVal {

		/** Tests if this `Opt` does not contain a value
		  * (is equal to [[scala.None None]]). */
		@inline def isEmpty: Boolean = self.asInstanceOf[AnyRef] eq None

		/** Tests if this `Opt` is defined. If true, `get` will not throw an exception. */
		@inline def nonEmpty: Boolean = self.asInstanceOf[AnyRef] ne None

		/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
		@inline def isConst: Boolean = self.asInstanceOf[AnyRef] ne None

		/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
		@inline def isDefined: Boolean = self.asInstanceOf[AnyRef] ne None

		/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
		@inline def isDefinite :Boolean = self.asInstanceOf[AnyRef] ne None

		/** Returns `1` if the `Opt` carries a value and `0` otherwise. */
		@inline def size :Int = if (self.asInstanceOf[AnyRef] eq None) 0 else 1

		/** Forces extraction of the value.
		  * @return contained value, if one exists.
		  * @throws NoSuchElementException if this `Opt` is empty. */
		@inline def get :A = (self :Any) match {
			case None                      => noSuch_!("None.get")
			case exists :One[A @unchecked] => exists.value
			case _                         => self.asInstanceOf[A]
		}

		/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
		@inline def getOrElse[O >: A](or: => O) :O =
			if (self.asInstanceOf[AnyRef] eq None) or else get

		/** Similarly to [[net.noresttherein.sugar.vars.OptExtension.getOrElse getOrElse]],
		  * returns the value if non-empty and `alt` otherwise. The difference is that the alternative value
		  * is not lazily computed and guarantees no closure will be created,
		  * at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def orDefault[O >: A](or: O) :O =
			if (self.asInstanceOf[AnyRef] eq None) or else get

		/** Assuming that `A` is a nullable type, return `null` if this `Opt` is `None`,
		  * or the wrapped value otherwise. */
		@inline def orNull[O >: A](implicit isNullable :Null <:< O) :O =
			if (self.asInstanceOf[AnyRef] eq None) null.asInstanceOf[O] else get

		/** Gets the element in this `Opt` or throws the exception given as the type parameter
		  * with the given message.
		  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
		  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
		  * @see [[net.noresttherein.sugar.vars.OptExtension.orNoSuch orNoSuch]]
		  * @see [[net.noresttherein.sugar.vars.OptExtension.orIllegal orIllegal]] */
		@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :A =
			if (self.asInstanceOf[AnyRef] eq None) raise[E](msg) else get

		/** Gets the element in this `Opt` or throws a [[NoSuchElementException]] with the given message.
		  * @see [[net.noresttherein.sugar.vars.OptExtension.orThrow orThrow]] */
		@inline def orNoSuch(msg: => String) :A =
			if (self.asInstanceOf[AnyRef] eq None) noSuch_!(msg) else get

		/** Gets the element in this `Opt` or throws an [[IllegalArgumentException]] with the given message.
		  * @see [[net.noresttherein.sugar.vars.OptExtension.orThrow orThrow]] */
		@inline def orIllegal(msg: => String) :A =
			if (self.asInstanceOf[AnyRef] eq None) illegal_!(msg) else get

		/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
		@inline def orError(msg: => String) :A = {
			assert(self.asInstanceOf[AnyRef] ne None, msg)
			get
		}


		/** Returns the value this `Opt` if it is not empty, or the lazily computed alternative otherwise. */
		@inline def orElse[O >: A](or: => Opt[O]) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None) or else self

		/** Similarly to [[net.noresttherein.sugar.vars.OptExtension.orElse orElse]], returns this `Opt`
		  *  if it is not empty and `or` otherwise. The difference is that the alternative value is not lazily computed
		  *  and guarantees no closure would be created, at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def ifEmpty[O >: A](or: Opt[O]) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None) or else self

		/** Returns this `Opt` if the condition is false and `No` if it is true. This is equivalent
		  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
		@inline def orEmptyIf(condition :Boolean) :Opt[A] =
			if (condition) None else self

		/** Returns this `Opt` if the condition is true and `No` if it is false. This is equivalent
		  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better. */
		@inline def orEmptyUnless(condition :Boolean) :Opt[A] =
			if (condition) self else None


		/** Returns a new `Opt` which is empty ''iff'' this value is empty, or one containing
		  * the result of applying the given function to its value otherwise. */
		@inline def map[O](f :A => O) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None) None else One(f(get))

		/** Applies the given function to the content of this `Opt` and returns the result
		  * or the provided alternative if this instance is empty.
		  * Equivalent to `this map f getOrElse or`, but in one step. */
		@inline def mapOrElse[O](f :A => O, or: => O) :O =
			if (self.asInstanceOf[AnyRef] eq None) or else f(get)

		/** Returns the result of applying `f` to the value of this `Opt` if it is non-empty,
		  * or the result of evaluating expression `ifEmpty` otherwise.
		  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
		  * one with another, but its name might be misleading. Consider using
		  * [[net.noresttherein.sugar.vars.OptExtension.mapOrElse mapOrElse]] instead.
		  *  @param  ifEmpty the expression to evaluate if empty.
		  *  @param  f       the function to apply if nonempty. */
		@inline def fold[O](ifEmpty: => O)(f: A => O): O =
			if (self.asInstanceOf[AnyRef] eq None) ifEmpty else f(get)

		/** The same as [[net.noresttherein.sugar.vars.OptExtension.map map]], but exceptions thrown
		  * by the function are caught and $None is returned instead. */
		@inline def guardMap[O](f :A => O) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None)
				None
			else
				try One(f(get)) catch {
					case _ :Exception => None
				}

		/** Returns the result of applying the given function to the value of this `Opt` if it is not empty,
		  * or `this` if `this.isEmpty`. */
		@inline def flatMap[O](f :A => Opt[O]) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None) None else f(get)

		/** Flattens `Opt[Opt[O]]` to a single `Opt[O]`. */
		@inline def flatten[O](implicit isPotential :A <:< Opt[O]) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None) None else get

		/** Returns `None` if `this.contains(o)`, or `this` otherwise. */
		@inline def removed[O >: A](o :O) :Opt[A] =
			if ((self.asInstanceOf[AnyRef] eq None) || get == o) None else self

		/** Returns `None` if `this.isEmpty` or `that` contains `this.get`, or `this` otherwise. */
		def removedAll[O >: A](that :IterableOnce[O]) :Opt[A] = that match {
			case _ if self.asInstanceOf[AnyRef] eq None => self
			case it :Set[O]     => if (it(get)) None else self
			case it :Ranking[O] => if (it.contains(get)) None else self
			case it :Iterable[O] if it.isEmpty => self
			case _ =>
				val i = that.iterator
				val x = get
				while (i.hasNext)
					if (i.next() == x)
						return None
				self
		}

		/** Returns a new `Opt` containing this value if it is not empty and its value satisfies
		  * the given predicate, or [[scala.None None]] otherwise. */
		@inline def filter(p :A => Boolean) :Opt[A] =
			if ((self.asInstanceOf[AnyRef] eq None) || p(get)) self else None

		/** Returns a new `Opt` containing this value if it is not empty and its value falsifies
		  * the given predicate, or [[scala.None None]] otherwise. */
		@inline def filterNot(p :A => Boolean) :Opt[A] =
			if ((self.asInstanceOf[AnyRef] eq None) || !p(get)) self else None

		/** Equivalent to `this.`[[net.noresttherein.sugar.vars.OptExtension.filter filter]]`(p)` -
		  * a variant for use in for-comprehensions. Note that as this implementation is performance oriented,
		  * it evaluates the predicate immediately, unlikely standard methods of [[scala.collection.Iterable Iterable]]. */
		@inline def withFilter(p :A => Boolean) :Opt[A] =
			if ((self.asInstanceOf[AnyRef] eq None) || p(get)) self else None

		/** Tests if this `Opt` is not empty and its value is equal to the given argument. */
		@inline def contains[O >: A](o :O): Boolean = (self.asInstanceOf[AnyRef] ne None) && get == o

		/** Tests if this `Opt` is not empty and its value satisfies the given predicate. */
		@inline def exists(p :A => Boolean): Boolean = (self.asInstanceOf[AnyRef] ne None) && p(get)

		/** Tests if this `Opt` contains no value or its value satisfies the given predicate. */
		@inline def forall(p :A => Boolean): Boolean = (self.asInstanceOf[AnyRef] eq None) || p(get)

		/** Executes the given block for this `Opt`'s value if it is not empty. */
		@inline def foreach[O](f :A => O) :Unit = if (self.asInstanceOf[AnyRef] ne None) f(get)

		/** Returns `None` if this `Opt` is empty or the partial function `f` is not defined for its value,
		  * otherwise applies it and wraps the result it in a new `Maybe`. */
		@inline def collect[O](f :PartialFunction[A, O]) :Opt[O] =
			if (self.asInstanceOf[AnyRef] eq None)
				None
			else
				f.applyOrElse(get, Maybe.NoContent.asInstanceOf[A => O]) match {
					case Maybe.NoContent => None
					case result        => One(result)
				}


		/** Returns an `Opt` formed from the contents of `this` and `that` by combining the corresponding elements
		  * in a pair. If either of the two options is empty, `None` is returned. */
		@inline def zip[O](that :Opt[O]) :Opt[(A, O)] =
			if ((self.asInstanceOf[AnyRef] eq None) | (that.asInstanceOf[AnyRef] eq None))
				None
			else One((get, that.get))

		/** Converts an `Opt` of a pair into `Opt`s of its first and second elements. */
		@inline def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (Opt[A1], Opt[A2]) =
			Opt.unzip(asPair.liftCo[Opt](self))

		/** Converts an `Opt` of a triple into three `Opt`s, one containing the element
		  * from each position of the triple. */
		@inline def unzip3[A1, A2, A3](implicit asTriple: A <:< (A1, A2, A3)): (Opt[A1], Opt[A2], Opt[A3]) =
			Opt.unzip3(asTriple.liftCo[Opt](self))


		/** An iterator returning this value as the only element if `this.nonEmpty`. */
		@inline def iterator :Iterator[A] =
			if (self.asInstanceOf[AnyRef] eq None) Iterator.empty else Iterator.single(get)

		/** Returns `Nil` if this `Opt` is empty or or `this.get::Nil` otherwise. */
		@inline def toList :List[A] = if (self.asInstanceOf[AnyRef] eq None) Nil else get::Nil

		/** Returns an empty list if this `Opt` is empty or a single element list with its value otherwise. */
		@inline def toSeq :Seq[A] = if (self.asInstanceOf[AnyRef] eq None) Nil else get::Nil

		/** Returns an empty collection if this `Opt` is empty or a singleton with its value otherwise. */
		@inline def toIterable :Iterable[A] =
			if (self.asInstanceOf[AnyRef] eq None) Iterable.empty else Iterable.single(get)

		/** Same as [[net.noresttherein.sugar.vars.OptExtension.toOption toOption]]. */
		@inline def option :Option[A] = if (self.asInstanceOf[AnyRef] eq None) None else Some(get)

		/** Same as [[net.noresttherein.sugar.vars.OptExtension.toMaybe toMaybe]]. */
		@inline def maybe :Maybe[A] = if (self.asInstanceOf[AnyRef] eq None) No else Yes(get)

		/** Same as [[net.noresttherein.sugar.vars.OptExtension.toUnsure toUnsure]]. */
		@inline def unsure :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq None)
				Missing
			else {
				val a = get
				new Sure(a, cachedOpt = One(a))
			}

		/** Conversion to standard Scala [[scala.Option]].
		  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
		@inline def toOption :Option[A] = if (self.asInstanceOf[AnyRef] eq None) None else Some(get)

		/** Conversion to a value class `Maybe`. The difference is that `Maybe` will be reified to a runtime object
		  * wrapping its value when used as a value of its supertype or an abstract type (for example, a type parameter).
		  * The benefit is that, unlike `Opt`, it loses no information and can be pattern matched,
		  * but at the cost of boxing. There is an implicit conversion to the same effect.
		  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
		@inline def toMaybe :Maybe[A] = if (self.asInstanceOf[AnyRef] eq None) No else Yes(get)

		/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
		  * is specialized for value types, this type is not, and the result will not be specialized. Neither will it
		  * require boxing though, as any value type was promoted to a reference wrapper when creating this `Opt`. */
		@inline def toUnsure :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq None)
				Missing
			else {
				val a = get
				new Sure(a, cachedOpt = One(a))
			}

		/** Same as [[net.noresttherein.sugar.vars.OptExtension.toOption toOption]]
		  * (for [[net.noresttherein.sugar.vars.Ref Ref]] interoperability). */
		@inline def constOption :Option[A] = if (self.asInstanceOf[AnyRef] eq None) None else Some(get)

		/** Same as [[net.noresttherein.sugar.vars.OptExtension.toMaybe toMaybe]]
		  * (for [[net.noresttherein.sugar.vars.Ref Ref]] interoperability). */
		@inline def constOpt :Maybe[A] = if (self.asInstanceOf[AnyRef] eq None) No else Yes(get)

		/** Same as [[net.noresttherein.sugar.vars.OptExtension.toUnsure toUnsure]]
		  * (for [[net.noresttherein.sugar.vars.Ref Ref]] interoperability). */
		@inline def unsureConst :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq None)
				Missing else {
				val a = get
				new Sure(a, cachedOpt = One(a))
			}


		/** Converts this `Opt` to `Either`, returning the content as `Left`,
		  *  or the value of the given expression as `Right` if empty. */
		@inline def toLeft[O](right: => O) :Either[A, O] =
			if (self.asInstanceOf[AnyRef] eq None) Right(right) else Left(get)

		/** Converts this `Opt` to `Either`, returning the content as `Right`,
		  *  or the value of the given expression as `Left` if empty. */
		@inline def toRight[O](left: => O) :Either[O, A] =
			if (self.asInstanceOf[AnyRef] eq None) Left(left) else Right(get)

		/** Converts this `Opt` to $Pill, returning the content as $Red,
		  *  or the value of the given expression as $Blue if empty. */
		@inline def toRed[O](blue: => O) :Pill[A, O] =
			if (self.asInstanceOf[AnyRef] eq None) Blue(blue) else Red(get)

		/** Converts this `Opt` to $Pill, returning the content as $Blue,
		  *  or the value of the given expression as $Red if empty. */
		@inline def toBlue[O](red: => O) :Pill[O, A] =
			if (self.asInstanceOf[AnyRef] eq None) Red(red) else Blue(get)

		/** Converts this `Opt` to $Outcome, returning the content as $Done,
		  * or the given `String` as $Failed error message if empty. */
		@inline def doneOr(err: => String) :Outcome[A] =
			if (self.asInstanceOf[AnyRef] eq None) Failed(() => err) else Done(get)

		/** Converts this `Opt` to $Outcome, returning the content as $Done,
		  *  or the given `Throwable` as $Failed error if empty. */
		@inline def doneOr(err :Throwable) :Outcome[A] =
			if (self.asInstanceOf[AnyRef] eq None) Failed(err) else Done(get)


		/** Formats this `Opt` like a collection: as `s"$prefix()"` or `s"$prefix($get)"`. */
		@inline def mkString(prefix :String) :String =
			if (self.asInstanceOf[AnyRef] eq None) prefix + "()" else prefix + "(" + self + ")"

		/** Formats this `Opt` as `s"Opt($get)"` or `"Opt()"`. */
		@inline def mkString :String =
			if (self.asInstanceOf[AnyRef] eq None) "Opt()" else "Opt(" + self + ")"

		/** Compares the contents for equality, with the result being false if any of the operands are empty. */
		@inline def same(other :Opt[_]) :Boolean =
			(self.asInstanceOf[AnyRef] ne None) & (other.asInstanceOf[AnyRef] ne None) && self == other

		/** Returns `for (a <- this; b <- other) yield a == b`. */
		@inline def sameOpt(other :Opt[_]) :Opt[Boolean] =
			(if (self.asInstanceOf[AnyRef] eq None) self
			 else if (other.asInstanceOf[AnyRef] eq None) other
			 else (self == other)
			).asInstanceOf[Opt[Boolean]]
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
	  */
	//The fact that R stands both for Red and Right, but Red is used as Left is a bit confusing.
	// However, the convention is for Either to be right-leaning, that is to carry the computation value in Right,
	// and it would be even more confusing for Red to be a success, rather than a failure.
	type Pill[+Red, +Blue] >: Pill.Red[Red] <: AnyRef

	/** Extension methods providing the interface of $Pill.
	  * @tparam R the type carried by the 'red' case, corresponding to the `Left` side of an `Either`.
	  * @tparam B the type carried by the 'blue' case, corresponding to the `Right` side of an `Either`.
	  * @define Pill [[net.noresttherein.sugar.vars.Pill! Pill]]
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
			case red  :Red[_]             => noSuch_!(red.toString)
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
			case red :Red[_] => noSuch_!(red.value.toString)
			case _           => get
		}
		/** Gets the element in this $Pill if it is $Blue, or throws an [[IllegalArgumentException]]
		  * with [[net.noresttherein.sugar.vars.Pill.Red.value value]]`.toString` as the error message if $Red.
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orThrow orThrow]] */
		@inline def orIllegal :B = self match {
			case red :Red[_] => illegal_!(red.value.toString)
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
		  * but works on the `Red` (`Left`) side. */
		@inline def joinRed[R1 >: R, B1 >: B, O](implicit redIsAlt :R1 <:< Pill[O, B1]) :Pill[O, B1] =
			self match {
				case red :Red[R @unchecked] => red.value
				case _                      => self.asInstanceOf[Pill[O, B1]]
			}
		/** Flattens `Pill[R, Pill[R, O]]` to `Pill[R, O]`: returns the value of this $Pill if it is $Red,
		  * or itself if it is $Blue. This is equivalent to [[net.noresttherein.sugar.vars.PillExtension.flatten flatten]],
		  * but allows to widen the type of the red pill. */
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

		/** Converts this value to a `Maybe` if it is $Blue, losing the information by replacing
		  * $Red with [[net.noresttherein.sugar.vars.Maybe.No No]]. */
		@inline def toMaybe :Maybe[B] = self match {
			case _ :Red[_] => No
			case _         => Yes(get)
		}
		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isBlue` or `None` otherwise. */
		@inline def toOption :Option[B] = self match {
			case _ :Red[_] => None
			case _         => Some(get)
		}
		/** Converts this value to an `Opt` if it is $Blue, losing the information by replacing
		  * $Red with [[scala.None None]]. */
		@inline def toOpt :Opt[B] = self match {
			case _ :Red[_] => None
			case _         => One(get)
		}
		/** Conversion to an `Unsure` carrying the value of this instance if it is $Blue.
		  * Note that the result will not be `specialized` for value types, but neither will it require boxing,
		  * as $Blue already contains boxed values. */
		@inline def toUnsure :Unsure[B] = self match {
			case _ :Red[_] => Missing
			case _         => Sure(get)
		}
		/** Conversion to [[scala.Either]], with $Red returned as [[scala.Right Right]] and $Blue as [[scala.Left Left]]. */
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

		/** Returns [[net.noresttherein.sugar.vars.Maybe.No No]] if any of the operands are $Red,
		  * and `this == other` otherwise. */
		@inline def blueEqualsOpt(other :Pill[_, _]) :Maybe[Boolean] =
			if (self.isInstanceOf[Red[_]] | other.isInstanceOf[Red[_]]) No
			else Yes(self == other)

		/** Returns [[net.noresttherein.sugar.vars.Maybe.No No]] if any of the operands are $Blue,
		  * and `this == other` otherwise. */
		@inline def redEqualsOpt(other :Pill[_, _]) :Maybe[Boolean] = (self, other) match {
			case (a :Red[_], b :Red[_]) => Yes(a.value == b.value)
			case _                      => No
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
	//consider: Renaming to Opt. The advantage lies in that methods could be named yieldXxx (and in a shorter name),
	// but `Done` and `Failed` are not good name matches for its subtypes.
	type Outcome[+A] >: Failed //can be also named Fail

	/** Extension methods providing the full interface of $Outcome.
	  * @define Outcome [[net.noresttherein.sugar.vars.Outcome! Outcome]]
	  */
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
			case fail :Throwable          => noSuch_!(fail)
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
			case fail :Failed    => noSuch_!(fail.msg)
			case fail :Throwable => noSuch_!(fail)
			case _ => get
		}

		/** Gets the element in this $Outcome if it is $Done, or throws an [[IllegalArgumentException]]
		  * with the exception carried by [[net.noresttherein.sugar.vars.Outcome.Failed Failed]] as its cause otherwise.
		  * @see [[net.noresttherein.sugar.vars.PillExtension.orThrow orThrow]] */
		@inline def orIllegal :A = self match {
			case fail :Failed    => illegal_!(fail.msg)
			case fail :Throwable => illegal_!(fail)
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
		  * and guarantees no closure would be created, at the cost of possibly discarding it without use.
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
		  * by the function are caught and $Failed with the exception's error message is returned. */
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

		/** Converts this value to a `Maybe` if it is $Done, losing the information by replacing
		  * $Failed with [[net.noresttherein.sugar.vars.Maybe.No No]]. */
		@inline def toMaybe :Maybe[A] = self match {
			case _ :Throwable => No
			case _            => Yes(get)
		}

		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isPassed` or `None` otherwise. */
		@inline def toOption :Option[A] = self match {
			case _ :Throwable => None
			case _            => Some(get)
		}

		/** Converts this value to an `Opt` if it is $Done, losing the information by replacing
		  * $Failed with [[scala.None None]]. */
		@inline def toOpt :Opt[A] = self match {
			case _ :Throwable => None
			case _            => One(get)
		}

		/** Conversion to an `Unsure` carrying the value of this instance if it is $Done.
		  * Note that the result will not be `specialized` for value types, but neither will it require
		  * additional boxing, as $Done already contains boxed values. */
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

		/** Returns `Yes(this == other)` if both operands are $Done, or `No` otherwise. */
		@inline def sameOpt(other :Outcome[_]) :Maybe[Boolean] =
			if (self.isInstanceOf[Throwable] | other.isInstanceOf[Throwable]) No
			else Yes(self == other)
	}



	/** A value of type `X` passed either 'by value' (as a constant) or 'by name' (as a Scala expression evaluated
	  * each time the value is needed). It is equivalent to `Either[X, () => X]`, but it does not box.
	  * The factories for each of the cases are located in the companion object
	  * [[net.noresttherein.sugar.vars.Term$ Term]]:
	  *   - [[net.noresttherein.sugar.vars.Term.Expression Expression]] wraps a `() => X`
	  *   - [[net.noresttherein.sugar.vars.Term.Value Value]] wraps an `X`.
	  *
	  * Both serve also as match patterns distinguishing the cases. The only circumstance in which an instance
	  * of `Term[X]` requires runtime boxing is if `X <: Function0[_]` itself.
	  *
	  * This type is in particular useful in classes which implement lazy value semantics:
	  * a `Term[X]` field can be set to an `Expression(init)`, and later replaced with a `Value(init())`.
	  * @see [[net.noresttherein.sugar.vars.Term$]]
	  * @see [[net.noresttherein.sugar.vars.TermExtension]]
	  */
	type Term[+X] >: Term.Value[X] <: AnyRef

	/** Extension methods for [[net.noresttherein.sugar.vars.Term! Term]] providing access to the value
	  * and allowing to distinguish between the two cases.
	  */
	implicit class TermExtension[X](private val self :Term[X]) extends AnyVal {
		/** Returns the value of this term, evaluating it
		  * if it is an [[net.noresttherein.sugar.vars.Term.Expression Expression]]. */
		def apply() :X = self match {
			case init :(() => X @unchecked) => init()
			case v    :Value[X] @unchecked  => v.value
			case v    :X @unchecked         => v
		}

		/** Same as [[net.noresttherein.sugar.vars.TermExtension.isExpression isExpression]]. */
		@inline def isByName :Boolean = self.isInstanceOf[() => Any]

		/** True if this term is a by-name expression
		  * created with [[net.noresttherein.sugar.vars.Term.Expression Expression]]
		  * or [[net.noresttherein.sugar.vars.Term.ByName ByName]]. */
		@inline def isExpression :Boolean = self.isInstanceOf[() => Any]

		/** True if this term is a constant value created with [[net.noresttherein.sugar.vars.Term.Value Value]]. */
		@inline def isValue :Boolean = !self.isInstanceOf[() => Any]

		/** Same as [[net.noresttherein.sugar.vars.TermExtension.isValue isValue]]. */
		@inline def isByValue :Boolean = !self.isInstanceOf[() => Any]

		/** Returns the lazy expression of this instance in a [[net.noresttherein.sugar.vars.Maybe.Yes Yes]]
		  * if it is an [[net.noresttherein.sugar.vars.Term.Expression Expression]]. */
		@inline def asExpression :Maybe[() => X] = Expression.unapply(self)

		/** Returns the value of this instance in a [[net.noresttherein.sugar.vars.Maybe.Yes Yes]]
		  * if it is a constant [[net.noresttherein.sugar.vars.Term.Value Value]]. */
		@inline def asValue      :Maybe[X] = Value.unapply(self)

		def toEither :Either[() => X, X] = self match {
			case init :(() => X @unchecked) => Left(init)
			case v    :Value[X] @unchecked  => Right(v.value)
			case v    :X @unchecked         => Right(v)
		}
		def toPill :Pill[() => X, X] = self match {
			case init :(() => X @unchecked) => Red(init)
			case v    :Value[X @unchecked]  => Blue(v.value)
			case v    :X @unchecked         => Blue(v)
		}
	}
}






package vars {

	private[sugar] sealed abstract class varsTypeClasses {
//		@inline implicit def OptToMaybe[T](opt :Opt[T]) :Maybe[T] = opt.toMaybe //consider: making these explicit
//		@inline implicit def MaybeToOpt[T](opt :Maybe[T]) :Opt[T] = Opt.yes_?(opt)
		@inline final implicit def OptClassTag     :ClassTag[Opt[Nothing]] = ClassTag.AnyRef.castParam[Opt[Nothing]]
		@inline final implicit def OutcomeClassTag :ClassTag[Outcome[Nothing]] = ClassTag.AnyRef.castParam[Outcome[Nothing]]
		@inline final implicit def PillClassTag    :ClassTag[Pill[Nothing, Nothing]] =
			ClassTag.AnyRef.castParam[Pill[Nothing, Nothing]]
	}



	/** A companion and factory of $Opt, a very lightweight alternative to [[Option]].
	  * @see [[net.noresttherein.sugar.vars.Opt.One]]
	  * @see [[scala.None]]
	  *
	  * @define Opt  [[net.noresttherein.sugar.vars.Opt! Opt]]
	  * @define One  [[net.noresttherein.sugar.vars.Opt.One$ One]]
	  * @define None [[scala.None None]]
	  */
	@SerialVersionUID(Ver)
	object Opt { //Synonyms: Opt; Maybe/Yes/No; Hope/Lucky/NoLuck; Wish
		/** Creates a $Done instance wrapping the value unless it is null, in which case it returns $None.
		  * This call will not box the value unless it is already an instance of `Opt`. */
		def apply[A](value :A) :Opt[A] = value match {
			case null             => None
			case None | _ :One[_] => new One(value).asInstanceOf[Opt[A]]
			case _                => value.asInstanceOf[Opt[A]]
		}

		/** Converts the given `Option[T]` into a lighter `Opt[T]` which is erased at runtime. */
		@inline def some_?[A](value :Option[A]) :Opt[A] = value match {
			case Some(a) => One(a)
			case _       => None
		}

		/** Converts the given `Maybe[T]` into an `Opt[T]`. */
		@inline def yes_?[A](value :Maybe[A]) :Opt[A] = value.toOpt

		/** Converts the given `Unsure[T]` into an `Opt[T]`, erased at runtime. */
		@inline def sure_?[A](value :Unsure[A]) :Opt[A] = value.toOpt

		/** Converts the given `Nullable[T]` into an `Opt[T]`, erased at runtime. */
		@inline def nonNull_?[A <: AnyRef](value :Nullable[A]) :Opt[A] = value.toOpt

		/** Converts the given `Option[T]` into a lighter `Opt[T]` which is erased at runtime. */
		@inline def fromOption[A](value :Option[A]) :Opt[A] = value match {
			case Some(a) => One(a)
			case _       => None
		}

		/** Converts the given `Maybe[T]` into an `Opt[T]`. */
		@inline def fromMaybe[A](value :Maybe[A]) :Opt[A] = value.toOpt

		/** Converts the given `Unsure[T]` into an `Opt[T]`, erased at runtime. */
		@inline def fromUnsure[A](value :Unsure[A]) :Opt[A] = value match {
			case Sure(a) => One(a)
			case _       => None
		}

		/** Converts the given `Nullable[T]` into an `Opt[T]`, erased at runtime. */
		@inline def fromNullable[A <: AnyRef](value :Nullable[A]) :Opt[A] = value.toOpt

		/** Returns [[scala.None None]] - an empty `Opt`. */
		@inline final def empty[T] :Opt[T] = None

		/** When a given condition is true, evaluates `a` argument and returns `One(a).`
		  * When the condition is false, `a` is not evaluated and `None` is returned. */
		@inline def when[A](cond: Boolean)(a: => A): Opt[A] =
			if (cond) One(a) else None

		/** Unless a given condition is true, this will evaluate `a` argument and return `One(a)`.
		  * Otherwise, `a` is not evaluated and `None` is returned. */
		@inline def unless[A](cond: Boolean)(a: => A): Opt[A] =
			if (!cond) One(a) else None

		/** Executes the given lazy expression in a `try-catch` block, returning `None` in case
		  * any exception is caught. Otherwise, the value is returned in an `One` instance as normal. */
		@inline def guard[A](a: => A) :Opt[A] =
			try One(a) catch {
				case _ :Exception => None
			}

		/** Applies the given function to the second argument in a `try-catch` block, returning `None` in case
		  * any exception is caught. Otherwise, the value is returned in an `One` instance as normal. */
		@inline def guard[A, B](f: A => B)(a :A) :Opt[B] =
			try One(f(a)) catch {
				case _ :Exception => None
			}

		/** Returns the first argument in `One` if it satisfies the predicate `p`.
		  * @return `One(value).filter(p)`. */
		@inline def satisfying[A](value :A)(p :A => Boolean) :Opt[A] =
			if (p(value)) One(value) else None

		/** Extracts the value from the `Opt`, if available.
		  * @return `One.unapply(value)`. */
		@inline final def unapply[A](value :Opt[A]) :Maybe[A] = One.unapply(value)

		/** A factory of 'full' (`Some`) instances of `Opt`.  */
		@SerialVersionUID(Ver) //Synonyms: Extant, Stuff, Lucky, One, Given
		object One { //consider: moving out to package vars
			def apply[A](value :A) :Opt[A] = value match {
				case None | _ :One[_] => new One(value).asInstanceOf[Opt[A]]
				case _                => value.asInstanceOf[Opt[A]]
			}

			def unapply[A](opt :Opt[A]) :Maybe[A] = opt match {
				case None                      => No
				case exists :One[A @unchecked] => Yes(exists.value)
				case _                         => Yes(opt.asInstanceOf[A])
			}
		}

		//We don't want anyone to manually wrap a value, as it will not symmetrically equal an erased Opt.
		@SerialVersionUID(Ver)
		private[vars] class One[+A](val value :A) extends Serializable {
			override def equals(that :Any) :Boolean = that match {
				case exists :One[_] => value == exists.value
				case _              => false
			}
			override def hashCode :Int = value.hashCode
			override def toString :String = "One(" + value + ")"
		}



		/** Optional implicit conversions to/from `Maybe`, `Option` and `Iterable`.
		  * They may result in boxing in some circumstance and are placed here for explicit importing.
		  * Additionally, importing [[net.noresttherein.sugar.vars.Opt.conversions.anyOne anyOne]] enables
		  * conversion lifting any value of type `T` to `Opt[T]`.
		  */
		@SerialVersionUID(Ver)
		object conversions {
			/** An implicit conversion that converts a $Opt to an iterable value. */
			@inline implicit def OptToIterable[A](opt :Opt[A]) :Iterable[A] = opt match {
				case One(v) => v::Nil
				case _      => Nil
			}

			/** An implicit conversion from an `Opt[A]` to an `Option[A]`.
			  * The results are cached, so repeated conversions of the same instance do not result in boxing.
			  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit. */
			@inline implicit def OptToOption[T](opt :Opt[T]) :Option[T] = opt match {
				case One(v) => Some(v)
				case _      => None
			}

			/** A nomen omen optional implicit conversion of an `Option[A]` to an `Opt[A]`.
			  * @see [[net.noresttherein.sugar.optional.extensions.OptionExtension]] */
			//consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
			@inline implicit def OptionToOpt[A](opt :Option[A]) :Opt[A] = some_?(opt)

			/** An implicit conversion from an `Opt[A]` to a `Maybe[A]`.
			  * The results are cached, so repeated conversions of the same instance do not result in boxing.
			  * Still, this conversion isn't placed in the implicit search scope for those preferring to be explicit. */
			@inline implicit def OptToMaybe[T](value :Opt[T]) :Maybe[T] = value match {
				case One(v) => Yes(v)
				case _      => No
			}

			/** A nomen omen optional implicit conversion of a `Maybe[A]` to an `Opt[A]`. */
			//consider: placing this also in vars.extensions (or vars.implicits/vars.imports)
			@inline implicit def MaybeToOpt[A](opt :Maybe[A]) :Opt[A] = yes_?(opt)

			@inline implicit def OptToNullable[A <: AnyRef](opt :Opt[A]) :Nullable[A] = opt match {
				case One(v) => NonNull(v)
				case _      => Null
			}
			@inline def NullableToOpt[A <: AnyRef](value :Nullable[A]) :Opt[A] = nonNull_?(value)

			//consider: making it available by default.
			/** Wraps any object in a [[net.noresttherein.sugar.vars.Opt Opt]] monad. */
			@inline implicit def anyOne[A](any :A) :Opt[A] = One(any)
		}


		/** Importing the contents of this object replaces all usage of [[Option]]/[[Some]]/[[None]] in the scope with
		  * [[net.noresttherein.sugar.vars.Opt Opt]]/[[net.noresttherein.sugar.vars.Opt.One One]]/[[scala.None None]].
		  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
		  * which allow seamless interoperability with standard Scala APIs.
		  *
		  * While the idea of using it in production code should be carefully reconsidered,
		  * it is useful for quickly checking during profiling what impact using `Option` vs `Opt` has.
		  *
		  * Other files which reference classes defined in the import's scope may also need to be modified in order
		  * to comply with changed interfaces.
		  */
		@SerialVersionUID(Ver)
		object OptAsOption {
			type Option[T] = Opt[T]

			val Option = Opt
			val Some   = One

			//same names as in implicits so if both are imported one shadows the other
			@inline implicit def OptFromOption[T](opt :Opt[T]) :scala.Option[T] = opt.option
			@inline implicit def OptionToOpt[T](opt :scala.Option[T]) :Opt[T] = fromOption(opt)
		}



		private[vars] def unzip[A, B](pair :Opt[(A, B)]) :(Opt[A], Opt[B]) =
			if (pair.asInstanceOf[AnyRef] eq None)
				unzip2None
			else {
				val (a, b) = pair.get
				(One(a), One(b))
			}

		private[vars] def unzip3[A, B, C](triplet :Opt[(A, B, C)]) :(Opt[A], Opt[B], Opt[C]) =
			if (triplet.asInstanceOf[AnyRef] eq None)
				unzip3None
			else {
				val (a, b, c) = triplet.get
				(One(a), One(b), One(c))
			}

		private val unzip2None = (None, None)
		private val unzip3None = (None, None, None)
	}




	//Do these four objects even serve any purpose?
	/** Matching pattern extracting value from [[net.noresttherein.sugar.vars.Opt Opt]]
	  * or any [[net.noresttherein.sugar.vars.Ref Ref]] value
	  * (in particular option-like types like [[net.noresttherein.sugar.vars.Maybe Maybe]],
	  * [[net.noresttherein.sugar.vars.Unsure Unsure]], [[net.noresttherein.sugar.vars.Nullable Nullable]]).
	  * The value is the one returned by
	  * [[net.noresttherein.sugar.vars.Ref.get get]]/[[net.noresttherein.sugar.vars.Ref.toMaybe toMaybe]].
	  */
	@SerialVersionUID(Ver)
	object Defined {
		@inline def unapply[T](ref :Opt[T]) :Maybe[T] = ref.toMaybe
		@inline def unapply[T](ref :Ref[T]) :Maybe[T] = ref.toMaybe
		@inline def unapply[T](ref :Option[T]) :Maybe[T] = Maybe.fromOption(ref)
	}

	/** Matching pattern which matches undefined [[net.noresttherein.sugar.vars.Ref Ref]]
	  * or [[net.noresttherein.sugar.vars.Opt Opt]].
	  * It returns `true` ''iff'' [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]] returns `false`.
	  */
	@SerialVersionUID(Ver)
	object Undefined {
		@inline def unapply(ref :Opt[_]) :Boolean = ref == None
		@inline def unapply(ref :Ref[_]) :Boolean = !ref.isDefined
		@inline def unapply(ref :Option[_]) :Boolean = ref == None
	}

	/** Matching pattern for [[net.noresttherein.sugar.vars.Ref Ref]] and [[net.noresttherein.sugar.vars.Opt Opt]]
	  *  instances holding a [[net.noresttherein.sugar.vars.Ref.isDefinite definite]] value.
	  */
	@SerialVersionUID(Ver)
	object Definite {
		/** Returns `ref.`[[net.noresttherein.sugar.vars.OptExtension.toMaybe toMaybe]]. */
		@inline def unapply[T](ref :Opt[T]) :Maybe[T] = ref.toMaybe
		/** Returns `ref.`[[net.noresttherein.sugar.vars.Ref.toMaybe toMaybe]]. */
		@inline def unapply[T](ref :Ref[T]) :Maybe[T] = ref.toMaybe
		/** Returns the value if `ref` is a `Some`. */
		@inline def unapply[T](ref :Option[T]) :Maybe[T] = Maybe.fromOption(ref)
	}

	/** A pattern matching [[net.noresttherein.sugar.vars.Ref Ref]] and [[net.noresttherein.sugar.vars.Opt Opt]]
	  * instances without a [[net.noresttherein.sugar.vars.Ref.isDefinite definite]] value.
	  */
	@SerialVersionUID(Ver)
	object Indefinite {
		/** Returns `!ref.`[[net.noresttherein.sugar.vars.OptExtension.isDefinite isDefinite]]. */
		@inline def unapply(ref :Opt[_]) :Boolean = ref eq None
		/** Returns `!ref.`[[net.noresttherein.sugar.vars.Ref.isDefinite isDefinite]]. */
		@inline def unapply(ref :Ref[_]) :Boolean = !ref.isDefinite
		/** Returns true for `None`. */
		@inline def unapply(ref :Option[_]) :Boolean = ref eq None
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


		/** A factory and matching pattern for [[net.noresttherein.sugar.vars.Pill! Pill]] instances
		  * representing a successful result of a computation.
		  */
		@SerialVersionUID(Ver)
		object Blue {
			def apply[B](value :B) :Pill[Nothing, B] = value match {
				case _ :Red[_] | _ :Blue[_] => new Blue(value).asInstanceOf[Pill[Nothing, B]]
				case _                      => value.asInstanceOf[Pill[Nothing, B]]
			}

			def unapply[B](alt :Pill[Any, B]) :Maybe[B] = alt match {
				case _    :Red[_]             => No
				case blue :Blue[B @unchecked] => Yes(blue.value)
				case _                        => Yes(alt.asInstanceOf[B])
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

			def unapply[R](alt :Pill[R, Any]) :Maybe[R] = alt match {
				case red :Red[R @unchecked] => Yes(red.value)
				case _                      => No
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
		  * [[net.noresttherein.sugar.vars.Outcome Outcome]]`.`[[net.noresttherein.sugar.vars.Outcome.conversions conversions]],
		  * importing from both objects all definitions will lead to implicit conversion ambiguity.
		  */
		@SerialVersionUID(Ver)
		object conversions {
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
	object Outcome { //other names: Watchman; Custodian; Return; Result; Outcome; Attempt; Action/Act; Do; Compute; Upshot; Issue; Opt

		/** Executes the given expression, and returns it in a $Done if it is not null, or $Failed otherwise.
		  * All exceptions are caught and returned as `Failed(e)`.
		  */
		@inline def apply[O](value : => O) :Outcome[O] =
			try {
				val v = value
				if (v == null) Failed("null") else Done(v)
			} catch {
				case e :Exception => Failed(e)
			}

		/** Executes the given lazy expression in a `try-catch` block, returning `Failed` in case
		  * any exception is caught. Otherwise, the value is returned as a `Done` instance as normal. */
		@inline def guard[A](a : => A) :Outcome[A] =
			try Done(a) catch {
				case e :Exception => Failed(e)
			}

		/** Applies the given function to the second argument in a `try-catch` block, returning `Failed` in case
		  * any exception is caught. Otherwise, the result is returned as a `Done` instance as normal. */
		//While swapped parameter order would make more sense, it would clash with the (a: => A) overload.
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
			def unapply[T](exam :Outcome[T]) :Maybe[T] = (exam :Any) match {
				case _    :Throwable          => No
				case pass :Done[T @unchecked] => Yes(pass.value)
				case _                        => Yes(exam.asInstanceOf[T])
			}
		}

		/** A reified `Done` case used when `value` is a `Failed` or `Done` instance.
		  * Used to differentiate between `Done(Failed)` and `Failed`.
		  */
		@SerialVersionUID(Ver)
		private[vars] class Done[+T](val value :T) extends Serializable {
			//It might look like equals doesn't handle the equality between reified and erased Done,
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
			def unapply(exam :Outcome[Any]) :Maybe[Throwable] = exam match {
				case fail :Throwable => Yes(fail)
				case _               => No
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

			@inline implicit def everythingIsDone[T](value :T) :Outcome[T] = Done(value)
		}
	}



	/** Factories/match patterns for [[net.noresttherein.sugar.vars.Term! Term]]. */
	@SerialVersionUID(Ver)
	case object Term {
		/** A ''by-name'' expression. This is equivalent to [[net.noresttherein.sugar.vars.Term.Expression Expression]],
		  * but accepts a lazy expression `=> X` rather than `() => X`, and similarly evaluates the expression
		  * on extraction when used as a match pattern. */
		@SerialVersionUID(Ver)
		case object ByName {
			/** A [[net.noresttherein.sugar.vars.Term! Term]] which will evaluate the argument expression
			  * each time its value is requested. */
			@inline def apply[X](init: => X) :Term[X] = Expression(() => init)

			/** If `term` is an [[net.noresttherein.sugar.vars.Term.Expression Expression]], evaluate it
			  * and return its value. Does not match [[net.noresttherein.sugar.vars.Term.Value Value]] cases. */
			@inline def unapply[X](term :Term[X]) :Maybe[X] = term match {
				case init :(() => X @unchecked) => Yes(init()) //acquireFence(); Yes(init())
				case _                          => No
			}
		}

		/** A [[net.noresttherein.sugar.vars.Term! Term]] represented by a (lazy) Scala expression.
		  * Wraps and unwraps `() => X` to/from `Term[X]`.
		  */
		@SerialVersionUID(Ver)
		case object Expression {
			/** A [[net.noresttherein.sugar.vars.Term! Term]] which will evaluate the argument expression
			  * each time its value is requested. */
			@inline def of[X](init: => X) :Term[X] = apply(() => init)

			/** A term which executes the argument `init()` each time its value is requested. */
			@inline def apply[X](init :() => X) :Term[X] = {
//				releaseFence()
				init.asInstanceOf[Term[X]]
			}

			/** An expression term always throwing a `NoSuchElementException`. */
			val nothing :Term[Nothing] = Expression(() => noSuch_!)

			/** Matches terms created by this object (and [[net.noresttherein.sugar.vars.Term.ByName ByName]]),
			  * extracting the expression passed as the argument on its creation. */
			@inline def unapply[X](v :Term[X]) :Maybe[() => X] = v match {
				case init :(() => X) @unchecked => Yes(init) //acquireFence(); Yes(init)
				case _                          => No
			}
		}

		/** A [[net.noresttherein.sugar.vars.Term! Term]] represented by a constant value.
		  * Wraps and unwraps `X` to/from `Term[X]`.
		  */
		@SerialVersionUID(Ver)
		case object Value {
			/** A `Term` of a constant value. If `value :() => Any`, then the returned instance will be boxed. */
			@inline def apply[X](value :X) :Term[X] = value match {
				case _ :(() => Any @unchecked) => /*releaseFence();*/ new Value(value).asInstanceOf[Term[X]]
				case _                         => /*releaseFence();*/ value.asInstanceOf[Term[X]]
			}
			/** Matches terms created by this object, extracting their value. */
			@inline def unapply[X](v :Term[X]) :Maybe[X] = v match {
				case _ :(() => Any)         => No
				case v :Value[X @unchecked] => Yes(v.value)
				case v :X @unchecked        => Yes(v) //acquireFence(); Yes(v)
			}
		}

		@SerialVersionUID(Ver)
		private[sugar] case class Value[+X](value :X)

		/** Matches every term, extracting its value.
		  * If the term is an [[net.noresttherein.sugar.vars.Term.Expression by-name]] expression, it is evaluated. */
		def unapply[X](term :Term[X]) :Maybe[X] = term match {
			case init :(() => X @unchecked) => Yes(init())
			case v :Value[X @unchecked]     => Yes(v.value)
			case v :X @unchecked            => Yes(v)
		}
	}



	@SerialVersionUID(Ver)
	class IntPair private (private val bits :Long) extends AnyVal with Serializable {
		def _1 :Int = (bits >> 32).toInt
		def _2 :Int = bits.toInt
		def toTuple :(Int, Int) = (_1, _2)
	}

	object IntPair {
		def apply(first :Int, second :Int) :IntPair = new IntPair(first.toLong << 32 | second & 0xffffffffL)
		def unapply(pair :IntPair) :Opt[(Int, Int)] = One(pair.toTuple)
	}
}
