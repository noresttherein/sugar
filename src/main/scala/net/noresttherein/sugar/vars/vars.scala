package net.noresttherein.sugar

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

import net.noresttherein.sugar.vars.Checked.Failed
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Pill.{Blue, Red}
import net.noresttherein.sugar.vars.Potential.{Existent, Inexistent, NonExistent, WithFilter}




/**
  * @define Potential [[net.noresttherein.sugar.vars.Potential! Potential]]
  * @define Existent [[net.noresttherein.sugar.vars.Existent$ Existent]]
  * @define Inexistent [[net.noresttherein.sugar.vars.Inexistent$ Inexistent]]
  * @define Checked [[net.noresttherein.sugar.vars.Checked! Checked]]
  * @define Passed [[net.noresttherein.sugar.vars.Checked.Passed Passed]]
  * @define Failed [[net.noresttherein.sugar.vars.Checked.Failed Failed]]
  * @define Pill [[net.noresttherein.sugar.vars.Pill! Pill]]
  * @define Blue [[net.noresttherein.sugar.vars.Pill.Blue$ Blue]]
  * @define Red  [[net.noresttherein.sugar.vars.Pill.Red$ Red]]
  */
package object vars {

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

	/** An erased variant of [[scala.Option]], with API defined by extension methods
	  * in [[net.noresttherein.sugar.vars.PotentialExtension PotentialExtension]].
	  * A `Potential[A]` can be have three forms:
	  *   1. $Inexistent, an [[net.noresttherein.sugar.vars.PotentialExtension.isEmpty empty]] instance corresponding
	  *      to [[scala.None]];
	  *   1. $Existent[A], erased to `A` (which may be a boxed version of a value type, both inbuilt or a value class);
	  *   1. $Existent[A], defensively wrapped - used to differentiate `Existent(Inexistent)` from `Inexistent`.
	  * Outside of nesting `Potential` instances within each other or explicitly calling
	  * [[net.noresttherein.sugar.vars.PotentialExtension.reify reify]], no boxing takes place for reference types
	  * under any circumstances, in particular when creating an `Array[Potential[A]]` or `Seq[Potential[A]]`,
	  * using it as an argument or return value of a function, or, in general, substituting it for an abstract type
	  * (including type parameters). The type is not `@specialized`, so boxing of value types, as well as lifting
	  * of value classes to their reference representations, will still occur: all values and arguments
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
	  */
	type Potential[+A]// >: Existent[A]

	/** The API of $Potential in the form of extension methods. */
	implicit class PotentialExtension[A](private val self :Potential[A]) extends AnyVal {

		/** Tests if this `Potential` does not contain a value
		  * (is equal to [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]]). */
		@inline def isEmpty: Boolean = self.asInstanceOf[AnyRef] eq NonExistent

		/** Tests if this `Potential` is defined. If true, `get` will not throw an exception. */
		@inline def nonEmpty: Boolean = self.asInstanceOf[AnyRef] ne NonExistent

		/** Tests if this `Potential` contains a value. This is the same as `nonEmpty`. */
		@inline def isDefined: Boolean = self.asInstanceOf[AnyRef] ne NonExistent

//		/** Lifts an erased, [[net.noresttherein.sugar.vars.Potential.Existent existent]] value to
//		  * its wrapped representation which will fail type matches in the form of `_ :A`.
//		  * This boxing may be useful when there is a need to match this object against types unrelated
//		  * to `Potential`, or as a defensive measure when passing it as an argument or returning as `Any`/`AnyRef`.
//		  * This essentially duplicates the behaviour of value classes, which are lifted to their `AnyRef`
//		  * representation whenever used in a place of an abstract type (such as an element of a collection).
//		  */
//		@inline def reify :Potential[A] = (self :Any) match {
//			case NonExistent | _ :Existent[_] => self
//			case value => new Existent(value).asInstanceOf[Potential[A]]
//		}

		/** Forces extraction of the value.
		  * @return contained value, if one exists.
		  * @throws NoSuchElementException if this `Potential` is empty. */
		@inline def get :A = (self :Any) match {
			case NonExistent => throw new NoSuchElementException("Inexistent.get")
			case exists :Existent[A @unchecked] => exists.value
			case _ => self.asInstanceOf[A]
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
		@inline def fold[B](ifEmpty: => B)(f: A => B): B =
			if (self.asInstanceOf[AnyRef] eq NonExistent) ifEmpty else f(get)

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
			case it :Iterable[O] =>
				if (it.isEmpty || !it.toSet(get)) self
				else Inexistent
			case _ if that.iterator.toSet(get) => Inexistent
			case _ => self
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
		  *  a variant for use in for-comprehensions. */
		@inline def withFilter(p :A => Boolean) :WithFilter[A] = new WithFilter[A](self, p)


		/** Tests if this `Potential` is not empty and its value is equal to the given argument. */
		@inline def contains[O >: A](o :O): Boolean = (self.asInstanceOf[AnyRef] ne NonExistent) && get == o

		/** Tests if this `Potential` is not empty and its value satisfies the given predicate. */
		@inline def exists(p :A => Boolean): Boolean = (self.asInstanceOf[AnyRef] ne NonExistent) && p(get)

		/** Tests if this `Potential` contains no value or its value satisfies the given predicate. */
		@inline def forall(p :A => Boolean): Boolean = (self.asInstanceOf[AnyRef] eq NonExistent) || p(get)

		/** Executes the given block for this `PotentiaL`s value if it is not empty. */
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


		/** Returns a `PotentiaL` formed from the contents of `this` and `that` by combining the corresponding elements
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

		//conflict with (this :Opt[A]).opt, which does the same thing
		@inline def opt :Opt[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) Lack else Got(get)

		/** Conversion to standard Scala [[scala.Option]].
		  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
		@inline def ? :Option[A] = if (self.asInstanceOf[AnyRef] eq NonExistent) None else Some(get)

		/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
		  * is specialized for value types, this type is not, and the result will not be specialized. Neither will it
		  * require boxing though, as any value type was promoted to a reference wrapper when creating this `Potential`.
		  */
		@inline def unsure :Unsure[A] =
			if (self.asInstanceOf[AnyRef] eq NonExistent)
				Blank
			else {
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
	}




	/** An alternative wrapper over one of two values: either `Pill.`${Red}`[Red]`, or `Pill`.${Blue}[Blue],
	  * with the latter type not visible to the application.
	  * It is a variant of `Either` optimized for usage in which one of the cases (corresponding to `Right` in `Either`)
	  * is much more common than the other (i.e., '`Left`'), such as in popular monadic usage of `Right`
	  * ($Blue in this case) for a result of a computation, while reserving `Left` ($Red) for error information
	  * passed unchanged when [[net.noresttherein.sugar.vars.PillExtension.map mapping]]
	  * or [[net.noresttherein.sugar.vars.PillExtension flat mapping]] a `Pill`.
	  *
	  * In most situations, the `Blue[B]` case is represented as an erased value of `B`, meaning that
	  * monadic usage of `Pill` doesn't involve create a new object with each `map`/`flatMap` operation.
	  * This is changed only if `Pill` is nested, i.e. a `Blue(Blue(_))` or `Blue(Red(_))` instance is
	  * created. Values of `Red` are represented normally, by wrapping them in an instance of
	  * [[net.noresttherein.sugar.vars.Pill.Red! Red]], but in the intended usage pattern, are passed unchanged
	  * by monadic operations, also avoiding creation of a new instance.
	  *
	  * Due to this design, the `Blue[_]` case cannot be matched by type, but only by the
	  * [[net.noresttherein.sugar.vars.Pill.Blue.unapply Blue.unapply]] match pattern. This is also the recommended
	  * practice for `Red[_]`.
	  *
	  * The interface of this type is provided as extension methods by
	  * [[net.noresttherein.sugar.vars.PillExtension PillExtension]].
	  */
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

//		/** If this instance is represented as an erased $Blue case, lift it to its non-erased wrapper, otherwise
//		  * leave it as it is. This may be useful if this instance is going expected to be pattern matched
//		  * with something else than `Blue(_)` or ${Red}`(_)`, in order to avoid matching patterns for its contents.
//		  * This can be used as a defensive measure if this value were to be passed as an `Any`/`AnyRef` argument
//		  * (or returned as such) to not controlled code.
//		  */
//		@inline def reify :Pill[R, B] = self match {
//			case _ :Red[_] | _ :Blue[_] => self
//			case _ => new Blue(self).asInstanceOf[Pill[R, B]]
//		}

		/** Swaps the meaning of $Red and $Blue: a `Red(r)` becomes `Blue(r)`, while `Blue(b)` becomes `Red(b)`. */
		@inline def swap :Pill[B, R] = self match {
			case red :Red[R @unchecked] => Blue(red.value)
			case _ => Red(get)
		}
		/** Forces extraction of the $Blue result.
		  * @return contained value, if `this` is $Blue.
		  * @throws NoSuchElementException if this $Pill is $Red. */
		def get :B = (self :Any) match {
			case red  :Red[_] => throw new NoSuchElementException(red.toString)
			case blue :Blue[B @unchecked] => blue.value
			case _ => self.asInstanceOf[B]
		}
		/** Returns the result if it is $Blue, or the lazily computed alternative passed as an argument otherwise. */
		@inline def getOrElse[B1 >: B](or: => B1) :B1 = self match {
			case _ :Red[_] => or
			case _ => get
		}
		/** Similarly to [[net.noresttherein.sugar.vars.PillExtension.getOrElse getOrElse]], returns the result
		  * of this $Pill if it is $Blue, or `alt` if it is $Red. The difference is that the alternative value
		  * is not lazily computed and guarantees no closure will be created,
		  * at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is a failure. */
		@inline def orDefault[B1 >: B](or :B1) :B1 = self match {
			case _ :Red[_] => or
			case _ => get
		}
		/** Assuming that `A` is a nullable type, return `null` if this $Pill is $Red,
		  * or a wrapped result of $Blue otherwise. */
		@inline def orNull[B1 >: B](implicit isNullable :B1 <:< Null) :B1 = self match {
			case _ :Red[_] => null.asInstanceOf[B1]
			case _ => get
		}

		/** Returns the result if it is $Blue, or throws an exception given as the type parameter with
		  * `(this :$Red[R]).value.toString` as the error message.
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
		  * @see [[net.noresttherein.sugar.vars.CheckedExtension.orNoSuch orNoSuch]]
		  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
		@inline def orThrow[E <: Throwable :ClassTag] :B = self match {
			case red :Red[_] => raise[E](red.value.toString)
			case _ => get
		}
		/** Gets the element in this $Pill if it is $Blue, or throws a [[NoSuchElementException]]
		  * with [[net.noresttherein.sugar.vars.Pill.Red.value value]]`.toString` as the error message if $Red.
		  * @see [[net.noresttherein.sugar.vars.CheckedExtension.orThrow orThrow]] */
		@inline def orNoSuch :B = self match {
			case red :Red[_] => throw new NoSuchElementException(red.value.toString)
			case _ => get
		}
		/** Gets the element in this $Pill if it is $Blue, or throws an [[IllegalArgumentException]]
		  * with [[net.noresttherein.sugar.vars.Pill.Red.value value]]`.toString` as the error message if $Red.
		  * @see [[net.noresttherein.sugar.vars.CheckedExtension.orThrow orThrow]] */
		@inline def orIllegal :B = self match {
			case red :Red[_] => throw new IllegalArgumentException(red.value.toString)
			case _ => get
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
			case _ => self
		}

		/** Similarly to [[net.noresttherein.sugar.vars.PillExtension.orElse orElse]], returns this $Pill
		  * if it is $Blue, or `alt` otherwise. The difference is that the alternative value is not lazily computed
		  * and guarantees no closure would be be created, at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def ifRed[R1 >: R, B1 >: B](or :Pill[R1, B1]) :Pill[R1, B1] = self match {
			case _ :Red[_] => or
			case _ => self
		}



		/** Returns $Blue with the result of applying the given function to the result of this $Pill,
		  * or this instance ''iff'' it is $Failed. */
		@inline def map[O](f :B => O) :Pill[R, O] = self match {
			case red :Red[R @unchecked] => red
			case _ => Blue(f(get))
		}
		/** Applies the given function to the value of this $Pill if it is $Blue, or returns `alt`
		  * if it is $Red. Equivalent to `this map f getOrElse alternative`, but in one step. */
		@inline def mapOrElse[O](f :B => O, or: => O) :O = self match {
			case _ :Red[_] => or
			case _ => f(get)
		}
		/** Applies the first function argument to this `Pill`'s value if it is $Red,
		  * or the second function if it is $Blue. */
		@inline def fold[O](ifRed :R => O, ifBlue :B => O) :O = self match {
			case red :Red[R @unchecked] => ifRed(red.value)
			case _ => ifBlue(get)
		}
		/** Returns the result of applying the given function to the value of this $Pill if it is $Blue,
		  * or `this` if it is $Red. */
		@inline def flatMap[R1 >: R, O](f :B => Pill[R1, O]) :Pill[R1, O] = self match {
			case red :Red[R @unchecked] => red
			case _ => f(get)
		}
		/** Flattens `Pill[R, Pill[R, O]]` to a single `Pill[R, O]`. */
		@inline def flatten[R1 >: R, O](implicit isAlt :B <:< Pill[R1, O]) :Pill[R1, O] =
			self match {
				case red :Red[R @unchecked] => red
				case _ => get
			}
		/** Flattens `Pill[Pill[O, B], B]]` to `Pill[O, B]`: returns the value of this $Pill if it is $Red,
		  * or itself if it is $Blue. This is similar to [[net.noresttherein.sugar.vars.PillExtension.flatten flatten]],
		  * but works on the `Red` (`Left`) side.
		  */
		@inline def joinRed[R1 >: R, B1 >: B, O](implicit redIsAlt :R1 <:< Pill[O, B1]) :Pill[O, B1] =
			self match {
				case red :Red[R @unchecked] => red.value
				case _ => self.asInstanceOf[Pill[O, B1]]
			}
		/** Flattens `Pill[R, Pill[R, O]]` to `Pill[R, O]`: returns the value of this $Pill if it is $Red,
		  * or itself if it is $Blue. This is equivalent to [[net.noresttherein.sugar.vars.PillExtension.flatten flatten]],
		  * but allows to widen the type of the red pill.
		  */
		@inline def joinBlue[R1 >: R, B1 >: B, O](implicit blueIsAlt :B1 <:< Pill[R1, O]) :Pill[R1, O] =
			self match {
				case red :Red[R @unchecked] => red
				case _ => get
			}
		/** Same as [[net.noresttherein.sugar.vars.PillExtension.joinRed joinRed]]. Exists for compatibility with `Either`. */
		@inline def joinLeft[R1 >: R, B1 >: B, O](implicit redIsAlt :R1 <:< Pill[O, B1]) :Pill[O, B1] = joinRed
		/** Same as [[net.noresttherein.sugar.vars.PillExtension.joinBlue joinBlue]]. Exists for compatibility with `Either`. */
		@inline def joinRight[R1 >: R, B1 >: B, O](implicit blueIsAlt :B1 <:< Pill[R1, O]) :Pill[R1, O] = joinBlue

		/** Returns `this` if $Blue and `p(get)` holds, or ${Red}(red) otherwise. */
		def filterOrElse[R1 >: R](p: B => Boolean, red: => R1): Pill[R1, B] = self match {
			case _ :Red[_] => Red(red)
			case _ if p(get) => self
			case _ => Red(red)
		}

		/** Tests if this $Pill is $Blue with a result equal to the given argument. */
		@inline def contains[B1 >: B](o :B1): Boolean = o match {
			case _ :Red[_] => false
			case _ => get == o
		}
		/** Tests if this $Pill is $Blue with a result satisfying the given predicate. */
		@inline def exists(p :B => Boolean): Boolean = self match {
			case _ :Red[_] => false
			case _ => p(get)
		}
		/** Tests if this $Pill $Red or $Blue with a value not satisfying the given predicate. */
		@inline def forall(p :B => Boolean): Boolean = self match {
			case _ :Red[_] => true
			case _ => p(get)
		}
		/** Executes the given block for this $Pill's value if it is $Blue. */
		@inline def foreach[O](f :B => O) :Unit =
			if (!self.isInstanceOf[Red[_]])
				f(get)

		/** Converts this value to an `Opt` if it is $Blue, losing the information by replacing
		  * $Red with [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
		  */
		@inline def opt :Opt[B] = self match {
			case _ :Red[_] => Lack
			case _ => Got(get)
		}
		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isBlue` or `None` otherwise. */
		@inline def ? :Option[B] = self match {
			case _ :Red[_] => None
			case _ => Some(get)
		}
		/** Converts this value to a `Potential` if it is $Blue, losing the information by replacing
		  * $Red with [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]].
		  */
		@inline def toPotential :Potential[B] = self match {
			case _ :Red[_] => Inexistent
			case _ => Existent(get)
		}
		/** Conversion to an `Unsure` carrying the value of this instance if it is $Blue.
		  * Note that the result will not be `specialized` for value types, but neither will it require boxing,
		  * as $Blue already contains boxed values.
		  */
		@inline def unsure :Unsure[B] = self match {
			case _ :Red[_] => Missing
			case _ => Sure(get)
		}
		/** Conversion to [[scala.Either]], with $Red returned as [[scala.Right Right]] and $Blue as [[scala.Left Left]].
		  */
		@inline def toEither :Either[R, B] = self match {
			case red :Red[R @unchecked] => Left(red.value)
			case _ => Right(get)
		}
		/** Standard conversion to [[scala.Option]].
		  * @return `Some(this.get)` if `this.isBlue` or `None` otherwise. */
		@inline def toOption :Option[B] = self match {
			case _ :Red[_] => None
			case _ => Some(get)
		}
		/** Returns a [[Seq]] containing `this.get` (if $Blue), or an empty `Seq` otherwise. */
		@inline def toSeq :Seq[B] = self match {
			case _ :Red[_] => Nil
			case _ => get::Nil
		}
		@inline def toTry(implicit ev :R <:< Throwable) :Try[B] = self match {
			case red :Red[R @unchecked] => Failure(red.value)
			case _ => Success(get)
		}
	}



	/** A variant of a non boxing `Either`, with instances of two categories: $Passed`[A]` containing a value of `A`,
	  * or $Failed with an error message. In order to avoid the creation of `Right` (successful) instance
	  * each time in a monadic composition of chained operations on an `Either`, a `Passed` is encoded
	  * as its erased (and boxed) contents, i.e. the value itself. A `Failed` corresponds to `Left[String]`.
	  * This solution brings two limitations:
	  *   1. Nesting `Checked` without another `Checked` must resort to boxing in order to differentiate between
	  *      `Passed(Failed)` and `Failed`. This is however transparent to the application.
	  *      2. No `Passed` (i.e. `Right`) type exist, because it is erased,
	  *      and construction and checking must happen using `apply` and `unapply` methods of singleton
	  *      object $Passed.
	  *      [[net.noresttherein.sugar.vars.CheckedExtension Extension]] methods are provided, mirroring the relevant
	  *      part of functionality of `Either` and `Option`.
	  */
	type Checked[+A] = Pill[String, A]

	/** Extension methods providing the full interface of $Checked. */
	implicit class CheckedExtension[A](private val self :Checked[A]) extends AnyVal {
		/** Checks if this $Checked is $Passed (successful) result containing a value. */
		@inline def isPassed :Boolean = !self.isInstanceOf[Failed @unchecked]
		/** Checks if this $Checked is $Failed containing an error message. */
		@inline def isFailed :Boolean = self.isInstanceOf[Failed @unchecked]

//		/** Forces extraction of the result.
//		  * @return contained value, if `this` is $Passed.
//		  * @throws NoSuchElementException if this $Checked is $Failed. */
//		def get :A = (self :Any) match {
//			case fail :Failed => throw new NoSuchElementException(fail.toString)
//			case pass :Passed[A @unchecked] => pass.value
//			case _ => self.asInstanceOf[A]
//		}
//		/** Returns the result if it is $Passed, or the lazily computed alternative passed as an argument otherwise. */
//		@inline def getOrElse[O >: A](or: => O) :O = self match {
//			case _ :Failed => or
//			case _ => get
//		}
//		/** Similarly to [[net.noresttherein.sugar.vars.CheckedExtension.getOrElse getOrElse]], returns the result
//		  * of this $Checked if it is $Passed, or `alt` if it is $Failed. The difference is that the alternative value
//		  * is not lazily computed and guarantees no closure will be created,
//		  * at the cost of possibly discarding it without use.
//		  * @param or the value to return if this instance is a failure. */
//		@inline def orDefault[O >: A](or :O) :O = self match {
//			case _ :Failed => or
//			case _ => get
//		}
//		/** Assuming that `A` is a nullable type, return `null` if this $Checked is $Failed,
//		  * or a wrapped result of $Passed otherwise. */
//		@inline def orNull[O >: A](implicit isNullable :O <:< Null) :O = self match {
//			case _ :Failed => null.asInstanceOf[O]
//			case _ => get
//		}
//
//
//		/** Returns the exam result if it is $Passed, or throws an exception given as the type parameter with
//		  * the error message contained in $Failed.
//		  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
//		  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
//		  * @see [[net.noresttherein.sugar.vars.CheckedExtension.orNoSuch orNoSuch]]
//		  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
//		@inline def orThrow[E <: Throwable :ClassTag] :A = self match {
//			case fail :Failed => raise[E](fail.error)
//			case _ => get
//		}
//		/** Gets the element in this $Checked if it is $Passed, or throws a [[NoSuchElementException]]
//		  * with the error message contained in $Failed.
//		  * @see [[net.noresttherein.sugar.vars.CheckedExtension.orThrow orThrow]] */
//		@inline def orNoSuch :A = self match {
//			case fail :Failed => throw new NoSuchElementException(fail.error)
//			case _ => get
//		}
//		/** Gets the element in this $Checked if it is $Passed, or throws an [[IllegalArgumentException]]
//		  * with the error message contained in $Failed.
//		  * @see [[net.noresttherein.sugar.vars.CheckedExtension.orThrow orThrow]] */
//		@inline def orIllegal :A = self match {
//			case fail :Failed => throw new IllegalArgumentException(fail.error)
//			case _ => get
//		}
//		/** Asserts that this instance is $Passed and returns its contents, throwing an [[AssertionError]]
//		  * with the error message contained in $Failed otherwise. */
//		@inline def orError :A = {
//			assert(!self.isInstanceOf[Failed], self.asInstanceOf[Failed].error)
//			get
//		}
//
//		/** Returns this $Checked if it is $Passed, or the lazily computed alternative otherwise. */
//		@inline def orElse[O >: A](or: => Checked[O]) :Checked[O] = self match {
//			case _ :Failed => or
//			case _ => self
//		}

		/** Similarly to [[net.noresttherein.sugar.vars.CheckedExtension.orElse orElse]], returns this $Checked
		  * if it is $Passed, or `alt` otherwise. The difference is that the alternative value is not lazily computed
		  * and guarantees no closure would be be created, at the cost of possibly discarding it without use.
		  * @param or the value to return if this instance is empty. */
		@inline def ifFailed[O >: A](or: Checked[O]) :Checked[O] = self match {
			case _ :Failed @unchecked => or
			case _ => self
		}



	}

	implicit class FailedExtension(private val self :Failed) extends AnyVal {
		/** The error message of this $Failed. */
		@inline def error :String = self.value
	}
}






package vars {

	object Potential {
		def apply[A](value :A) :Potential[A] = value match {
			case null => Inexistent
			case Inexistent | _ :Existent[_] => new Existent(value).asInstanceOf[Potential[A]]
			case _ => value.asInstanceOf[Potential[A]]
		}

		/** Converts the given `Option[T]` into a lighter `Potential[T]` which is erased at runtime. */
		@inline def some_?[A](value :Option[A]) :Potential[A] = value match {
			case Some(a) => Existent(a)
			case _ => Inexistent
		}

		/** Converts the given `Opt[T]` into a `Potential[T]`. */
		@inline def got_?[A](value :Opt[A]) :Potential[A] = value match {
			case Got(a) => Existent(a)
			case _ => Inexistent
		}

		/** Converts the given `Unsure[T]` into a `Potential[T]`, erased at runtime. */
		@inline def sure_?[A](value :Unsure[A]) :Potential[A] = value match {
			case Sure(a) => Existent(a)
			case _ => Inexistent
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

		//shadowing of the implicit to enforce local usage of PotentialExtensions conversion.
		@inline implicit private def PotentialExtension[A](potential :Potential[A]) :PotentialExtension[A] =
			new PotentialExtension(potential)

		object Existent {
			def apply[A](value :A) :Potential[A] = value match {
				case Inexistent | _ :Existent[_] => new Existent(value).asInstanceOf[Potential[A]]
				case _ => value.asInstanceOf[Potential[A]]
			}

			def unapply[A](opt :Potential[A]) :Opt[A] = opt match {
				case Inexistent => Lack
				case exists :Existent[A @unchecked] => Got(exists.value)
				case _ => Got(opt.asInstanceOf[A])
			}
		}

//		type Existent[+A] // >: Exists[A]
		private[vars] class Existent[+A](val value :A) extends Serializable {
			//fixme: erased instance does not equal a wrapped instance; if we add a case value == that here,
			//  equals will become asymmetrical. Lets look what we can do about in Scala 3, but if nothing,
			//  then we'll have to prohibit nesting of Potentials
			override def equals(that :Any) :Boolean = that match {
				case exists :Existent[_] => value == exists.value
				case _ => false
			}
			override def hashCode :Int = value.hashCode
			override def toString :String = "Existent(" + value + ")"
		}

		final val Inexistent :Potential[Nothing] = NonExistent.asInstanceOf[Potential[Nothing]]

		private[vars] object NonExistent  extends (Any => AnyRef) with Serializable {
			override def apply(v1 :Any) = this
			override def toString = "Inexistent"
		}

		/** The for-comprehension facade for `Potential[A]`, which does not evaluate the filter predicate until
		  * `map`, `flatMap` or `foreach` is called.
		  */
		class WithFilter[+A](self :Potential[A], p :A => Boolean) {
			@inline def map[B](f: A => B): Potential[B] = self filter p map f
			@inline def flatMap[B](f: A => Potential[B]): Potential[B] = self filter p flatMap f
			@inline def foreach[U](f: A => U): Unit = self filter p foreach f
			@inline def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
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



	object Pill {
		def apply[R, B](either :Either[R, B]) :Pill[R, B] = either match {
			case Left(red) => Red(red)
			case Right(blue) => Blue(blue)
		}

		/** A factory and matching pattern for [[net.noresttherein.sugar.vars.Pill! Pill]] instances
		  * representing a successful result of a computation.
		  */
		object Blue {
			def apply[B](value :B) :Pill[Nothing, B] = value match {
				case _ :Red[_] | _ :Blue[_] => new Blue(value).asInstanceOf[Pill[Nothing, B]]
				case _ => value.asInstanceOf[Pill[Nothing, B]]
			}

			def unapply[B](alt :Pill[Any, B]) :Opt[B] = alt match {
				case _ :Red[_] => Lack
				case blue :Blue[B @unchecked] => Got(blue.value)
				case _ => Got(alt.asInstanceOf[B])
			}
		}

		//fixme: Incorrect equality; erased `Blue` will not equal this class.
		//  If it's not possible to fix with Scala 3 equality, we might have to prohibit nesting Pills
		@SerialVersionUID(1L) //not a case class to avoid unwanted apply method
		private[vars] class Blue[+B](val value :B) extends Serializable {
			override def equals(that :Any) :Boolean = that match {
				case blue :Blue[_] => value == blue.value
				case _ => false
			}
			override def hashCode :Int = value.hashCode
			override def toString = "Blue(" + value + ")"
		}

		/** A factory and matching pattern for [[net.noresttherein.sugar.vars.Pill! Pill]] instances
		  * representing a failed result (containing error information).
		  */
		object Red {
			def apply[R](value :R) :Pill[R, Nothing] = new Red(value)

			def unapply[R](alt :Pill[R, Any]) :Opt[R] = alt match {
				case red :Red[R @unchecked] => Got(red.value)
				case _ => Lack
			}
		}

		/** The unsuccessful result of an $Pill, carrying error information. It conforms to `Pill[R, Nothing]`,
		  * so it can be carried over while mapping or flat mapping the $Blue case.
		  */
		@SerialVersionUID(1L)
		final case class Red[+R](value :R)
	}



	object Checked {
		def apply[O](either :Either[String, O]) :Checked[O] = either match {
			case Left(error) => Failed(error)
			case Right(value) => Passed(value)
		}

		/** Factory and matching pattern for [[net.noresttherein.sugar.vars.Checked! Checked]] instances
		  * representing a passed result (containing a value).
		  */
		object Passed {
			def apply[T](value :T) :Checked[T] = value match {
				case _ :Failed @unchecked | _ :Passed[_] => new Passed(value).asInstanceOf[Checked[T]]
				case _ => value.asInstanceOf[Checked[T]]
			}

			def unapply[T](exam :Checked[T]) :Opt[T] = (exam :Any) match {
				case _ :Failed @unchecked => Lack
				case pass :Passed[T @unchecked] => Got(pass.value)
				case _ => Got(exam.asInstanceOf[T])
			}
		}

		/** A reified `Passed` case used when `value` is a `Failed` or `Passed` instance.
		  * Used to differentiate between `Passed(Failed)` and `Failed`.
		  */
		@SerialVersionUID(1L)
		private[vars] class Passed[+T](val value :T) extends Serializable {
			override def equals(that :Any) :Boolean = that match {
				case other :Passed[_] => value == other.value
				case _ => false
			}
			override def hashCode :Int = value.hashCode
			override def toString :String = "Passed(" + value + ")"
		}

		/** Factory and matching pattern for [[net.noresttherein.sugar.vars.Checked! Checked]] instances
		  * representing a failed result (containing an error message).
		  */
		object Failed {
			def apply(error :String) :Checked[Nothing] = new Failed(error).asInstanceOf[Checked[Nothing]]

			def unapply(exam :Checked[Any]) :Opt[String] = exam match {
				case fail :Failed @unchecked => Got(fail.error)
				case _ => Lack
			}
		}

		/** The unsuccessful result of $Checked, carrying an error message. It conforms to `Checked[Nothing]`,
		  * so it can be carried over while mapping or flat mapping the $Passed case.
		  */
		type Failed = Red[String]
//		@SerialVersionUID(1L)
//		final class Failed(val error :String) extends Serializable {
//			override def equals(that :Any) :Boolean = that match {
//				case fail :Failed => error == fail.error
//				case _ => false
//			}
//			override def hashCode :Int = error.hashCode
//			override def toString :String = "Failed(" + error + ")"
//		}
	}
}
