package net.noresttherein.sugar.vars

import scala.reflect.ClassTag

import net.noresttherein.sugar.raise
import net.noresttherein.sugar.vars.Opt.{unzip2Lack, unzip3Lack, Got, Lack, NoContent, WithFilter}



/** A value class treating nullable reference types as option-like monads erased in runtime. It behaves
  * exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  * recursion/loops. As a value class, it has no distinct subclasses for empty and non-empty instances, which results
  * in certain differences from `Option`. Aside from the obvious lack of creation of an additional object,
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, a disadvantage
  * of being erased in runtime is that methods accepting `Opt`s with different type arguments will clash
  * with each other, as well as with methods accepting `T` itself (if erased) in 'double definition' errors.
  * Moreover, a generic method accepting/returning an abstract type (parameter) `T` cannot be overriden/implemented
  * by a method accepting an `Opt[O]`, where `O` is an abstract type (or type parameter).
  *
  * Note that, as this is a value class wrapping any type, boxing of built in value types to their reference wrappers
  * will still occur. Unlike its standard counterpart, this type is not an `Iterable`, although it contains all standard
  * collection methods and an implicit conversion to `Iterable[T]` exists. The latter will however result in boxing
  * which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions''
  * composing several `Opt`s' can result in closures being created (as manual nesting of `flatMap` calls also can).
  * For this reason its best to either directly use `isEmpty` and `get` in a conditional expression, or use the
  * [[net.noresttherein.sugar.vars.Opt.Got$ Got]] matching pattern, which should by modern compilers be translated
  * to non-boxing byte code.
  * @see [[net.noresttherein.sugar.vars.Opt.Got$]]
  * @see [[net.noresttherein.sugar.vars.Opt.Lack]]
  * @see [[net.noresttherein.sugar.vars.Unsure]]
  */
@SerialVersionUID(1L)
//todo: in Scala 3 make it an opaque type to differentiate it more from Unsure. Will need some boxing to distinguish
// Got(Lack) from Lack.
class Opt[+A] private[Opt] (private val ref :AnyRef) //private[Opt] to allow inlining of its construction
	extends AnyVal with Ref[A] with IterableOnce[A] with Product with Equals with Serializable
{
	/** A flag member type specifying if the option is full or empty on type level through refinement.
	  * For example, `opt :Opt[T] { type isEmpty = false }` guarantees that `opt.get` will return a value.
	  * Naturally, this being erased type, this guarantee is weaker than `opt :Some[T]`, because any instance
	  * can be successfully cast to a type with any value for this type.
	  * @see [[net.noresttherein.sugar.vars.Opt.Got!]] */
	type isEmpty <: Boolean with Singleton

	/** Tests if this `Opt` does not contain a value (is equal to [[net.noresttherein.sugar.vars.Opt.Lack Lack]]). */
	@inline def isEmpty: Boolean = ref eq NoContent

	/** Tests if this `Opt` contains a value. If true, `get` will not throw an exception. */
	@inline def nonEmpty: Boolean = ref ne NoContent

	/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
	@inline override def isDefined: Boolean = ref ne NoContent

	@inline override def knownSize :Int = if (ref eq NoContent) 0 else 1

	@inline override def productArity :Int = if (ref eq NoContent) 0 else 1

	@inline override def productElement(n :Int) :Any =
		if (n == 1 && (ref ne NoContent)) ref
		else throw new IndexOutOfBoundsException(toString + ".productElement(" + n + ")")


	/** The wrapped value. It is the same as [[net.noresttherein.sugar.vars.Opt.get get]], but this method
	  * is only possible to call on instances for which it is statically known that the value exists based on
	  * [[net.noresttherein.sugar.vars.Opt.isEmpty! isEmpty]] member type.
	  * @return the contained value. */
	@inline def value(implicit got :this.type <:< Got[_]) :A =
		if (ref eq NoContent) throw new NoSuchElementException("Opt.empty.get")
		else ref.asInstanceOf[A]

	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Opt` is empty. */
	@inline override def get :A =
		if (ref eq NoContent) throw new NoSuchElementException("Opt.empty.get")
		else ref.asInstanceOf[A]

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: A](alt: => O) :O =
		if (ref eq NoContent) alt else ref.asInstanceOf[A]

	/** Similarly to [[net.noresttherein.sugar.vars.Opt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty. */
	@inline def orDefault[O >: A](alt: O) :O =
		if (ref eq NoContent) alt else ref.asInstanceOf[A]

	/** Assuming that `A` is a nullable type, return `null` if this `Opt` is empty, or the wrapped value otherwise. */
	@inline def orNull[O >: A](implicit isNullable :Null <:< O) :O =
		if (ref eq NoContent) null.asInstanceOf[O] else ref.asInstanceOf[O]


	/** Gets the element in the `Opt` or throws the exception given as the type parameter with the given message.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  * @see [[net.noresttherein.sugar.vars.Opt.orNoSuch orNoSuch]]
	  * @see [[net.noresttherein.sugar.vars.Opt.orIllegal orIllegal]] */
	@inline def orThrow[E <: Throwable :ClassTag](msg: => String) :A =
		if (ref eq NoContent) raise[E](msg) else ref.asInstanceOf[A]

	/** Gets the element in this `Opt` or throws a [[NoSuchElementException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Opt.orThrow orThrow]] */
	@inline def orNoSuch(msg: => String) :A =
		if (ref eq NoContent) throw new NoSuchElementException(msg) else ref.asInstanceOf[A]

	/** Gets the element in this `Opt` or throws an [[IllegalArgumentException]] with the given message.
	  * @see [[net.noresttherein.sugar.vars.Opt.orThrow orThrow]] */
	@inline def orIllegal(msg: => String) :A =
		if (ref eq NoContent) throw new IllegalArgumentException(msg) else ref.asInstanceOf[A]

	/** Asserts that this instance is not empty and returns its contents, throwing an [[AssertionError]] otherwise. */
	@inline def orError(msg: => String) :A = {
		assert(ref ne NoContent, msg)
		ref.asInstanceOf[A]
	}



	/** Returns the value this `Opt` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse[O >: A](alt: => Opt[O]) :Opt[O] =
		if (ref eq NoContent) alt else this

	/** Similarly to [[net.noresttherein.sugar.vars.Opt.orElse orElse]], returns this `Opt` if it is not empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty. */
	@inline def ifEmpty[O >: A](alt: Opt[O]) :Opt[O] =
		if (ref eq NoContent) alt else this

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
	@inline def map[O](p :A => O) :Opt[O] =
		if (ref eq NoContent) new Opt(NoContent)
		else new Opt(p(ref.asInstanceOf[A]).asInstanceOf[AnyRef])

	/** Applies the given function to the content of this `Opt` and returns the result or the provided alternative
	  * if this instance is empty. Equivalent to `this map f getOrElse alternative`, but in one step. */
	@inline def mapOrElse[X](f :A => X, alternative: => X) :X =
		if (ref eq NoContent) alternative else f(ref.asInstanceOf[A])

	/** Returns the result of applying `f` to the value of this `Opt` if it is non empty,
	  * or the result of evaluating expression `ifEmpty` otherwise.
	  * '''Note''': this method exists in order to fully duplicate the API of `Option` and allow easy replacing
	  * one with another, but its name might be misleading. Consider using
	  * [[net.noresttherein.sugar.vars.Opt.mapOrElse mapOrElse]] instead.
	  *  @param  ifEmpty the expression to evaluate if empty.
	  *  @param  f       the function to apply if nonempty. */
	@inline def fold[B](ifEmpty: => B)(f: A => B): B =
		if (ref eq NoContent) ifEmpty else f(ref.asInstanceOf[A])

	/** Returns the result of applying the given function to the value of this `Opt` if it is not empty,
	  * or `this` if `this.isEmpty` */
	@inline def flatMap[O](p :A => Opt[O]) :Opt[O] =
		if (ref eq NoContent) new Opt(NoContent)
		else p(ref.asInstanceOf[A])

	/** Flattens `Opt[Opt[O]]` to a single `Opt[O]`. */
	def flatten[O](implicit isOpt :A <:< Opt[O]) :Opt[O] =
		(ref :Any) match {
			case NoContent => new Opt(NoContent)
			case opt :Opt[O @unchecked] => opt
			case _ => new Opt(ref)
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

	/** Returns an empty `Opt` if this `Opt` is empty the partial function `f` is not defined for its value,
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

	@inline override def opt :Opt[A] = this

	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise. */
	@inline override def ? :Option[A] = if (ref eq NoContent) None else Some(ref.asInstanceOf[A])

	/** Conversion to an `Unsure` carrying the same value as this instance, if any. Note that while the `Unsure` trait
	  * is specialized for value types, this class is not, and the result will not be specialized. Neither will it
	  * require boxing though, as any value type was promoted to a reference wrapper before putting it in an `Opt`.
	  */
	@inline override def unsure :Unsure[A] =
		if (ref eq NoContent) Blank else new Sure(ref.asInstanceOf[A], cachedOpt = this)

	/** Converts this `Opt` to `Either`, returning the content as `Left`, or the value of the given expression
	  * as `Right` if empty. */
	@inline def toLeft[O](right: => O) :Either[A, O] =
		if (ref eq NoContent) Right(right) else Left(ref.asInstanceOf[A])

	/** Converts this `Opt` to `Either`, returning the content as `Right`, or the value of the given expression
	  * as `Left` if empty. */
	@inline def toRight[O](left: => O) :Either[O, A] =
		if (ref eq NoContent) Left(left) else Right(ref.asInstanceOf[A])


	private[vars] override def isSpecialized = false

	@inline override def canEqual(that :Any) :Boolean = that.isInstanceOf[Opt[_]]
}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.sugar.vars.Opt Opt]]s.
  * @see [[net.noresttherein.sugar.vars.Opt.Lack]]
  * @see [[net.noresttherein.sugar.vars.Opt.Got$]]
  */
object Opt {
	/** Wraps the given reference in a purely syntactic option-like object erased in the runtime.
	  * Note that the wrapped type is upper bound here by `AnyRef` rather than lower bound by `Null`,
	  * as providing an argument of type `T` excludes the single `AnyRef` subtype which is not the supertype
	  * of `Null`, that is `Nothing`.
	  * @see [[net.noresttherein.sugar.vars.Opt]]
	  */
	@inline final def apply[T](value :T) :Opt[T] =
		if (value == null) Lack else new Opt(value.asInstanceOf[AnyRef])

	//when we migrate to an opaque type, we might create a promoting constructor
//	@inline def flat[T](x :Opt[T]) :Opt[Opt[T]] =
//		if (x.ref eq NoContent) Lack else x.asInstanceOf[Opt[Opt[T]]]
//
//	@inline def flat[T](x :T) :Opt[T] =
//		if ((x.asInstanceOf[AnyRef] eq null) | (x.asInstanceOf[AnyRef] eq NoContent)) Lack
//		else x.asInstanceOf[Opt[T]]

	/** Converts the given `Option[T]` into a lighter `Opt[T]` which is erased at runtime. */
	@inline def some_?[T](value :Option[T]) :Opt[T] =
		new Opt(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Converts the given `Unsure[T]` into an `Opt[T]`, erased at runtime. */
	@inline def sure_?[T](value :Unsure[T]) :Opt[T] =
		new Opt(if (value.isDefined) value.get.asInstanceOf[AnyRef] else NoContent)

	/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]] - an empty `Opt`. */
	@inline final def empty[T] :Opt[T] = Lack

	/** When a given condition is true, evaluates the `a` argument and returns `Got(a).`
	  * When the condition is false, `a` is not evaluated and `Lack` is returned.
	  */
	@inline def when[A](cond: Boolean)(a: => A): Opt[A] =
		if (cond) Got(a) else Lack

	/** Unless a given condition is true, this will evaluate the `a` argument and return `Got(a)`.
	  * Otherwise, `a` is not evaluated and `Lack` is returned. */
	@inline def unless[A](cond: Boolean)(a: => A): Opt[A] =
		if (!cond) Got(a) else Lack


	/** A refinement of [[net.noresttherein.sugar.vars.Opt Opt]] marking it through a member flag type
	  * as non-empty. [[net.noresttherein.sugar.vars.Opt$ Opt]] factory object creates instances
	  * narrowed down to this type.
	  */
	type Got[+T] = Opt[T] { type isEmpty = false }

	/** Factory and a matching pattern for non empty values of [[net.noresttherein.sugar.vars.Opt Opt]]. */
	object Got {
		/** Creates a non-empty [[net.noresttherein.sugar.vars.Opt Opt]] wrapping the given value. */
		@inline def apply[T](x :T) :Got[T] = new Opt(x.asInstanceOf[AnyRef]).asInstanceOf[Got[T]]

		/** Matches non-empty [[net.noresttherein.sugar.vars.Opt Opt]] instances. */
		@inline def unapply[T](opt :Opt[T]) :Opt[T] = if (opt == null) Lack else opt
	}

	/** A refinement of [[net.noresttherein.sugar.vars.Opt Opt]] marking it through a member flag type
	  * as empty. [[net.noresttherein.sugar.vars.Opt.Lack Opt.Lack]] is an instance of this type.
	  */
	type Lack = Opt[Nothing] { type isEmpty = true }

	/** A special, empty instance of [[net.noresttherein.sugar.vars.Opt Opt]] which conforms to any `Opt[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object.
	  * @see [[net.noresttherein.sugar.vars.Opt.empty]]
	  */
	@inline final val Lack :Lack = new Opt(NoContent).asInstanceOf[Lack]


	/** The for-comprehension facade for `Opt[A]`, which does not evaluate the filter predicate until
	  * `map`, `flatMap` or `foreach` is called.
	  */
	class WithFilter[+A](self :Opt[A], p :A => Boolean) {
		def map[B](f: A => B): Opt[B] = self filter p map f
		def flatMap[B](f: A => Opt[B]): Opt[B] = self filter p flatMap f
		def foreach[U](f: A => U): Unit = self filter p foreach f
		def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
	}



//	/** Provides [[net.noresttherein.sugar.vars.Opt.OptionToOptConverter.toOpt toOpt]] extension method
//	  * for any `Option[T]`, converting it to `Opt[T]`. Note that this type is largely superfluous as
//	  * there is an implicit conversion from `Option[T]` to `Opt[T]` (no boxing is necessary in that case).
//	  * It can however help in cases where automatic type inference fails.
//	  */ //consider: removing it. Duplicated by OptionExtension in optional.extensions
//	implicit class OptionToOptConverter[T](private val self :Option[T]) extends AnyVal {
//		/** Converts this option into an `Opt` wrapping the same type/value. */
//		@inline def toOpt :Opt[T] = new Opt[T](self match {
//			case Some(value) => value.asInstanceOf[AnyRef]
//			case _ => NoContent
//		})
//	}


	/** Implicit conversions between `Opt` and `Option`.
	  * Conversions between `Opt` and [[net.noresttherein.sugar.vars.Unsure Unsure]] are located
	  * in `Unsure.`[[net.noresttherein.sugar.vars.Unsure.implicits implicits]].
	  */
	object implicits {
		@inline final implicit def optToOption[T](opt :Opt[T]) :Option[T] = opt.asOption
		@inline final implicit def optToIterable[T](opt :Opt[T]) :Iterable[T] = opt.toIterable

		@inline final implicit def optionToOpt[T](option :Option[T]) :Opt[T] =
			new Opt(if (option.isDefined) option.get.asInstanceOf[AnyRef] else NoContent)

		//consider: placing this also in optional.extensions (or optional.implicits)
		/** Implicitly lifts any value `T` to [[net.noresttherein.sugar.vars.Opt Opt]]`[T]`. */
		@inline implicit def gotAny[T](x :T) :Got[T] = new Opt(x.asInstanceOf[AnyRef]).asInstanceOf[Got[T]]
	}

	/** Importing the contents of this object replace all usage of [[Option]]/[[Some]]/[[None]] in the scope with
	  * [[net.noresttherein.sugar.vars.Opt Opt]]/[[net.noresttherein.sugar.vars.Opt.Got Got]]/[[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * This object contains the requiring type aliases overriding the standard types as well as implicit conversions
	  * which allow seamless interoperability with standard Scala APIs.
	  *
	  * Other files which reference classes defined in the import's scope may also need to be modified in order
	  * to comply with changed interfaces.
	  */
	object OptAsOption {
		type Option[T] = Opt[T]
		type Some[T]   = Got[T]

		val Option = Opt
		val Some   = Got
		val None   = Lack
		//same names as in implicits so if both are imported one shadows the other
		@inline implicit def optToOption[T](opt :Opt[T]) :scala.Option[T] = opt.?
		@inline implicit def optionToOpt[T](opt :scala.Option[T]) :Opt[T] = some_?(opt)
		@inline implicit def someToGot[T](opt :scala.Some[T]) :Got[T] = Got(opt.get)
		@inline implicit def gotToSome[T](opt :Sure[T]) :scala.Some[T] = opt.?.asInstanceOf[scala.Some[T]]

		@inline implicit def noneToMiss(none :scala.None.type) :Lack.type = Lack
		@inline implicit def missToNone(miss :Lack.type) :scala.None.type = scala.None
	}


	//extends Any => AnyRef out of laziness, allowing it to pass as the argument to applyOrElse
	//is private[vars] so that methods of Opt can be inlined
	// and because some Ref classes play with the erasure and need access to this marker object
	@SerialVersionUID(1L)
	private[vars] object NoContent extends (Any => AnyRef) with Serializable {
		def apply(ignore :Any) :AnyRef = this
	}

	private val unzip2Lack = (Lack, Lack)
	private val unzip3Lack = (Lack, Lack, Lack)
}








