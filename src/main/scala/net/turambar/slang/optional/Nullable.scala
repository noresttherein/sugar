package net.turambar.slang.optional


/**  A value class treating nullable reference types as option-like monads erased in runtime. It behaves
  *  exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  *  recursion/loops.
  *
  *  For convenience, the type parameter is upper bound by `AnyRef` rather than lower bound by `Null`.
  *  This makes it impossible to create an instance by directly passing the `null` literal as argument, as
  *  `Nothing &lt;: AnyRef` but `null` is not an instance of `Nothing`. Unlike `Option[T]` however, the empty
  *  value is not of type `Option[Nothing]` but `Nullable[Null]` and it is impossible to create an instance of
  *  type `Nullable[Nothing]` due to no such public constructor being available. Thus while it is possible to
  *  create a method declaring, either explicitly or through type constructor application, an argument of type
  *  `Nullable[Nothing]`, it will be impossible to invoke (without explicit casting by client code). This is
  *  similar to accepting a function `Nothing => T` denoting that it is never called.  The advantage of this
  *  not elegant hack is removing the need for declaring type parameters of generic types to be `T &gt;: Null`,
  *  only to use `Nullable[T]` in implementation. This would be not only tedious, but often requires propagating
  *  this lower bound to many related types just for one edge case.
  */
final class Nullable[+T <: AnyRef] private (/** A wrapped reference value which may be null. */val orNull :T) extends AnyVal {

	/** Tests if this value is null representing 'no value'. */
	@inline def isEmpty: Boolean = orNull==null

	/** Tests if this value is not null. */
	@inline def nonEmpty: Boolean = orNull!=null

	/** Tests if this value is not null. */
	@inline def isDefined: Boolean = orNull!=null

	/** Tests if this instance is `null`, Same as `isEmpty`. */
	@inline def isNull: Boolean = orNull==null


	/** Returns an empty `Nullable` of the same type as this instance. As in contexts where it is not known if `T &lt;: Null`
	  * it is impossible to use an empty value [[net.turambar.slang.optional,Nullable.Empty]] this instance enforces the
	  * invariant that `T` cannot not be `Nothing` (or the code is part of a method with argument type for which no values
	  * exists), even if it is not known to the compiler at the calling point.
	  * @return a `Nullable[T]` wrapping `null`,
	  */
	@inline def empty :Nullable[T] = new Nullable(null.asInstanceOf[T])



	/** Forces extraction of the value.
	  * @return contained value, if it not null
	  * @throws NoSuchElementException if this value is `null`.
	  */
	@inline def get :T =
		if (orNull==null) throw new NoSuchElementException("Nullable.Empty.get")
		else orNull

	/** Returns this value if it is not null or the lazily computed alternative passed as the argument in the opposite case. */
	@inline def getOrElse[O >: T <: AnyRef](alt : =>O) :O =
		if (orNull==null) alt else orNull

	/** Returns this `Nullable` if it is not null or the lazily computed alternative for null values. */
	@inline def orElse[O >: T <: AnyRef](alt : =>Nullable[O]) :Nullable[O] =
		if (orNull==null) alt else this


	/** Similarly to [[getOrElse]], returns the value if non-null and `alt` otherwise. The difference is that the
	  * alternative value is not lazily computed and guarantees no closure would be created at the cost of possibly
	  * discarding it without use.
	  * @param alt the value to return if this instance is `null`.
	  */
	@inline def defaults[O >: T <: AnyRef](alt: O) :O =
		if (orNull==null) alt else orNull


	/** Similarly to [[orElse]], returns this `Nullable` if it is not `null` and `alt` otherwise. The difference is
	  * that the alternative value is not lazily computed and guarantees no closure would be be created at the cost
	  * of possibly discarding it without use.
	  * @param alt the value to return if this instance is `null`.
	  */
	@inline def ifEmpty[O >: T <: AnyRef](alt: Nullable[O]) :Nullable[O] =
		if (orNull==null) alt else this


	/** Similarly to [[orElse]], returns this `Nullable` if it is not `null` and `alt` otherwise. The difference is
	  * that the alternative value is not lazily computed and guarantees no closure would be be created at the cost
	  * of possibly discarding it without use.
	  * @param alt the value to return if this instance is `null`.
	  */
	@inline def ifNull[O >: T <: AnyRef](alt: Nullable[O]) :Nullable[O] =
		if (orNull==null) alt else this



	/** Wraps the value in an option.
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	@inline def toOption = Option(orNull)


	/** Executes the given block for this value if it is not null. */
	@inline def foreach[O](f :T=>O) :Unit = if (orNull!=null) f(orNull)

	/** Tests if this value is not null and satisfies the given predicate. */
	@inline def exists(p :T=>Boolean): Boolean = orNull!=null && p(orNull)

	/** Tests if this value is null or satisfies the given predicate. */
	@inline def forall(p :T=>Boolean): Boolean = orNull==null || p(orNull)

	/** Tests if this value is not null and equal to the given argument. */
	@inline def contains[O >: T <: AnyRef](o :O): Boolean = orNull!=null && orNull == o

	/** Returns a new `Nullable` containing this value if it is not null and satisfies the given predicate,
	  * or a null value otherwise.
	  */
	@inline def filter(p :T=>Boolean) :Nullable[T] =
		if (orNull!=null && p(orNull)) this else new Nullable(null.asInstanceOf[T])

	/** Equivalent to `this.`[[Nullable#filter]]`(p)` - a variant for use in for-comprehensions. */
	@inline def withFilter(p :T=>Boolean) :Nullable[T] =
		if (orNull!=null && p(orNull)) this else new Nullable(null.asInstanceOf[T])


	/** Returns a new `Nullable` which is empty if this value is null or contains the result of applying
	  * the given function to it otherwise. Lower bound of `Null` has to be enforced here for the edge case
	  * of `T =:= Nothing` which would map a null value into `Nullable[Nothing]`, which in turn would imply
	  * `null :Nothing` by the `orNull` method.
	  * @see [[net.turambar.slang.optional.Nullable#transform]]
	  */
	@inline def map[O >: Null <: AnyRef](p :T=>O) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else new Nullable(p(orNull))


	/** Similar to `map` but returns a supertype of `T` and thus doesn't need an additional lower bound of `Null`
	  * like the latter.
	  * @param p mapping function applied to this value if it is not null
	  * @return `null` if `this.isNull` or `Nullable(p(this.get))` otherwise.
	  */
	@inline def transform[O >: T <: AnyRef](p :T=>O) :Nullable[O] =
		if (orNull==null) this else new Nullable(p(orNull))


	/** Returns the result of applying the given function to this value if it is not null or
	  * `this` if `this.isEmpty`
	  */
	@inline def flatMap[O <: AnyRef](p :T=>Nullable[O]) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else p(orNull)

	/** Flattens `Nullable[Nullable[O]]` to a single `Nullable[O]`. */
	@inline def flatten[O <: AnyRef](implicit isOpt :T<:<Nullable[O]) :Nullable[O] =
		if (orNull==null) new Nullable(null.asInstanceOf[O])
		else orNull


	/** Returns an empty `Nullable` if this value is null or `f` is not defined for it, otherwise
	  * applying the given partial function and wrapping it in a new `Nullable`.  Lower bound of `Null`
	  * has to be enforced here for the edge case of `T =:= Nothing` which would map a null value into
	  * `Nullable[Nothing]`, which in turn would imply `null :Nothing` * by the `orNull` method.
	  */
	@inline def collect[O >: Null <: AnyRef](f :PartialFunction[T, O]) :Nullable[O] =
		if (orNull!=null) new Nullable(f.applyOrElse(orNull, null))
		else new Nullable(null)



	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline def iterator :Iterator[T] =
		if (orNull==null) Iterator.single(orNull) else Iterator.empty


	/** Returns `Nil` if this value is null or or `this.get::Nil` otherwise. */
	@inline def toList :List[T] = if (orNull==null) Nil else orNull::Nil

	/** Returns an empty list if this value is `null` or a single element list with this value otherwise. */
	@inline def toSeq :Seq[T] = if (orNull==null) Nil else orNull::Nil
}





/** Companion object providing factory methods and extractors working with [[Nullable]]s. */
object Nullable {
	@inline final implicit def nullableToOption[T <: AnyRef](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
	@inline final implicit def optionToNullable[T >: Null <: AnyRef](opt :Option[T]) :Nullable[T] = new Nullable(opt.orNull)

	object ExtraImplicits {
		@inline final implicit def nullableToList[T <: AnyRef](opt :Nullable[T]) :List[T] =
			if (opt.orNull == null) Nil else opt.orNull :: Nil
	}


	/** Wraps the given reference in a purely syntactic option-like object erased in the runtime.
	  * Note that the wrapped type is upper bound here by `AnyRef` rather than lower bound by `Null`,
	  * as providing an argument of type `T` excludes the single `AnyRef` subtype which is not the supertype
	  * of `Null`, that is `Nothing`.
	  * @see [[net.turambar.slang.optional.Nullable!]]
	  */
	@inline final def apply[T <: AnyRef](value :T) :Nullable[T] = new Nullable(value)



	/** A `null` value representing an empty [[Nullable]] for any true reference type.
	  * Note that we need to enforce the lower bound of `Null` to prevent the implication of `null :Nothing`,
	  * which makes it impossible to use directly as `[[[Nullable[T]]]]` in contexts where the compiler can't prove
	  * that `T &gt;: Null`. As a workaround, [[net.turambar.slang.optional.Nullable$.empty]] can be used instead.
	  * In addition, instances of `Nullable` provide [[net.turambar.slang.optional.Nullable#empty]] method
	  * returning a value representing `null` of the same type, serving as a guarantee of the required condition,
	  * which must have had held at their creation.
	  * @see [[#empty]]
	  * @see [[net.turambar.slang.optional.Nullable!.empty]]
	  */
	@inline final val Empty :Nullable[Null] = new Nullable(null)


	/** Returns a `null` value as a `Nullable[T]`.
	  * @param isNotNothing an ignored argument serving solely as a proof that `T` cannot be `Nothing` and thus
	  *                     must be a supertype of `Null`.
	  */
	@inline final def empty[T <: AnyRef](isNotNothing :T) :Nullable[T] = new Nullable(null.asInstanceOf[T])


	/** Extractor for non-null values of [[Nullable]] to be used in pattern matching.
	  * This may well result in boxing of the value which `Nullable` is designed to prevent,
	  * so common sense in application is required.
	  */
	object NotNull {
		@inline final def unapply[T <: AnyRef](opt :Nullable[T]) :Option[T] = Option(opt.orNull)
	}


}


