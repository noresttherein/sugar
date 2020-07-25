package net.noresttherein.slang.optional

import net.noresttherein.slang.optional.Opt.EmptyToken



/**  A value class treating nullable reference types as option-like monads erased in runtime. It behaves
  *  exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  *  recursion/loops.
  *
  *  For convenience, the type parameter is upper bound by `AnyRef` rather than lower bound by `Null`.
  *  This makes it impossible to create an instance by directly passing the `null` literal as argument, as
  *  `Nothing &lt;: AnyRef` but `null` is not an instance of `Nothing`. Unlike `Option[T]` however, the empty
  *  value is not of type `Option[Nothing]` but `Opt[Null]` and it is impossible to create an instance of
  *  type `Opt[Nothing]` due to no such public constructor being available. Thus while it is possible to
  *  create a method declaring, either explicitly or through type constructor application, an argument of type
  *  `Opt[Nothing]`, it will be impossible to invoke (without explicit casting by client code). This is
  *  similar to accepting a function `Nothing => T` denoting that it is never called.  The advantage of this
  *  not elegant hack is removing the need for declaring type parameters of generic types to be `T &gt;: Null`,
  *  only to use `Opt[T]` in implementation. This would be not only tedious, but often requires propagating
  *  this lower bound to many related types just for one edge case.
  */
final class Opt[+T] private (private val ref :AnyRef) extends AnyVal with Serializable {

	/** Tests if this value is null representing 'no value'. */
	@inline def isEmpty: Boolean = ref eq EmptyToken

	/** Tests if this value is not null. */
	@inline def nonEmpty: Boolean = !(ref eq EmptyToken)

	/** Tests if this value is not null. */
	@inline def isDefined: Boolean = !(ref eq EmptyToken)



	/** Forces extraction of the value.
	  * @return contained value, if it not null
	  * @throws NoSuchElementException if this value is `null`.
	  */
	@inline def get :T =
		if (ref eq EmptyToken) throw new NoSuchElementException("Opt.Empty.get")
		else ref.asInstanceOf[T]

	/** Returns this value if it is not null or the lazily computed alternative passed as the argument in the opposite case. */
	@inline def getOrElse[O >: T](alt: => O) :O =
		if (ref eq EmptyToken) alt else ref.asInstanceOf[T]

	/** Returns this `Opt` if it is not null or the lazily computed alternative for null values. */
	@inline def orElse[O >: T](alt: => Opt[O]) :Opt[O] =
		if (ref eq EmptyToken) alt else this


	/** Similarly to [[getOrElse]], returns the value if non-null and `alt` otherwise. The difference is that the
	  * alternative value is not lazily computed and guarantees no closure would be created at the cost of possibly
	  * discarding it without use.
	  * @param alt the value to return if this instance is `null`.
	  */
	@inline def defaults[O >: T](alt: O) :O =
		if (ref eq EmptyToken) alt else ref.asInstanceOf[T]


	/** Similarly to [[orElse]], returns this `Opt` if it is not `null` and `alt` otherwise. The difference is
	  * that the alternative value is not lazily computed and guarantees no closure would be be created at the cost
	  * of possibly discarding it without use.
	  * @param alt the value to return if this instance is `null`.
	  */
	@inline def ifEmpty[O >: T](alt: Opt[O]) :Opt[O] =
		if (ref eq EmptyToken) alt else this


	/** Assuming that `T` is a nullable type, return `null` if this `Opt` is empty, or the wrapped value otherwise. */
	@inline def orNull[O >: T](implicit ev :Null <:< O) :O =
		if (ref eq EmptyToken) null.asInstanceOf[O] else ref.asInstanceOf[O]


	/** Wraps the value in an option.
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	@inline def toOption :Option[T] = if (ref eq EmptyToken) None else Some(ref.asInstanceOf[T])


	/** Executes the given block for this value if it is not null. */
	@inline def foreach[O](f :T => O) :Unit = if (!(ref eq EmptyToken)) f(ref.asInstanceOf[T])

	/** Tests if this value is not null and satisfies the given predicate. */
	@inline def exists(p :T => Boolean): Boolean = !(ref eq EmptyToken) && p(ref.asInstanceOf[T])

	/** Tests if this value is null or satisfies the given predicate. */
	@inline def forall(p :T => Boolean): Boolean = (ref eq EmptyToken) || p(ref.asInstanceOf[T])

	/** Tests if this value is not null and equal to the given argument. */
	@inline def contains[O >: T](o :O): Boolean = ref == o

	/** Returns a new `Opt` containing this value if it is not null and satisfies the given predicate,
	  * or a null value otherwise.
	  */
	@inline def filter(p :T => Boolean) :Opt[T] =
		if (!(ref eq EmptyToken) && p(ref.asInstanceOf[T])) this else new Opt(EmptyToken)

	/** Equivalent to `this.`[[Opt#filter]]`(p)` - a variant for use in for-comprehensions. */
	@inline def withFilter(p :T => Boolean) :Opt[T] =
		if (!(ref eq EmptyToken) && p(ref.asInstanceOf[T])) this else new Opt(EmptyToken)


	/** Returns a new `Opt` which is empty if this value is null or contains the result of applying
	  * the given function to it otherwise. Lower bound of `Null` has to be enforced here for the edge case
	  * of `T =:= Nothing` which would map a null value into `Opt[Nothing]`, which in turn would imply
	  * `null :Nothing` by the `orNull` method.
	  * @see [[net.noresttherein.slang.optional.Opt#transform]]
	  */
	@inline def map[O](p :T => O) :Opt[O] =
		if (ref eq EmptyToken) new Opt(EmptyToken)
		else new Opt(p(ref.asInstanceOf[T]).asInstanceOf[AnyRef])

	/** Returns the result of applying the given function to this value if it is not null or
	  * `this` if `this.isEmpty`
	  */
	@inline def flatMap[O](p :T => Opt[O]) :Opt[O] =
		if (ref eq EmptyToken) new Opt(EmptyToken)
		else p(ref.asInstanceOf[T])

	/** Flattens `Opt[Opt[O]]` to a single `Opt[O]`. */
	def flatten[O](implicit isOpt :T <:< Opt[O]) :Opt[O] =
		(ref :Any) match {
			case EmptyToken => new Opt(EmptyToken)
			case opt :Opt[O @unchecked] => opt
			case _ => new Opt(ref)
		}


	/** Returns an empty `Opt` if this value is null or `f` is not defined for it, otherwise
	  * applying the given partial function and wrapping it in a new `Opt`.  Lower bound of `Null`
	  * has to be enforced here for the edge case of `T =:= Nothing` which would map a null value into
	  * `Opt[Nothing]`, which in turn would imply `null :Nothing` * by the `orNull` method.
	  */
	@inline def collect[O](f :PartialFunction[T, O]) :Opt[O] =
		if (ref eq EmptyToken)
			new Opt(EmptyToken)
		else
			new Opt(f.asInstanceOf[PartialFunction[T, AnyRef]].applyOrElse(ref.asInstanceOf[T], EmptyToken))



	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline def iterator :Iterator[T] =
		if (ref eq EmptyToken) Iterator.empty else Iterator.single(ref.asInstanceOf[T])


	/** Returns `Nil` if this value is null or or `this.get::Nil` otherwise. */
	@inline def toList :List[T] = if (ref eq EmptyToken) Nil else ref.asInstanceOf[T]::Nil

	/** Returns an empty list if this value is `null` or a single element list with this value otherwise. */
	@inline def toSeq :Seq[T] = if (ref eq EmptyToken) Nil else ref.asInstanceOf[T]::Nil

}




/** Companion object providing factory methods and extractors working with [[Opt]]s. */
object Opt {
	@inline final implicit def optToOption[T](opt :Opt[T]) :Option[T] = opt.toOption

	@inline final implicit def optionToOpt[T](opt :Option[T]) :Opt[T] = opt match {
		case Some(x) => new Opt(x.asInstanceOf[AnyRef])
		case _ => new Opt(EmptyToken)
	}

//	object ExtraImplicits {
//		@inline final implicit def OptToList[T <: AnyRef](opt :Opt[T]) :List[T] =
//			if (opt.orNull == null) Nil else opt.orNull :: Nil
//	}


	/** Wraps the given reference in a purely syntactic option-like object erased in the runtime.
	  * Note that the wrapped type is upper bound here by `AnyRef` rather than lower bound by `Null`,
	  * as providing an argument of type `T` excludes the single `AnyRef` subtype which is not the supertype
	  * of `Null`, that is `Nothing`.
	  * @see [[net.noresttherein.slang.optional.Opt!]]
	  */
	@inline final def apply[T](value :T) :Opt[T] = new Opt(value.asInstanceOf[AnyRef])

	@inline final def empty[T] :Opt[T] = Miss


	/** Extractor for non-null values of [[Opt]] to be used in pattern matching.
	  * This may well result in boxing of the value which `Opt` is designed to prevent,
	  * so common sense in application is required.
	  */
	object Got {
		@inline def apply[T](x :T) :Opt[T] = new Opt(x.asInstanceOf[AnyRef])
		@inline final def unapply[T](opt :Opt[T]) :Opt[T] = opt
	}

	/** A `null` value representing an empty [[Opt]] for any true reference type.
	  * Note that we need to enforce the lower bound of `Null` to prevent the implication of `null :Nothing`,
	  * which makes it impossible to use directly as [[Opt[T]]] in contexts where the compiler can't prove
	  * that `T &gt;: Null`. As a workaround, [[net.noresttherein.slang.optional.Opt$.empty]] can be used instead.
	  * In addition, instances of `Opt` provide [[net.noresttherein.slang.optional.Opt#empty]] method
	  * returning a value representing `null` of the same type, serving as a guarantee of the required condition,
	  * which must have had held at their creation.
	  * @see [[#empty]]
	  * @see [[net.noresttherein.slang.optional.Opt!.empty]]
	  */
	@inline final val Miss = new Opt[Nothing](EmptyToken)


	private[Opt] object EmptyToken extends (Any => AnyRef) with Serializable {
		def apply(ignore :Any) :AnyRef = this
	}
}








