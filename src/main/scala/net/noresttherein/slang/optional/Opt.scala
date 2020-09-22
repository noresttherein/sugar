package net.noresttherein.slang.optional

import scala.reflect.ClassTag

import net.noresttherein.slang.optional.Opt.{EmptyToken, Miss}
import net.noresttherein.slang.raise



/** A value class treating nullable reference types as option-like monads erased in runtime. It behaves
  * exactly like `scala.Option[T]`, but does not require boxing and thus yields performance benefits in tight
  * recursion/loops. As a value class, it has no distinct subclasses for empty and non-empty instances, which results
  * in certain differences from `Option`. Aside from the obvious lack of creation of an additional object, 
  * all methods, being very short, are declared as `@inline`, yielding additional benefits. However, a disadvantage 
  * of being erased in runtime is that methods accepting `Opt`s with different type arguments will clash 
  * with each other, as well as with methods accepting `T` itself (if erased) in 'double definition' errors.
  * Note that, as this is a generic value class, boxing of built in value types to their reference form will still occur.
  * Unlike its standard counterpart, this type is not an `Iterable`, although it contains all standard 
  * collection methods and an implicit conversion to `Iterable[T]` exists. The latter will however result in boxing
  * which this type was designed to prevent, so should be typically avoided. Similarly, ''for comprehensions'' 
  * composing several `Opt`s' can result in closures being created (as manual nesting of `flatMap` calls also can).
  * For this reason its best to either directly use `isEmpty` and `get` in a conditional expression, or use the
  * [[net.noresttherein.slang.optional.Opt.Got$ Got]] matching pattern, which should by modern compilers be translated
  * to non-boxing byte code.
  * @see [[net.noresttherein.slang.optional.Opt.Got$]]
  * @see [[net.noresttherein.slang.optional.Opt.Miss]]
  */
final class Opt[+T] private[Opt] (private val ref :AnyRef) extends AnyVal with Serializable {

	/** Tests if this `Opt` does not contain a value (is equal to [[net.noresttherein.slang.optional.Opt.Miss Miss]]). */
	@inline def isEmpty: Boolean = ref eq EmptyToken

	/** Tests if this `Opt` contains a value. If true, `get` will not throw an exception. */
	@inline def nonEmpty: Boolean = !(ref eq EmptyToken)

	/** Tests if this `Opt` contains a value. This is the same as `nonEmpty`. */
	@inline def isDefined: Boolean = !(ref eq EmptyToken)



	/** Forces extraction of the value.
	  * @return contained value, if one exists.
	  * @throws NoSuchElementException if this `Opt` is empty.
	  */
	@inline def get :T =
		if (ref eq EmptyToken) throw new NoSuchElementException("Opt.empty.get")
		else ref.asInstanceOf[T]

	/** Returns this value if it is not empty, or the lazily computed alternative passed as the argument otherwise. */
	@inline def getOrElse[O >: T](alt: => O) :O =
		if (ref eq EmptyToken) alt else ref.asInstanceOf[T]

	/** Similarly to [[net.noresttherein.slang.optional.Opt.getOrElse getOrElse]], returns the value if non-empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees 
	  * no closure will be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty.
	  */
	@inline def defaults[O >: T](alt: O) :O =
		if (ref eq EmptyToken) alt else ref.asInstanceOf[T]

	/** Assuming that `T` is a nullable type, return `null` if this `Opt` is empty, or the wrapped value otherwise. */
	@inline def orNull[O >: T](implicit ev :Null <:< O) :O =
		if (ref eq EmptyToken) null.asInstanceOf[O] else ref.asInstanceOf[O]



	/** Gets the element in this `Opt` or throws a `NoSuchElementException` with the given message. */
	@inline def getOrThrow(msg: => String) :T =
		if (ref eq EmptyToken) throw new NoSuchElementException(msg) else ref.asInstanceOf[T]

	/** Gets the element in the `Opt` or throws the exception given as the type parameter with the given message.
	  * @tparam E an exception class which must provide publicly available constructor accepting a single `String`
	  *           argument, or a two-argument constructor accepting a `String` and a `Throwable`.
	  */
	@inline def getOrThrow[E <: Exception :ClassTag](msg: => String) :T =
		if (ref eq EmptyToken) raise[E](msg) else ref.asInstanceOf[T]

	/** Gets the element in the option or throws an `IllegalArgumentException` with the given message. */
	@inline def illegalIfEmpty(msg: => String) :T =
		if (ref eq EmptyToken) throw new IllegalArgumentException(msg) else ref.asInstanceOf[T]



	/** Returns the value this `Opt` if it is not empty, or the lazily computed alternative otherwise. */
	@inline def orElse[O >: T](alt: => Opt[O]) :Opt[O] =
		if (ref eq EmptyToken) alt else this

	/** Similarly to [[net.noresttherein.slang.optional.Opt.orElse orElse]], returns this `Opt` if it is not empty
	  * and `alt` otherwise. The difference is that the alternative value is not lazily computed and guarantees 
	  * no closure would be be created, at the cost of possibly discarding it without use.
	  * @param alt the value to return if this instance is empty.
	  */
	@inline def ifEmpty[O >: T](alt: Opt[O]) :Opt[O] =
		if (ref eq EmptyToken) alt else this

	/** Returns this `Opt` if the condition is false and `Miss` if it is true. This is equivalent
	  * to `this.filterNot(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
	  */
	@inline def orEmptyIf(condition :Boolean) :Opt[T] =
		if (condition) Miss else this

	/** Returns this `Opt` if the condition is true and `Miss` if it is false. This is equivalent
	  * to `this.filter(_ => condition)`, but avoids creating a function and arguably conveys the intent better.
	  */
	@inline def orEmptyUnless(condition :Boolean) :Option[T] =
		if (condition) this else Miss



	/** Conversion to standard Scala [[scala.Option]].
	  * @return `Some(this.get)` if `this.nonEmpty` or `None` otherwise.
	  */
	@inline def toOption :Option[T] = if (ref eq EmptyToken) None else Some(ref.asInstanceOf[T])



	/** Executes the given block for this `Opt`s value if it is not empty. */
	@inline def foreach[O](f :T => O) :Unit = if (!(ref eq EmptyToken)) f(ref.asInstanceOf[T])

	/** Tests if this `Opt` is not empty and its value satisfies the given predicate. */
	@inline def exists(p :T => Boolean): Boolean = !(ref eq EmptyToken) && p(ref.asInstanceOf[T])

	/** Tests if this `Opt` contains no value or its value satisfies the given predicate. */
	@inline def forall(p :T => Boolean): Boolean = (ref eq EmptyToken) || p(ref.asInstanceOf[T])

	/** Tests if this `Opt` is not empty and its value is equal to the given argument. */
	@inline def contains[O >: T](o :O): Boolean = ref == o

	/** Returns a new `Opt` containing this value if it is not empty and its value satisfies the given predicate,
	  * or [[net.noresttherein.slang.optional.Opt.Miss Miss]] otherwise. */
	@inline def filter(p :T => Boolean) :Opt[T] =
		if (!(ref eq EmptyToken) && p(ref.asInstanceOf[T])) this else new Opt(EmptyToken)

	/** Equivalent to `this.`[[net.noresttherein.slang.optional.Opt.filter filter]]`(p)` - a variant for use
	  * in for-comprehensions. */
	@inline def withFilter(p :T => Boolean) :Opt[T] =
		if (!(ref eq EmptyToken) && p(ref.asInstanceOf[T])) this else new Opt(EmptyToken)


	/** Returns a new `Opt` which is empty ''iff'' this value is empty, or one containing the result of applying
	  * the given function to its value otherwise. */
	@inline def map[O](p :T => O) :Opt[O] =
		if (ref eq EmptyToken) new Opt(EmptyToken)
		else new Opt(p(ref.asInstanceOf[T]).asInstanceOf[AnyRef])

	/** Returns the result of applying the given function to the value of this `Opt` if it is not empty, 
	  * or `this` if `this.isEmpty` */
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


	/** Returns an empty `Opt` if this `Opt` is empty the partial function `f` is not defined for its value, 
	  * otherwise applies it and wraps the result it in a new `Opt`. */
	@inline def collect[O](f :PartialFunction[T, O]) :Opt[O] =
		if (ref eq EmptyToken)
			new Opt(EmptyToken)
		else
			new Opt(f.asInstanceOf[PartialFunction[T, AnyRef]].applyOrElse(ref.asInstanceOf[T], EmptyToken))


	/** Applies the given function to the content of this option and returns the result or the provided alternative
	  * if this option is empty. Equivalent to `this map f getOrElse alternative`, but in one step.
	  */
	@inline def mapOrElse[X](f :T => X, alternative: => X) :X =
		if (ref eq EmptyToken) alternative else f(ref.asInstanceOf[T])





	/** An iterator returning this value as the only element if `this.nonEmpty`. */
	@inline def iterator :Iterator[T] =
		if (ref eq EmptyToken) Iterator.empty else Iterator.single(ref.asInstanceOf[T])


	/** Returns `Nil` if this `Opt` is empty or or `this.get::Nil` otherwise. */
	@inline def toList :List[T] = if (ref eq EmptyToken) Nil else ref.asInstanceOf[T]::Nil

	/** Returns an empty list if this `Opt` is empty or a single element list with its value otherwise. */
	@inline def toSeq :Seq[T] = if (ref eq EmptyToken) Nil else ref.asInstanceOf[T]::Nil

}




/** Companion object providing factory methods and extractors working with [[net.noresttherein.slang.optional.Opt Opt]]s.
  * @see [[net.noresttherein.slang.optional.Opt.Miss]]
  * @see [[net.noresttherein.slang.optional.Opt.Got$]]
  */
object Opt {
	@inline final implicit def optToOption[T](opt :Opt[T]) :Option[T] = opt.toOption

	@inline final implicit def optionToOpt[T](opt :Option[T]) :Opt[T] = opt match {
		case Some(x) => new Opt(x.asInstanceOf[AnyRef])
		case _ => new Opt(EmptyToken)
	}



	/** Wraps the given reference in a purely syntactic option-like object erased in the runtime.
	  * Note that the wrapped type is upper bound here by `AnyRef` rather than lower bound by `Null`,
	  * as providing an argument of type `T` excludes the single `AnyRef` subtype which is not the supertype
	  * of `Null`, that is `Nothing`.
	  * @see [[net.noresttherein.slang.optional.Opt]]
	  */
	@inline final def apply[T](value :T) :Opt[T] = 
		if (value == null) Miss else new Opt(value.asInstanceOf[AnyRef])

	/** Returns [[net.noresttherein.slang.optional.Opt.Miss Miss]] - an empty `Opt`. */
	@inline final def empty[T] :Opt[T] = Miss


	/** Factory and a matching pattern for non empty values of [[net.noresttherein.slang.optional.Opt Opt]]. */
	object Got {
		@inline def apply[T](x :T) :Opt[T] = new Opt(x.asInstanceOf[AnyRef])
		@inline final def unapply[T](opt :Opt[T]) :Opt[T] = opt
	}

	/** A special, empty instance of [[net.noresttherein.slang.optional.Opt Opt]] which conforms to any `Opt[T]` type.
	  * It is represented by wrapping a special, private singleton object and all `isEmpty` tests check for
	  * referential equality of the wrapped value with this object. While normally inaccessible from scala code,
	  * it is however still possible to obtain its reference, which makes this class unsuitable for security conscious
	  * applications.
	  * @see [[net.noresttherein.slang.optional.Opt.empty]]
	  */
	@inline final val Miss = new Opt[Nothing](EmptyToken)

	//extends Any => AnyRef out of laziness, allowing it to pass as the argument to applyOrElse
	private[Opt] object EmptyToken extends (Any => AnyRef) with Serializable {
		def apply(ignore :Any) :AnyRef = this
	}
}








