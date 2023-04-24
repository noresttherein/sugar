package net.noresttherein.sugar.witness

import scala.annotation.implicitNotFound




/** A value class wrapper over any value with an additional associated label type distinguishing it from other instances.
  * It allows having multiple implicit values of the same type `T`, but distinguished by different labels.
  * By wrapping the intended values in `Labelled` instances the actual types of the implicit values differ, allowing
  * the client code to declare which value is of interested by the label type. This class is covariant both regarding
  * the wrapped type `T` and the `Label` type, so it is possible to declare implicit values with different
  * generalisation levels leveraging the subtyping of labels. Any type can be used for the label; in particular,
  * the singleton type of the wrapped object is a good unique label, although this level of coupling
  * will be often undesirable.
  * @param  get   the wrapped value.
  * @tparam T     the business type of this object.
  * @tparam Label any type, possibly phantom types, to use as a label differentiating this value from other possible
  *               wrappers of `T`.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@implicitNotFound("No implicit ${T} labelled ${Label}")
@SerialVersionUID(1L)
class Labeled[+T, +Label](/** The labelled value.*/val get :T) extends AnyVal with Serializable



/** Factory of labelled (implicit by design) values as well as labels which can be used with them. */
@SerialVersionUID(Ver)
object Labeled {

	/** Attach the label given as the type parameter to the argument value, creating a labelled value `T Labelled Label`.
	  * @tparam Label label type to attach to an object.
	  * @return an intermediate builder object whose `apply` method accepts arguments of any type and returns the final result.
	  */
	@inline def apply[Label] :LabelImplicit[Label] = new LabelImplicit[Label] {}

	/** Labels the first argument with the singleton type of the second argument. */
	@inline def apply[T](value :T, label :AnyRef) :T Labeled label.type = new Labeled(value)

	/** Labels the given value with its singleton type to distinguish it from other values of type `T`. */
	@inline def singleton[T <: AnyRef](value :T) :T Labeled value.type = new Labeled(value)

	/** Resolves an implicit value associated with the given label in two steps. The first is here and serves
	  * only to separate the label type parameter. The second is the `apply[T]()` method of the returned object,
	  * which resolves an implicit value of `T Labelled Label`. This separation is dictated by the fact that the type
	  * parameter `T` can in most cases be inferred from either present implicit values or the expected type.
	  * @usecase `val component :Component = Labelled.get[Label]()` will search for an implicit value of
	  *         `Component Labelled Label`.
	  */
	@inline def summon[Label] :ResolveImplicit[Label] = new ResolveImplicit[Label] {}



	/** An intermediate builder for `Labelled` values existing to separate the type arguments for the label and the value:
	  * the latter can be easily inferred while the former is usually specified explicitly.
	  * @tparam Label the label to attach to all wrapped values.
	  */
	trait LabelImplicit[Label] extends Any {
		/** Attach the `Label` type parameter specified previously to the given value, creating an instance
		  * of `T Labelled Label` wrapping the argument.
		  */
		@inline def apply[T](value :T) :T Labeled Label = new Labeled(value)
	}


	/** An intermediate functional object with a method returning the implicit value for `T Labelled Label`. */
	trait ResolveImplicit[Label] extends Any {
		/** Resolve the implicit value for `T Labelled Label`. As the type parameter `T` is separated from the
		  * label type, it can be inferred based on the expected type or present `Labelled` implicits.
		  */
		@inline def apply[T]()(implicit value :T Labeled Label) :T = value.get
	}

}

