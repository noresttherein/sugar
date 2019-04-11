package net.noresttherein.slang.witness



/** A value class wrapper over any value with an additional associated label type distinguishing it from other instances.
  * It allows having multiple implicit values of the same type `T`, but distinguished by different labels.
  * By wrapping the intended values in `Labelled` instances the actual types of the implicit values differ, allowing
  * the client code to declare which value is of interested by the label type. This class is covariant both regarding
  * the wrapped type `T` and the `Label` type, so it is possible to declare implicit values with different generalisation
  * levels leveraging the subtyping of labels. While a basic framework for creating unique label types using the
  * [[net.noresttherein.slang.witness.Labelled.Label]] class exists in the companion object, any type can be used for
  * the label. In particular, the singleton type of the wrapped object is a good unique label, although this level
  * of coupling will be often undesirable.
  * @param  get   the wrapped value.
  * @tparam T     the business type of this object.
  * @tparam Label any type, possibly phantom types, to use as a label differentiating this value from other possible
  *               wrappers of `T`.
  * @see [[net.noresttherein.slang.witness.Labelled.Label]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Labelled[+T, +Label](/** The labelled value.*/val get :T) extends AnyVal {
//	/** An alias for the label type given as the type parameter. */
//	type ## = Label
}



/** Factory of labelled (implicit by design) values as well as labels which can be used with them. */
object Labelled {

	/** Attach the label given as the type parameter to the argument value, creating a labelled value `T Labelled Label`.
	  * @tparam Label label type to attach to an object.
	  * @return an intermediate builder object which `apply` method accepts arguments of any type and returns the final result.
	  */
	@inline def apply[Label] :LabelImplicit[Label] = new LabelImplicit[Label] {}

	/** Labels the first argument with the singleton type of the second argument. */
	@inline def apply[T](value :T, label :AnyRef) :T Labelled label.type = new Labelled(value)

	/** Labels the given value with its singleton type to distinguish it from other values of type `T`. */
	@inline def apply[T <: AnyRef](value :T) :T Labelled value.type = new Labelled(value)




	/** An intermediate builder for `Labelled` values existing to separate the type arguments for the label and the value:
	  * the latter can be easily inferred while the former is usually specified explicitly.
	  * @tparam Label the label to attach to all wrapped values.
	  */
	trait LabelImplicit[Label] extends Any {
		/** Attach the `Label` type parameter specified previously to the given value, creating an instance
		  * of `T Labelled Label` wrapping the argument.
		  */
		@inline def apply[T](value :T) :T Labelled Label = new Labelled(value)
	}





	/** An ephemeral trait wrapping an abstract type definition `##` to be used as unique labels for
	  * [[net.noresttherein.slang.witness.Labelled!]].
	  * {{{
	  *     val label1 = Labelled.Label
	  *     val label2 = Labelled.Label
	  *
	  *     class VeryImportantModule
	  *
	  *     implicit val module1 = Labelled[label1.##](new VeryImportantModule)
	  *     implicit val module2 = Labelled[label2.##](new VeryImportantModule)
	  *
	  *     implicitly[VeryImportantModule Labelled label1.##]
	  * }}}
	  * @see
	  */
	trait Label extends Any {
		/** A unique label type differing between all `Label` instances. */
		type ##
	}

	/** A convenience type alias narrowing the `Label` type providing the upper bound for its `##` type member serving
	  * as actual labels. Can be used to create a label hierarchy.
	  */
	type SubLabel[+UB] = Label { type ## <: UB }


	/** Create a new Label wrapper over a unique `##` type to be used by `Labelled` objects.
	  * @see [[net.noresttherein.slang.witness.Labelled.Label!]]
	  */
	@inline def Label :Label = SubLabel[Label]

	/** Create label with an upper bound as a part of a larger label hierarchy.
	  * @tparam UB the upper bound for label type `##` wrapped by the returned object
	  * @see [[net.noresttherein.slang.witness.Labelled.Label]]
	  */
	@inline def SubLabel[UB] :SubLabel[UB] = new Label { type ## = UB }



}

