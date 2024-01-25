package net.noresttherein.sugar.util

import scala.annotation.tailrec

import net.noresttherein.sugar
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




/** A mixin trait for any interface `Self` we might want to decorate, aware what its decorator is.
  * The decorator pattern has a flaw in that an implementation of a given interface, for which decorators exist,
  * cannot pass a reference to itself as an argument, because it would lose the decorator(s) wrapping it.
  * This in particular rules out combining decorator and visitor patterns, as a decorator of a visitor traversing
  * a recursive structure, after delegating to the decorated visitor implementation, would never regain control,
  * with subsequent traversal happening solely using the (innermost) decorated instance.
  *
  * This interface addresses this problem by being aware that it might be decorated, keeping the 'effective'
  * implementation on property [[net.noresttherein.sugar.util.Decorable.self self]]. This object can be then passed
  * instead of `this` reference to any method expecting a `Self` instance. For non-decorated instances,
  * `self eq this`, but for those wrapped in a decorator, it is the outermost decorator (as decorator composition
  * is possible). Because this introduces a cyclic dependency with both the decorator and the decorated instance
  * needing a reference to each other, the decorators are not created directly, but instead their constructor
  * function is passed as an argument to [[net.noresttherein.sugar.util.Decorable.decorate decorate]] method
  * of this trait. It is then responsible for creating an exact copy of this instance, but with `self` property
  * initialized to a freshly created decorator.
  *
  * @tparam Self an interface supporting decorators, the self type of this instance.
  * @see [[net.noresttherein.sugar.util.Decorable.AbstractDecorable]]
  * @see [[net.noresttherein.sugar.util.Decorable.Decorator]]
  * @define Self `Decorable`
  * @define self this type
  */
trait Decorable[Self] { this :Self =>
	@inline private def `->this` :Self = this

	/** The 'external' instance, that is this object wrapped in any chain of decorators.  */
	def self :Self = this

	/** Create a copy of yourself, with a new decorator/decorators on top of the decorator stack.
	  * It is superficially equivalent to `decorator(this)`, but `decorator` is actually applied to a copy
	  * of this object, with any existing decorators so that [[net.noresttherein.sugar.util.Decorable.self self]]
	  * property of the result is the returned object itself.
	  * @return A new $Self created by applying the argument function to a copy of this instance,
	  *         such that `result eq result.self`. It should be free of side effects
	  * @see [[net.noresttherein.sugar.util.Decorable.redecorate]]
	  */
	def decorate(decorator :Self => Self) :Self

	/** Discards any current decorators and wraps the underlying, bottom $Self instance in the decorator(s)
	  * created by the argument function. As with [[net.noresttherein.sugar.util.Decorable.decorate decorate]],
	  * result's [[net.noresttherein.sugar.util.Decorable.self self]] property is equal to the returned object itself.
	  * The difference is that where the latter adds a new decorator on top of any already existing,
	  * this method completely replaces the whole decorator stack with the object created by the constructor function.
	  *
	  * This method is useful when there is a need to either remove, or modify one of the decorators currently
	  * present on the stack.
	  * @return A new $Self created by applying the argument function to the underlying $Self instance,
	  *         such that `result eq result.self`.
	  */
	def redecorate(decorator :Self => Self) :Self

	/** [[net.noresttherein.sugar.util.Decorable.redecorate Redecorates]] the underlying instance
	  * in all decorators up to and including this instance. The result is a copy of this instance such
	  * that `copy.self == copy` (and, by extension, `self` for the bottom implementation is also equal to
	  * the returned copy). When called for an undecorated implementation, the result
	  * is the same as [[net.noresttherein.sugar.util.Decorable.undecorated undecorated]].
	  * @return `this.`[[net.noresttherein.sugar.util.Decorable.undecorate undecorate]]`(identity)`.
	  */
	def undecorate :Self = undecorate(identity)

	/** [[net.noresttherein.sugar.util.Decorable.redecorate Redecorates]] the underlying instance
	  * with a copy of this decorator (and all `Self`s underneath it), with `decorator` applied on
	  * top. For undecorated implementations, it is the same as `redecorate(decorator)`.
	  * The difference from [[net.noresttherein.sugar.util.Decorable.redecorate redecorate]] is that decorators
	  * do not simply delegate the call to the [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]
	  * instance, but compose the argument with this decorator's constructor. In other words,
	  * where `redecorate`'s argument is applied directly to the bottom instance, `undecorated`'s argument
	  * is applied to a deep copy of this instance.
	  *
	  * This method is normally called indirectly by parameterless
	  * [[net.noresttherein.sugar.util.Decorable.undecorate undecorate]] called for some decorator
	  * higher on the stack. Another use case allows a decorator to be swapped for another, conflicting one
	  * (for example, of the same class, but with different parameters).
	  * @return [[net.noresttherein.sugar.util.Decorable.redecorate redecorate]]`(`[[net.noresttherein.sugar.util.Decorable.redecoration redecoration]]` andThen decorator)`.
	  */
	def undecorate(decorator :Self => Self) :Self = redecorate(redecoration andThen decorator)

	/** Discards all decorators wrapping the underlying $Self instance, if any. The returned object is a copy
	  * of the originally decorated instance on the bottom of the stack,
	  * with its [[net.noresttherein.sugar.util.Decorable.self self]] property equal to itself.
	  * @return [[net.noresttherein.sugar.util.Decorable.redecorate redecorate]]`(identity)`.
	  */
	def undecorated :Self = redecorate(identity)

	/** The bottommost $Self, unwrapped from all decorators. This does not create a new, undecorated instance
	  * like [[net.noresttherein.sugar.util.Decorable.undecorated undecorated]], but simply returns the one
	  * referenced by the [[net.noresttherein.sugar.util.Decorable.Decorator decorator]] chain, so
	  * [[net.noresttherein.sugar.util.Decorable.self self]] property of the returned $self is also the exact same
	  * instance as `this.self`. Calling any methods directly on the returned object may have unintended consequences,
	  * as this $Self was meant to be used in its current form. It is however equal to discover if the underlying
	  * instance is of a particular class.
	  */
	def underlying  :Self

	/** The function applied to the bottommost instance in order to produce the final $Self
	  * [[net.noresttherein.sugar.util.Decorable.self self]]: `this.self eq this.decorations(this.undecorated)`
	  */
	def decorations :Self => Self

	/** The decoration function up to this instance and no more. If this is the outermost $Self,
	  * it is the same as [[net.noresttherein.sugar.util.Decorable.decorations decorations]]. Bottommost instance
	  * returns identity, and [[net.noresttherein.sugar.util.Decorable.Decorator decorators]] in between
	  * return an intermediate step of `decorations`, such that this instance is the result of applying
	  * the returned function to the bottommost instance, that is `decorated.redecoration andThen decorator(_)`
	  * where `decorated` is the directly wrapped $Self by this decorator, and `decorator` is a constructor
	  * of this decorator accepting the decorated object.
	  */
	def redecoration :Self => Self

	/** Equality on $Self is defined as equality of their [[net.noresttherein.sugar.util.Decorable.self self]]
	  * versions. The default implementation delegates to
	  * [[net.noresttherein.sugar.util.Decorable.undecoratedEquals undecoratedEquals]], which compares
	  * recursively the whole decorator stack down to the underlying $self.
	  */
	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if self eq this => true
		case other :Decorable[Self @unchecked] if canEqual(other) && other.canEqual(this) =>
			undecoratedEquals(other.`->this`)
		case _ => false
	}

	/** Delegates to [[net.noresttherein.sugar.util.Decorable.undecoratedCanEqual undecoratedCanEqual]].
	  * Subclasses, in most cases, should override the latter, rather than this method.
	  */
	def canEqual(that :Any) :Boolean = that match {
		case other :Decorable[Self @unchecked] => this undecoratedCanEqual other.`->this`
		case _ => false
	}

	/** Compares for equality `this` with `that`, ignoring decorators on top.
	  * It is used to compare $self instances not on the top of the decorator stack, in particular the underlying
	  * $Self. This process ignores [[net.noresttherein.sugar.util.Decorable.self self]] property:
	  * two instances may compare equal according to this method as long as their of the same type and have
	  * all other properties equal. Should never call `equals` on another $Self,
	  * as this will result in an infinite recursion.
	  * @return `this eq that`, unless overridden.
	  */
	def undecoratedEquals(that :Self) :Boolean = this eq that.asInstanceOf[AnyRef]

	/** Similar to [[net.noresttherein.sugar.util.Decorable.undecoratedCanEqual undecoratedCanEqual]],
	  * subclasses should override this method instead of the actual
	  * [[net.noresttherein.sugar.util.Decorable.canEqual canEqual]], as the latter will delegate to this method.
	  * @return `getClass == that.getClass`, unless overridden.
	  */
	def undecoratedCanEqual(that :Self) :Boolean = getClass == that.getClass

	/** Hash code compatible with [[net.noresttherein.sugar.util.Decorable.undecoratedEquals undecoratedEquals]],
	  * that is ignoring [[net.noresttherein.sugar.util.Decorable.self self]] property.
	  */
	def undecoratedHashCode :Int = System.identityHashCode(this)
}




@SerialVersionUID(Ver)
object Decorable {

	/** Base class for decorable, bottom $Self implementations. It leaves
	  * [[net.noresttherein.sugar.util.Decorable.AbstractDecorable.redecorate redecorate]] method for subclasses
	  * to implement as their copy constructor. Does not implement any sort of specific equality.
	  * @param decorations a constructor function for [[net.noresttherein.sugar.util.Decorable.self self]]:
	  *                    it accepts the bottom implementation (`this`) as an argument, and wraps it in a sequence
	  *                    of zero or more decorators.
	  * @see [[net.noresttherein.sugar.util.Decorable.BaseDecorable]]
	  */
	abstract class AbstractDecorable[Self <: Decorable[Self]](override val decorations :Self => Self)
		extends Decorable[Self]
	{ this :Self =>
		override val self :Self = decorations(this)
		override def redecoration :Self => Self = identity

		/** Composes the given decorator constructor with whatever the existing decorating function is
		  * (`this.decorators`), and creates a copy of this instance with that function as the `decorators` argument.
		  * As this will create the bottom `Self`, not the external one which should be used, return the `self`
		  * property of the former, which is the fully decorated instance.
		  * @return [[net.noresttherein.sugar.util.Decorable.BaseDecorable.redecorate redecorate]]`(this.decorators andThen decorator)`.
		  */
		override def decorate(decorator :Self => Self) :Self = redecorate(decorator andThen decorator)

		/** A copy constructor of this class: creates a new instance of the same type, and with the same parameters
		  * as this object, but with its [[net.noresttherein.sugar.util.Decorable.BaseDecorable.self self]] property
		  * containing the result of applying the `decorator` argument to the copy. The returned value
		  * is `self`, rather than the copy, that is a reference to the outermost decorator.
		  * @param decorator a constructor function for the new decorator stack built on top of the new $Self instance,
		  *                  possibly identity if no decorators are needed.
		  */
		override def redecorate(decorator :Self => Self) :Self

		override def undecorate :Self = undecorated

		override def undecorate(decorator :Self => Self) :Self = redecorate(decorator)

		override def underlying :Self = this
	}

	/** A full implementation of a 'bottom' `Decorable`, requiring only mixing in an implementation of $Self.
	  * Provides a skeletal implementation of equality which compares only the classes of the objects,
	  * but allows [[net.noresttherein.sugar.util.Decorable.Decorator decorators]] to equal this class
	  * if they so choose by deferring the equality check to their
	  * [[net.noresttherein.sugar.util.Decorable.undecoratedEquals undecoratedEquals]] method.
	  * @param decorations a constructor function for [[net.noresttherein.sugar.util.Decorable.self self]]:
	  *                    it accepts the bottom implementation (`this`) as an argument, and wraps it in a sequence
	  *                    of zero or more decorators.
	  * @param constructor a constructor function for a concrete subclass of this class:
	  *                    it accepts the decorating function which is passed as the argument to this class' constructor
	  *                    and creates a $Self instance. It is assumed that the class of the created object passes
	  *                    the very same function as `constructor` argument to this class.
	  */ //or should we rename it to DecorableBase?
	abstract class BaseDecorable[Self <: Decorable[Self]]
	                            (decorations :Self => Self, constructor :(Self => Self) => Self)
		extends AbstractDecorable[Self](decorations)
	{ this :Self =>
		/** Creates a copy `twin` of this instance, preserving all its properties,
		  * but with [[net.noresttherein.sugar.util.Decorable.self self]] property equal to `decorator(twin)`.
		  * This completely discards the current decorator stack.
		  * @return `constructor(decorator).self`, where `constructor` is the argument of the primary constructor
		  *         of [[net.noresttherein.sugar.util.Decorable.BaseDecorable BaseDecorable]].
		  */
		override def redecorate(decorator :Self => Self) :Self = constructor(decorator).self

		override def undecoratedEquals(that :Self) :Boolean = that match {
			case decorator :Decorator[Self @unchecked] => decorator undecoratedEquals this
			case _ => undecoratedCanEqual(that) && that.undecoratedCanEqual(this)
		}
		override def undecoratedHashCode :Int = getClass.hashCode
	}


	/** Base interface for decorators of `Self`, that is implementations of `Self` which delegate to another instance
	  * of `Self`. It is, at the same time, also a `Decorable`, as it can be wrapped in another decorator.
	  * It does not deviate from the standard decorator pattern implementation, with all responsibility delegated
	  * to the inner-most decorated object. However, instances are not created manually, but by the decorated
	  * instance in a inversion of control:
	  * {{{
	  *     trait Bell extends Decorable[Bell] {
	  *         def ding() :Unit
	  *     }
	  *     class ImpatientBell(override val decorated :Bell) extends Decorator[Bell] {
	  *         override def ding() = { decorated.ding(); decorated.ding(); decorated.ding() }
	  *     }
	  *     def ImpatientBell(decorated :Bell) :Bell = decorated.decorate(new ImpatientBell(_))
	  * }}}
	  *
	  * Most methods defined here simply delegate
	  * to the [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]] instance, unless noted.
	  */
	trait Decorator[Self <: Decorable[Self]] extends Decorable[Self] { this :Self =>
		/** The (directly) wrapped object, which might be the inner-most `Self` implementation, or another decorator.
		  * This property should be overridden by a constructor argument field or a method
		  */ //consider: making it public
		protected def decorated :Self

		private[Decorable] def `->decorated`   :Self = decorated
		private[Decorable] def `->constructor` :Self => Self = decorate
		private[Decorable] def `->this`        :Self = this

		/** Equals [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.sugar.util.Decorable.self self]]. */
		override val self       :Self = decorated.self
		/** Same as [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.sugar.util.Decorable.underlying underlying]]. */
		override def underlying :Self = decorated.underlying

		/** A copy constructor: creates and returns a new instance of the same class and with same parameters
		  * as this decorator, but wrapping the given object.
		  * This is not a recursive method and does ''not'' return [[net.noresttherein.sugar.util.Decorable.self self]],
		  * but the wrapper itself. As such, `self` decorator does not necessarily equal or even contain on the stack
		  * the returned instance, unless this method is used in the very process of constructing the `self` object.
		  */ //consider: making it public; renaming to copy?
		protected def decorate(decorated :Self) :Self

		/** Delegates to [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.sugar.util.Decorable.decorate decorate]].  */
		override def decorate(decorator :Self => Self) :Self = decorated.decorate(decorator)

		/** Delegates to [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.sugar.util.Decorable.redecorate redecorate]].  */
		override def redecorate(decorator :Self => Self) :Self = decorated.redecorate(decorator)

		/** Invokes `decorated.`[[net.noresttherein.sugar.util.Decorable.undecorate undecorate]]`(this.`[[net.noresttherein.sugar.util.Decorable.Decorator.decorate decorate]]` andThen decorator)`. */
		override def undecorate(decorator :Self => Self) :Self =
			decorated.undecorate((decorate :Self => Self) andThen decorator)

		/** Invokes `decorated.`[[net.noresttherein.sugar.util.Decorable.undecorate undecorate]]`(this.`[[net.noresttherein.sugar.util.Decorable.Decorator.decorate decorate]]`)`. */
		override def undecorate :Self = decorated.undecorate(decorate)

		/** Returns `decorated.`[[net.noresttherein.sugar.util.Decorable.decorations decorations]]. */
		override def decorations :Self => Self = decorated.decorations

		/** Returns `decorated.`[[net.noresttherein.sugar.util.Decorable.redecoration redecoration]]` andThen `[[net.noresttherein.sugar.util.Decorable.Decorator.decorate decorate]]. */
		override def redecoration :Self => Self = decorated.redecoration andThen decorate
	}
//
//	//gives access to Decorator's private methods
//	private object Decorator {
//		def decorated[Self <: Decorable[Self]](decorator :Decorator[Self]) = decorator.`->decorated`
//		def constructor[Self <: Decorable[Self]](decorator :Decorator[Self]) = decorator.`->constructor`
//	}

	/** A base class for [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] implementations
	  * which wrap another instance of $Self. Aside from implementing
	  * [[net.noresttherein.sugar.util.Decorable.Decorator.decorate decorate]] with the function passed
	  * as the constructor argument, it also provides a skeleton implementation of equality based on class equality.
	  */
	abstract class BaseDecorator[Self <: Decorable[Self]]
	                            (private[Decorable] override val `->constructor` :Self => Self)
		extends Decorator[Self]
	{ this :Self =>
		@inline private def thisDecorable :Self = this
		protected override def decorate(decorated :Self) :Self = `->constructor`(decorated)

		override def undecoratedEquals(that :Self) :Boolean = that match {
			case _ if that eq this => true
			case other :BaseDecorator[Self @unchecked] =>
				undecoratedCanEqual(other.thisDecorable) && other.undecoratedCanEqual(this) &&
					(decorated undecoratedEquals other.decorated)
			case _ => false
		}
		override def undecoratedCanEqual(that :Self) :Boolean = getClass == that.getClass
		override def undecoratedHashCode :Int = getClass.hashCode * 31 + decorated.undecoratedHashCode
	}




	/** Creates a new zipper pointing ''after'' `decorable.`[[net.noresttherein.sugar.util.Decorable.self self]]. */
	def DecorationsZipper[Self <: Decorable[Self]](decorable :Decorable[Self]) :DecorationsZipper[Self] =
		decorable.self match {
			case decorator :Decorator[Self @unchecked] => DecorationsZipper(decorator, None)
			case undecorated => new DecorationsZipper(undecorated.`->this`, identity, None)
		}

	@inline private def DecorationsZipper[Self <: Decorable[Self]]
	                                     (decorator :Decorator[Self], next :Option[DecorationsZipper[Self]])
			:DecorationsZipper[Self] =
		DecorationsZipper[Self](decorator.`->decorated`, decorator.`->constructor`, next)

	@inline private def DecorationsZipper[Self <: Decorable[Self]]
	                                     (current :Self, next :Self => Self, following :Option[DecorationsZipper[Self]])
			:DecorationsZipper[Self] =
		new DecorationsZipper(current, next, following)
//		current match {
//			case decorator :Decorator[Self @unchecked] => new DecorationsZipper(current, next, following)
//			case bottom => new DecorationsZipperBottom(current, next, following)
//		}


	/** A zipper is an immutable object allowing traversal of a [[net.noresttherein.sugar.util.Decorable Decorable]]
	  * stack. It is able to split function `Decorable.`[[net.noresttherein.sugar.util.Decorable.decorations decorations]]
	  * into constructors for individual [[net.noresttherein.sugar.util.Decorable.Decorator decorators]]
	  * and modify the aforementioned function composition by removing, replacing or adding new decorator constructors
	  * on the stack.
	  *
	  * A new `DecorationsZipper` always points to the outermost
	  * `decorable.`[[net.noresttherein.sugar.util.Decorable.self self]], no matter the `decorable` instance given
	  * as the argument. It can be rewound, 'unwrapping' the decorators,
	  * either [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.<< one by one]]
	  * or [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.|<< completely]] to the bottommost `Decorable`.
	  * This allows to inspect decorators other than the outermost one and apply mutations to the stack.
	  * After modifications are completed, a new copy of the bottommost `Decorable` is can be created
	  * with all decorators listed by the zipper on top using
	  * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.apply apply]]`()`.
	  * Of course, as a consequence of mutations, the decorator stack
	  * as reported by `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
	  * will not match the constructor function represented by this zipper.
	  *
	  * Note that there is no one to one relationship between stacked decorators
	  * and calls to [[net.noresttherein.sugar.util.Decorable.decorate decorate]]
	  * (functions composed into a constructor for `self`). A single function passed to `decorate` may wrap its argument
	  * in multiple decorators, or can be identity. This zipper works on decorators: each step is always
	  * unwraps/wraps in a single decorator, even if a function passed to the zipper itself would create multiple.
	  * Another important consequence is that `decorate` functions may introduce side effects,
	  * aside from decorating their arguments. These effects will be lost in the instance produced by this zipper,
	  * ''even if no mutations are applied'', as long as the zipper is 'rewound' over the problematic function.
	  *
	  * @tparam Self the decorable type for which this zipper works. Note that it requires a bound of `Decorable[Self]`,
	  *              which `Decorable` itself doesn't impose on its type parameter.
	  */ //todo: try to drop identity function when it's used as decoration whenever possible.
	@SerialVersionUID(Ver)
	final class DecorationsZipper[Self <: Decorable[Self]] private[Decorable]
	                             (/** The `Decorable[Self]` instance this zipper is pointing at. */
	                              val point :Self,
	                              /** A constructor for a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]]
	                                * applied directly on top of `point` if this zipper moves
	                                * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.>> forward]].
	                                * If the zipper points at the outermost instance (`continuation == None)`,
	                                * this is an identity function. */
	                              val decoration :Self => Self,
	                              /** The future of this zipper, following the current `decoration`. */
	                              //consider: using Maybe
	                              val continuation :Option[DecorationsZipper[Self]])
	{
		private def this(decorator :Decorator[Self], above :Option[DecorationsZipper[Self]]) =
			this(decorator.`->decorated`, decorator.`->constructor`, above)

		/** A function with decorations applied on top of the instance this zipper is pointing at.
		  * If this zipper is not a result of any modification, then
		  * `this.point.`[[net.noresttherein.sugar.util.Decorable.redecoration redecoration]]` andThen this.decorations`
		  * is always equivalent to `this.point.`[[net.noresttherein.sugar.util.Decorable.decorations decorations]].
		  * If this zipper points to the outermost instance, `identity` function is returned.
		  */
		def decorations :Self => Self = continuation match {
			case Some(zipper) => decoration andThen zipper.decorations
			case _            => decoration
		}

		/** True if this zipper points to the bottommost instance of the `Decorable` stack
		  * and the zipper cannot be rewound any farther.
		  * It is defined as [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * not being a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]]
		  */
		def isBottom :Boolean = !point.isInstanceOf[Decorator[_]]

		/** True if this zipper points to the outermost instance of the decorable stack,
		  * that is [[net.noresttherein.]] */
		def isTop    :Boolean = continuation.isEmpty

		/** Applies all preceding and following decorators to the bottommost instance and returns its
		  * [[net.noresttherein.sugar.util.Decorable.self self]] representative.
		  * Decorators preceding are defined here as the constructors of all
		  * [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] instances stacked one on top of each other
		  * in `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * The following decorators is the function
		  * `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decorations decorations]],
		  * that is a composition of the decorator constructor
		  * `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]] with
		  * those in all following `DecorationsZipper` instances
		  * in `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]].
		  * @return `this.point.`[[net.noresttherein.sugar.util.Decorable.redecorate redecorate]]`(`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.|<< |<<]]`.decorations)`
		  */
		def apply() :Self = point.redecorate(|<<.decorations)

		/** Advances the zipper,
		  * returning its [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]].
		  */
		@throws[NoSuchElementException]("if the zipper points to the outermost instance (continuation.isEmpty).")
		def >> :DecorationsZipper[Self] = continuation match {
			case Some(zipper) => zipper
			case _ => throw new NoSuchElementException("Zipper pointing to the outermost instance.")
		}
		/** Advances this zipper, wrapping its [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * in the decorators, until `pred(this.point)` is true,
		  * or [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]] stack is empty.
		  * The first argument to `pred` is `continuation.get.point` (assuming `continuation.isDefined)`,
		  * not `this.point`.
		  */
		def >>(pred :Self => Boolean) :DecorationsZipper[Self] = {
			@tailrec def up(zipper :DecorationsZipper[Self]) :DecorationsZipper[Self] = zipper.continuation match {
				case Some(next) if pred(next.point) => up(next)
				case _ => zipper
			}
			up(this)
		}
		/** Applies all decorator constructors ahead of this zipper,
		  * starting with `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]]
		  * and following with `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]].
		  * If the full decorator stack is empty
		  * (`this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * is not a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] and `continuation.isEmpty`),
		  * then simply this instance is returned and its `decoration` is an identity function.
		  * @see [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.>>]]
		  */
		def >>| :DecorationsZipper[Self] = {
			@tailrec def rec(current :DecorationsZipper[Self]) :DecorationsZipper[Self] =
				current.continuation match {
					case Some(next) => rec(next)
					case _          => current
				}
			rec(this)
		}

		/** Assumes that the `Decorable` instance immediately below this zipper,
		  * is a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] and unwraps it,
		  * creating a zipper pointing to
		  * `this.point.`[[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]], with the constructor
		  * for the `point`'s class being the immediately following decorator.
		  */
		@throws[NoSuchElementException]("if this.point is not a Decorator.")
		def << :DecorationsZipper[Self] = point match {
			case decorator :Decorator[Self @unchecked] =>
				new DecorationsZipper[Self](decorator, Some(this))
			case _ =>
				throw new NoSuchElementException("Zipper pointing to the bottom instance.")
		}
		/** Unwraps `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]] from all
		  * wrapping [[net.noresttherein.sugar.util.Decorable.Decorator decorators]], until the argument $Self
		  * is reached, pushing the constructors of every unwrapped `decorator` onto the
		  * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]] stack.
		  * If `point` is not found, `No` is returned.
		  */
		def <<(point :Decorable[Self]) :Option[DecorationsZipper[Self]] = {
			val End = point
			@tailrec def unwrap(zipper :DecorationsZipper[Self]) :Option[DecorationsZipper[Self]] =
				zipper.point match {
					case End => Some(zipper)
					case decorator :Decorator[Self @unchecked] => unwrap(new DecorationsZipper(decorator, Some(zipper)))
					case _ => None
				}
			unwrap(this)
		}
		/** Applies `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.<< <<]] recursively,
		  * for as long as `pred(this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]`)`
		  * is true, or until `point` is not a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]].
		  */
		def <<(pred :Self => Boolean) :DecorationsZipper[Self] = {
			@tailrec def unwrap(zipper :DecorationsZipper[Self]) :DecorationsZipper[Self] =
				zipper.point match {
					case decorator :Decorator[Self @unchecked] if pred(zipper.point) =>
						unwrap(new DecorationsZipper(decorator, Some(zipper)))
					case _ => zipper
				}
			unwrap(this)
		}
		/** Moves the zipper all the way to the bottom `Decorable`, putting the constructors for all decorators
		  * in a zipper queue [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]].
		  * This method has no effect if `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * is not a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]].
		  * If the full decorator stack is empty
		  * (`this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * is not a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] and `continuation.isEmpty`),
		  * then this instance is returned,
		  * and its [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]]
		  * is an identity function.
		  * @see [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.<<]]
		  */
		def |<< :DecorationsZipper[Self] = {
			@tailrec def rec(current :Self, next :Self => Self, up :Option[DecorationsZipper[Self]])
					:DecorationsZipper[Self] =
				current match {
					case decorator :Decorator[Self @unchecked] =>
						rec(decorator.`->decorated`, decorator.`->constructor`, Some(new DecorationsZipper(decorator, up)))
					case _ if current eq point =>
						this
					case _ =>
						new DecorationsZipper(current, next, up)
				}
			rec(point, decoration, continuation)
		}

		/** Removes from the stack the decorator applied on top of
		  * `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]],
		  * that is `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]] constructor.
		  * If `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]] exists,
		  * it is returned, with its `point` being replaced with `this.point`. If not, the returned zipper
		  * represents an empty stack: `point == this.point && decoration == identity && continuation.isEmpty`.
		  */
		@throws[UnsupportedOperationException]("if the zipper points to the outermost instance (continuation.isEmpty).")
		def >>- :DecorationsZipper[Self] = continuation match {
			case Some(next) =>
				DecorationsZipper(point, next.decoration, next.continuation)
			case _ =>
				throw new UnsupportedOperationException("Cannot remove a decorator: zipper pointing to the outermost instance")
		}
		/** Removes from the stack all decorators applied on top of
		  * `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]],
		  * starting with `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]]
		  * and following with the constructors listed by zipper instances
		  * in `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]].
		  * @return a zipper `z` such that `z.point == this.point && z.decoration == identity && z.continuation == None`.
		  * @see [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.>>- >>-]]
		  */
		def >>-| :DecorationsZipper[Self] = DecorationsZipper(point, identity, None)

		/** Removes from the stack the decorator to which this zipper points. After the operation,
		  * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]] is the $Self instance decorated
		  * by `this.point`, while [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]]
		  * and [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]] remain the same.
		  * @throws UnsupportedOperationException
		  *         If `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]] is not
		  *         a [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]].
		  */
		@throws[UnsupportedOperationException]("if this.point is not a Decorator.")
		def -<< :DecorationsZipper[Self] = point match {
			case decorator :Decorator[Self @unchecked] =>
				DecorationsZipper(decorator.`->decorated`, decoration, continuation)
			case _ =>
				throw new UnsupportedOperationException("Cannot remove the bottommost instance.")
		}
		/** Unwraps the underlying $Self, removing all [[net.noresttherein.sugar.util.Decorable.Decorator decorators]]
		  * below this zipper's [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]].
		  * This recursive process ends when `this.point` is not an instance of `Decorator`.
		  */
		def |-<< :DecorationsZipper[Self] =
			new DecorationsZipper(bottom(point), decoration, continuation)

		/** Replaces the [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]] following
		  * this zipper's [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]] with the given
		  * decorator (or other $Self transformation).
		  */
		def replace(decoration :Self => Self) :DecorationsZipper[Self] =
			DecorationsZipper(point, decoration, continuation)

		/** Replaces all decorators on top of this zipper's
		  * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]] with the given decorator(s).
		  * The function is split into constructors for individual decorators, and the position of the zipper does
		  * not change, as long as `decoration` does not modify its argument (that is, purely wraps it in one or more
		  * decorators). This is done by first applying `decorators(point)`, and then recursively unwrapping
		  * every [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]], until either `this.point`
		  * or a $Self instance other than a decorator is reached. Each unwrapped decorator is then turned into
		  * an individual constructor function following this zipper.
		  *
		  * Due to the nature of the implementation, if `decoration` contains any conditional logic depending
		  * on the properties of its argument or other state caught in its closure, or if transforms the argument
		  * rather than simply wrapping it in decorators, this logic will be lost once the zipper is rewound to the top
		  * of the stack. If you wish to treat `decoration` as an atomic operation, preserved in its exact form, use
		  * `decorators `[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.inject_: inject_:]]` this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.>>-| >>-|]]
		  * instead. Note however that in that case, the zipper will be pointing at the top of the stack, and any
		  * attempt to rewind it will have the same effect as in this method.
		  */
		def replaceAll(decorators :Self => Self) :DecorationsZipper[Self] =
			unwrap(decorators(point), identity, None)

		/** Replaces the decorator this zipper is pointing at.
		  * @return [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.<< <<]]` `[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.replace replace]]` decorators`.
		  */
		@throws[UnsupportedOperationException]("if this.point is not a Decorator.")
		def replace_:(decorators :Self => Self) :DecorationsZipper[Self] =
			<< replace decorators

		/** Replaces the constructors of all decorators below this zipper with the given function, while leaving
		  * all decorator constructors above it unchanged.
		  * The [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.poin point]] of the new zipper will be
		  * `decoration(`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.-<< -<<]]`.point)`, while
		  * `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]] and
		  * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.continuation continuation]] remain unchanged.
		  */
		def replaceAll_:(decoration :Self => Self) :DecorationsZipper[Self] =
			DecorationsZipper(decoration(bottom(point)), this.decoration, continuation)

		/** Replaces the $Self instance at the [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]]
		  * of this zipper with `predecessor`.
		  * @return `new DecorationsZipper(predecessor, decoration, continuation)`.
		  */
		def replace(predecessor :Self) :DecorationsZipper[Self] =
			new DecorationsZipper(predecessor, decoration, continuation)


		/** Pushes the given decorator(s) into the stack directly above this zipper,
		  * between `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.point point]] and
		  * `this.`[[net.noresttherein.sugar.util.Decorable.DecorationsZipper.decoration decoration]].
		  * This method works by applying `decoration(point)`, and then
		  * [[net.noresttherein.sugar.util.Decorable.DecorationsZipper.<< rewinding]] through all of the decorators
		  * created by the function, until `this.point`,
		  * or a non [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] instance is reached.
		  * Assuming `decoration` only wraps the argument in one or more decorators, rather than performs an arbitrary
		  * transformation, when the method returns the zipper's `point` remains unchanged, while `decoration`
		  * is the constructor of the `Decorator` directly wrapping it in `decoration(point)`, or,
		  * if `decoration == identity`, `this.decoration`.
		  */
		def inject(decoration :Self => Self) :DecorationsZipper[Self] =
			unwrap(decoration(point), this.decoration, continuation)

		/** Pushes the given decorator(s) into the stack directly below this zipper.
		  * @return `DecorationsZipper(decorator(point`
		  */
		def inject_:(decoration :Self => Self) :DecorationsZipper[Self] =
			DecorationsZipper(decoration(point), this.decoration, continuation)


		@tailrec private def unwrap(current :Self, next :Self => Self, up :Option[DecorationsZipper[Self]])
				:DecorationsZipper[Self] =
			current match {
				case _ if current eq point => new DecorationsZipper(point, next, up)
				case decorator :Decorator[Self @unchecked] =>
					unwrap(decorator.`->decorated`, decorator.`->constructor`,
					       Some(new DecorationsZipper(decorator.`->this`, next, up)))
				case bottom => DecorationsZipper[Self](bottom, next, up)
			}

		@tailrec private def bottom(current :Self) :Self = current match {
			case decorator :Decorator[Self @unchecked] => bottom(decorator.`->decorated`)
			case _ => current
		}
	}

}
