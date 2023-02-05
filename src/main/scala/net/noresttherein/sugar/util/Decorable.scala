package net.noresttherein.sugar.util

import net.noresttherein.sugar




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
  * @define Self `Self`
  * @define self this general type
  */
trait Decorable[Self] { this :Self =>
	@inline private def thisDecorable :Self = this

	/** The 'external' instance, that is this object wrapped in any chain of decorators.  */
	def self :Self = this

	/** Create a copy of yourself, with a new decorator/decorators on top of the decorator stack.
	  * It is superficially equivalent to `decorator(this)`, but `decorator` is actually applied to a copy
	  * of this object, with any existing decorators so that [[net.noresttherein.sugar.util.Decorable.self self]]
	  * property of the result is the returned object itself.
	  * @return A new $Self created by applying the argument function to a copy of this instance,
	  *         such that `result eq result.self`.
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
	  * @return `this.undecorate(identity)`.
	  */
	def undecorate :Self = undecorate(identity)

	/** [[net.noresttherein.sugar.util.Decorable.redecorate Redecorates]] the underlying instance
	  * with a copy of this decorator (and all `Self`s underneath it), with `decorator` applied on
	  * top. For undecorated implementations, it is the same as `redecorate(decorator)`.
	  * The difference from [[net.noresttherein.sugar.util.Decorable.redecorate redecorate]] is that decorators
	  * do not simply delegate the call to the [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]
	  * instance, but compose it with this decorator's constructor. In other words, where `redecorate`'s argument
	  * is applied directly to the bottom instance, `undecorated`'s argument is applied to a copy of this instance.
	  *
	  * This method is normally called indirectly by parameterless `undecorate` called for some decorator
	  * higher on the stack. Another use case allows a decorator to be swapped for another, conflicting one
	  * (for example, of the same class, but with different parameters).
	  * @return `redecorate(copy(_) andThen decorator)`, where `copy` is a copy constructor of this class
	  *         which was used to create this instance in the first place.
	  */
	def undecorate(decorator :Self => Self) :Self

	/** Discards all decorators wrapping the underlying $Self instance, if any. The returned object is a copy
	  * of the originally decorated instance on the bottom of the stack,
	  * with its [[net.noresttherein.sugar.util.Decorable.self self]] property equal to itself.
	  * @return [[net.noresttherein.sugar.util.Decorable.redecorate redecorate]]`(identity)`.
	  */ //consider: renaming to bottom
	def undecorated :Self = redecorate(identity)


	/** The function applied to the bottommost instance in order to produce the final $Self
	  * [[net.noresttherein.sugar.util.Decorable.self self]]: `this.self eq this.decorator(this.undecorated)`
	  */
	def decorator :Self => Self

	/** Equality on $Self is defined as equality of their [[net.noresttherein.sugar.util.Decorable.self self]]
	  * versions. The default implementation delegates to
	  * [[net.noresttherein.sugar.util.Decorable.undecoratedEquals undecoratedEquals]], which compares
	  * recursively the whole decorator stack down to the underlying $self.
	  */
	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if self eq this => true
		case other :Decorable[Self @unchecked] if canEqual(other) && other.canEqual(this) =>
			undecoratedEquals(other.thisDecorable)
		case _ => false
	}

	/** Delegates to [[net.noresttherein.sugar.util.Decorable.undecoratedCanEqual undecoratedCanEqual]].
	  * Subclasses, in most cases, should override the latter, rather than this method.
	  */
	def canEqual(that :Any) :Boolean = that match {
		case other :Decorable[Self @unchecked] => this undecoratedCanEqual other.thisDecorable
		case _ => false
	}

	/** Compares for equality `this` with `that`, ignoring decorators on top.
	  * It is used to compare reforms not on the top of the decorator stack, in particular the underlying
	  * $Self. This process ignores [[net.noresttherein.sugar.util.Decorable.self self]] property:
	  * two instances may compare equal according to this method as long as their of the same type and have
	  * all other properties equal. Should never call `equals` as this will result in infinite recursion.
	  *
	  * The default behaviour is to compare referential equality as `AnyRef.equals`.
	  */
	def undecoratedEquals(that :Self) :Boolean = this eq that.asInstanceOf[AnyRef]

	/** Similar to [[net.noresttherein.sugar.util.Decorable.undecoratedCanEqual undecoratedCanEqual]],
	  * subclasses should override this method instead of the actual
	  * [[net.noresttherein.sugar.util.Decorable.canEqual canEqual]], as the latter will delegate to this method.
	  */
	def undecoratedCanEqual(that :Self) :Boolean = getClass == that.getClass

	/** Hash code compatible with [[net.noresttherein.sugar.util.Decorable.undecoratedEquals undecoratedEquals]],
	  * that is ignoring [[net.noresttherein.sugar.util.Decorable.self self]] property.
	  */
	def undecoratedHashCode :Int = System.identityHashCode(this)
}




@SerialVersionUID(sugar.ver)
object Decorable {

	/** Base class for decorable, bottom $Self implementations. It leaves
	  * [[net.noresttherein.sugar.util.Decorable.AbstractDecorable.redecorate redecorate]] method for subclasses
	  * to implement as their copy constructor. Does not implement any sort of specific equality.
	  * @param decorators  a constructor function for [[net.noresttherein.sugar.util.Decorable.self self]]:
	  *                    it accepts the bottom implementation (`this`) as an argument, and wraps it in a sequence
	  *                    of zero or more decorators.
	  * @see [[net.noresttherein.sugar.util.Decorable.BaseDecorable]]
	  */
	abstract class AbstractDecorable[Self <: Decorable[Self]](override val decorator :Self => Self)
		extends Decorable[Self]
	{ this :Self =>
		override val self :Self = decorator(this)

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
	}

	/** Full implementation of `Decorable`, requiring only mixing in an implementation of $Self.
	  * @param decorators  a constructor function for [[net.noresttherein.sugar.util.Decorable.self self]]:
	  *                    it accepts the bottom implementation (`this`) as an argument, and wraps it in a sequence
	  *                    of zero or more decorators.
	  * @param constructor a constructor function for the implementation of $Self extending this class:
	  *                    it accepts the decorating function and creates an instance of an extending class.
	  */
	abstract class BaseDecorable[Self <: Decorable[Self]]
	                            (decorators :Self => Self, constructor :(Self => Self) => Self)
		extends AbstractDecorable[Self](decorators)
	{ this :Self =>
		/** Creates a copy `twin` of this instance, preserving all its parameters,
		  * but with [[net.noresttherein.sugar.util.Decorable.self self]] property equal to `decorator(twin)`.
		  * This completely discards the current decorator stack.
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
		/** The (directly) wrapped object, which might be the inner-most `Self` implementation, or another decorator. */
		protected val decorated :Self

		/** Equals [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.sugar.util.Decorable.self self]]. */
		override val self :Self = decorated.self

		/** A copy constructor: creates and returns a new instance of the same class and with same parameters
		  * as this decorator, but wrapping the given object.
		  * This is not a recursive method and does ''not'' return [[net.noresttherein.sugar.util.Decorable.self self]],
		  * but the wrapper itself. As such, `self` decorator does not necessarily equal or even contain on the stack
		  * the returned instance, unless this method is used in the very process of constructing the `self` object.
		  */
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

		override def decorator :Self => Self = decorated.decorator
	}

	/** A base class for [[net.noresttherein.sugar.util.Decorable.Decorator Decorator]] implementations
	  * which wrap another instance of $Self. Aside from implementing
	  * [[net.noresttherein.sugar.util.Decorable.Decorator.decorate decorate]] with the function passed
	  * as the constructor argument, it also provides a skeleton implementation of equality based on class equality.
	  */
	abstract class BaseDecorator[Self <: Decorable[Self]](constructor :Self => Self) extends Decorator[Self] {
		this :Self =>
		@inline private def thisDecorable :Self = this
		protected override def decorate(decorated :Self) :Self = constructor(decorated)

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
}
