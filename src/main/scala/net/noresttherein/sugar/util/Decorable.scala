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
	  * The difference from `redecorate` is that decorators do not simply delegate the call to
	  * the [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]] instance, but compose it
	  * with this decorator's constructor.
	  * This method is normally called indirectly by parameterless `undecorate` called for some decorator
	  * higher on the stack.
	  * @return `redecorate(copy(_) andThen decorator)`, where `copy` is a copy constructor of this class
	  *         which was used to create this instance in the first place.
	  */
	def undecorate(decorator :Self => Self) :Self

	/** Discards all decorators wrapping the underlying $Self instance, if any. The returned object is a copy
	  * of the originally decorated instance on the bottom of the stack,
	  * with its [[net.noresttherein.sugar.util.Decorable.self self]] property equal to itself.
	  * @return [[net.noresttherein.sugar.util.Decorable.redecorate redecorate]]`(identity)`.
	  */
	def undecorated :Self = redecorate(identity)
}




@SerialVersionUID(sugar.ver)
object Decorable {

	/** Base class for decorable, bottom $Self implementations. It leaves
	  * [[net.noresttherein.sugar.util.Decorable.AbstractDecorable.redecorate redecorate]] method for subclasses
	  * to implement as their copy constructor.
	  * @param decorators  a constructor function for [[net.noresttherein.sugar.util.Decorable.self self]]:
	  *                    it accepts the bottom implementation (`this`) as an argument, and wraps it in a sequence
	  *                    of zero or more decorators.
	  * @see [[net.noresttherein.sugar.util.Decorable.BaseDecorable]]
	  */
	abstract class AbstractDecorable[Self <: Decorable[Self]](decorators :Self => Self)
		extends Decorable[Self]
	{ this :Self =>
		override val self :Self = decorators(this)

		/** Composes the given decorator constructor with whatever the existing decorating function is
		  * (`this.decorators`), and creates a copy of this instance with that function as the `decorators` argument.
		  * As this will create the bottom `Self`, not the external one which should be used, return the `self`
		  * property of the former, which is the fully decorated instance.
		  * @return [[net.noresttherein.sugar.util.Decorable.BaseDecorable.redecorate redecorate]]`(this.decorators andThen decorator)`.
		  */
		override def decorate(decorator :Self => Self) :Self = redecorate(decorators andThen decorator)

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

		override def undecorate(decorator :Self => Self) :Self =
			decorated.undecorate((decorate :Self => Self) andThen decorator)

//		override def undecorate :Self = decorated.undecorate(decorate)
	}

	abstract class BaseDecorator[Self <: Decorable[Self]](constructor :Self => Self)
		extends Decorator[Self]
	{ this :Self =>
		protected override def decorate(decorated :Self) :Self = constructor(decorated)
	}
}

