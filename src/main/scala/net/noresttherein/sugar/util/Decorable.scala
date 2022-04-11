package net.noresttherein.sugar.util




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
  */
trait Decorable[Self] { this :Self =>
	/** The 'external' instance, that is this object wrapped in any chain of decorators.  */
	def self :Self = this

	/** Create a copy of yourself, with its `self` property set to the result of applying itself
	  * to the given function.
	  * @return the result of applying `decorator` to a copy of this instance.
	  */
	def decorate(decorator :Self => Self) :Self
}




object Decorable {
	/** Full implementation of `Decorable`, requiring only mixing in an implementation of `Self`.
	  * @param wrap        constructor function for `self`: it accepts the bottom implementation (`this`) as an argument,
	  *                    and wraps it in a sequence of 0 or more decorators.
	  * @param constructor constructor function for the implementation of Self extending this class:
	  *                    it accepts the decorating function and creates an instance of an extending class.
	  */
	abstract class AbstractDecorable[Self <: Decorable[Self]](wrap :Self => Self, constructor :(Self => Self) => Self)
		extends Decorable[Self]
	{ this :Self =>
		override val self :Self = wrap(this)
		/** Composes the given decorator constructor with whatever the existing decorating function is (`this.wrap`)
		  * and creates a copy of this instance with that function as the `wrap` argument.
		  * As this will create the bottom `Self`, not the external one which should be used, return the `self`
		  * property of the former, which is the fully decorated instance.
		  */
		override def decorate(decorator :Self => Self) :Self = constructor(wrap andThen decorator).self
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

		/** Delegates to [[net.noresttherein.sugar.util.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.sugar.util.Decorable.decorate decorate]].  */
		override def decorate(decorator :Self => Self) :Self = decorated.decorate(decorator)
	}
}

