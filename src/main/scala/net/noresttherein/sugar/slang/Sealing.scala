package net.noresttherein.sugar.slang

import net.noresttherein.sugar.slang.Sealing.NotSealed
import net.noresttherein.sugar.{Ver, slang}




/** Utilities which simulate the functionality of the `sealed` modifier for declarations such as `def`, `val` and `var`,
  * as well for 'sealing' a class or trait to a package (that is, preventing classes from outside
  * of the package from extending them).
  *
  * Declarations with `protected[scope]` or `private[scope]` visibility, despite being unusable outside
  * the specified scope, can be somewhat counterintuitively overridden/implemented by extending classes from any package
  * with a matching public definition, leading to leaking private API. The easiest way to address is to make
  * the declarations `final`, but this is not always possible if an interface type is extended by implementation types
  * from the protected scope. A `private[scope]`/`protected[scope]` class/trait cannot be used or extended outside
  * the declared scope, so methods with arguments or return types referencing types of restricted visibility are
  * safe from accidental overriding in client code, but this still does not cover all use cases. This class
  * expands on the aforementioned protected types by declaring an artificial implicit argument type
  * [[slang.Sealing.Seal Seal]] and a value class [[slang.Sealing.Sealed Sealed]]`[T]`
  * wrapper for any type. It can be set up be creating an instance in the desired package/scope with visibility
  * restricted to that scope:
  * {{{
  *     package object scope {
  *         private[scope] val seals = new Sealing
  *     }
  * }}}
  * All methods which require sealing can be now declared with an additional implicit parameter, which will make
  * them impossible to override by classes/traits from outside `scope`:
  * {{{
  *     package scope {
  *         class Public { protected[scope] def notPublic(implicit seals.Sealing) :String = "no." }
  *     }
  * }}}
  * Similarly, any value of a public type `T` which should remain protected, but cannot be `final`,
  * can be lift to `seals.Sealed[T]`:
  * {{{
  *     package scope {
  *         class Public { protected[scope] val notPublic :seals.Sealed[Int] = 42 }
  *     }
  * }}}
  *
  * This mechanism can be also used to simulate a `sealed[package]` modifier:
  * {{{
  *     package scope {
  *         trait Closed {
  *             protected[scope] def seal(s :seals.Sealed) :Unit
  *         }
  *     }
  *     //in another file:
  *     package scope {
  *         class Extension extends Closed {
  *             protected[scope] override def seal(s :seals.Sealed) :Unit = ()
  *         }
  *     }
  * }}}
  * Note that, in the above example, `Sealed` must be a methods parameter rather than return type,
  * as in the latter case it could still be overridden by a method returning `Nothing`.
  *
  * Implicit boxing and unboxing helps reduce the syntax cost of using a wrapper type.
  *
  * @author Marcin Mościcki
  */ //consider: moving to util.
trait Sealing {

	/** An artificial class used to prevent overriding of a method by classes outside of the visibility scope
	  * of the enclosing [[Sealing Sealing]] instance. It circumvents the problem
	  * that a ''package protected'' or ''package private'' declaration of a base class can be overriden/implemented
	  * with a matching public definition by a subclass from any package, which is usually undesirable.
	  * It serves two functions:
	  *   1. 'Sealing' of traits/classes to a package, rather than file scope.
	  *      This can be accomplished by introducing an abstract method with the (transitive) visibility of this trait,
	  *      returning an instance of this class in the 'sealed' type, and implementing it in the 'open' traits/classes
	  *      declared within the same scope.
	  *      {{{
	  *          package seal {
	  *             private[seal] val seals = new Sealing
	  *             trait Sealed {
	  *                 protected[seal] def seal :seals.Seal
	  *             }
	  *             trait Open extends Sealed {
	  *                 protected[seal] override def seal = seals.Seal()
	  *             }
	  *          }
	  *      }}}
	  *   1. To truly limit the visibility of a declaration, including to overriding by subclasses, in order to prevent
	  *      leaking of private API or unintentional overrides (especially for abstract methods).
	  *      While the simplest way to do this is simply by making the declaration final in addition to protected,
	  *      it is not always possible or feasible, especially for large class hierarchies. If any of the types
	  *      included in the signature of the declaration (arguments, return type, including their type parameters)
	  *      has restricted visibility, the goal is attained automatically. Otherwise an implicit argument `Seal`
	  *      added to a method (optionally with a default value `Seal()` in case other implicit parameters are already
	  *      present and might be applied explicitly) achieved the same way:
	  *      {{{
	  *          package seal {
	  *             private[seal] val seals = new Sealing
	  *             trait Sealed {
	  *                 protected[seals] def asIfSealed(implicit seal :seals.Seal) = ???
	  *             }
	  *          }
	  *      }}}
	  *
	  * While this method is usually most convenient for methods, it cannot be easily adapted to `val` and `var`
	  * declarations. For this purpose a [[slang.Sealing.Sealed Sealed]]`[T]` wrapper value class
	  * is introduced alongside it, which works according to the same principles. Note that this should not be dependent
	  * upon in the context of security, as all such declarations are public in the bytecode and can thus
	  * be easily accessed from `Java`.
	  */
	@SerialVersionUID(Ver)
	final class Seal extends Serializable

	@SerialVersionUID(Ver)
	object Seal {
		@inline def apply() :Seal = instance
		implicit final val instance :Seal = new Seal
	}

	/** A value wrapper with visibility restricted to the same as the enclosing
	  *  [[Sealing Sealing]] instance, essentially limiting the visibility of declarations
	  *  of its type to the same scope. While redundant from the point o view of usages of such definitions
	  *  (which must contain a matching `protected`/`private` modifier anyway), it additionally prevents
	  *  overriding of such non-final declarations from any classes outside of the scope, which would be possible
	  *  in case of a simple `private[scope] val meaning = 42`. Instead, a definition of
	  *  {{{
	  *      private[scope] val seals = new Sealing
	  *      private[scope] val meaning = Sealed(42)
	  *  }}}
	  *  achieves the same result that a modifier `sealed[scope]` would have.
	  *
	  *  This class is the only feasible solution for values and variables of otherwise public types,
	  *  but for methods a sibling class [[slang.Sealing.Seal Seal]] will typically be more convenient.
	  */
	type Sealed[+T] <: NotSealed[T]

	@SerialVersionUID(Ver)
	object Sealed {
		@inline def apply[T](value :T) :Sealed[T] = new NotSealed[T](value).asInstanceOf[Sealed[T]]

		@inline implicit def unseal[T](seal :Sealed[T]) :T = seal.value
		@inline implicit def seal[T](value :T) :Sealed[T] = new NotSealed(value).asInstanceOf[Sealed[T]]
	}
}



@SerialVersionUID(Ver)
object Sealing {
	@SerialVersionUID(Ver)
	class NotSealed[+T](val value :T) extends AnyVal with Serializable
}
