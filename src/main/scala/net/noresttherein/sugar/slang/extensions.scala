package net.noresttherein.sugar.slang

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.reflect.ClassTag

import net.noresttherein.sugar.exceptions.raise
import net.noresttherein.sugar.slang.extensions.{hashCodeMethods, applyToMethod, ensuringMethodsConversion, notNullMethod}
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.typist.casting.extensions.castTypeParamMethods
import net.noresttherein.sugar.witness.Ignored





trait extensions extends Any {
	/** Adds `identityHashCode` method to all reference types. */
	@inline implicit final def hashCodeMethods(self :AnyRef) :hashCodeMethods = new hashCodeMethods(self)

	/** Adds [[net.noresttherein.sugar.slang.extensions.applyToMethod.applyTo applyTo]] method to any value,
	  * which applies an argument function to it, inverting the writing direction. The method is also available as `=*>`.
	  */
	@inline implicit final def applyToMethod[X](x :X) :applyToMethod[X] = new applyToMethod(x)

	/** Adds additional `ensuring` methods to any object which accept exception classes to throw on failure. */
	@inline implicit final def ensuringMethods[T] :ensuringMethodsConversion[T] =
		extensions.ensuringMethodsConversionPrototype.castParam[T]

	/** Adds a [[net.noresttherein.sugar.slang.extensions.notNullMethod.notNull notNull]]`(msg: String)` method to any value. */
	@inline implicit final def notNullMethod[X](x :X) :notNullMethod[X] = new notNullMethod(x)

}



@SerialVersionUID(Ver)
object extensions extends extensions {

	class hashCodeMethods private[slang] (private val self :AnyRef) extends AnyVal {
		/** The value of `hashCode` as it would be inherited for this object from the default implementation in `AnyRef`. */
		@inline def identityHashCode :Int = System.identityHashCode(self)

		@inline def shortIdentityHashCode :Int = System.identityHashCode(self)
	}


	/** Adds a `applyTo` method to any value which applies a given function to `this`. */
	class applyToMethod[X] private[slang](private val x :X) extends AnyVal {
		/** Applies the argument function to the 'self' argument. As self is eagerly computed, `expr feedTo f`
		  * is equivalent to `{ val x = expr; f(x) }`, but may be more succinct and convenient to write, especially
		  * when applying an argument to a composed function expression:
		  * {{{
		  *     x applyTo (f andThen g andThen h)
		  * }}}
		  */
		def applyTo[T](f :X => T) :T = f(x)

		/** Applies the argument function to the 'self' argument. As self is eagerly computed, `expr feedTo f`
		  * is equivalent to `{ val x = expr; f(x) }`, but may be more succinct and convenient to write, especially
		  * when applying an argument to a composed function expression:
		  * {{{
		  *     x \=> (f andThen g andThen h)
		  * }}}
		  */
		def \=>[T](f :X => T) :T = f(x)
	}


	//todo: new method name and a macro to include the actual expression used as a string.
	/** Extension methods overloading `scala.ensuring`, allowing for passing an arbitrary exception to be thrown. */
	class ensuringMethods[T] private[slang] (private val self :T) extends AnyVal {

		/** Return `this` if `condition` is true, or throw the give exception otherwise. */
		@inline def ensuring(condition :Boolean, ex :Exception) :T =
			if (condition) self else throw ex


		/** Return `this` if `condition` is false, or throw the given exception otherwise. */
		@inline def ensuring(condition :T => Boolean, ex :Exception) :T =
			if (condition(self)) self else throw ex


		/** Return `this` if `condition` is true, or raise the exception given as type parameter (must provide
		  * a default constructor). */
		@inline def ensuring[E <: Exception :ClassTag](condition :Boolean) :T =
			if (condition) self else raise[E]

		/** Return `this` if `condition` is true, or raise the exception specified as the type parameter,
		  * passing the given string to its constructor. */
		@inline def ensuring[E <: Exception :ClassTag](condition :Boolean, msg: => String) :T =
			if (condition) self else raise[E](msg)

		//methods from Predef.Ensuring, because we are shadowing them
	    def ensuring(cond: Boolean) :T = { assert(cond); self }
	    def ensuring(cond: Boolean, msg: => Any) :T = { assert(cond, msg); self }
	    def ensuring(cond: T => Boolean) :T = { assert(cond(self)); self }
	    def ensuring(cond: T => Boolean, msg: => Any) :T = { assert(cond(self), msg); self }
	}

	sealed trait ensuringMethodsConversion[T] extends (T => ensuringMethods[T]) {
		/* See collections.extensions.ArrayExtensionConversion. */
		@inline final def apply(v1 :T)(implicit __ :Ignored) :ensuringMethods[T] = new ensuringMethods(v1)
	}
	private def newEnsuringMethodsConversion[T] =
		new PriorityConversion.Wrapped[T, ensuringMethods[T]](new ensuringMethods(_))
			with ensuringMethodsConversion[T]
	private val ensuringMethodsConversionPrototype :ensuringMethodsConversion[Any] = newEnsuringMethodsConversion



	/** An extension method for any object throwing an [[AssertionError]] if it is `null`. */
	class notNullMethod[X] private[sugar] (private val x :X) extends AnyVal {
		/** An extension method for any object throwing an [[AssertionError]] if it is `null`. */
		@elidable(ASSERTION) @inline def notNull(msg: => String) :X = {
			if (x == null)
				throw new AssertionError(msg)
			x
		}
		/** An extension method for any object throwing an [[AssertionError]] if it is `null`. */
		@elidable(ASSERTION) @inline def notNull :X = {
			if (x == null)
				throw new AssertionError
			x
		}
	}

//	object conditionalExpression {
//		class IfFalse[+T] private[sugar] (private val ifFalse: () => T) {
//			@inline def /:[U >: T](ifTrue: => U) :ConditionalExpressionAlternatives[U] =
//				new ConditionalExpressionAlternatives[U](ifTrue, ifFalse)
//		}
//		class ConditionalExpressionAlternatives[+T] private[sugar] (ifTrue: => T, ifFalse: () => T) {
//			@inline def ?:(condition :Boolean) :T = if (condition) ifTrue else ifFalse()
//		}
//	}

}
