package net.noresttherein.sugar.slang

import scala.reflect.ClassTag

import net.noresttherein.sugar.exceptions.raise
import net.noresttherein.sugar.slang.extensions.{AnyRefExtension, ensuringMethodsConversion}
import net.noresttherein.sugar.typist.PriorityConversion
import net.noresttherein.sugar.typist.casting.extensions.castTypeParamMethods





trait extensions extends Any {
	/** Adds `identityHashCode` method to all reference types. */
	@inline implicit final def AnyRefExtension(self :AnyRef) :AnyRefExtension = new AnyRefExtension(self)

	/** Adds additional `ensuring` methods to any object which accept exception classes to throw on failure. */
	@inline implicit final def ensuringMethods[T] :ensuringMethodsConversion[T] =
		extensions.ensuringMethodsConversionPrototype.castParam[T]

//	@inline implicit final def ensuringMethods[T](self :T) :ensuringMethods[T] = new ensuringMethods[T](self)

}



@SerialVersionUID(Ver)
object extensions extends extensions {
	class AnyRefExtension(private val self :AnyRef) extends AnyVal {
		/** The value of `hashCode` as it would be inherited for this object from the default implementation in `AnyRef`. */
		@inline def identityHashCode :Int = System.identityHashCode(self)
	}



	//todo: new method name and a macro to include the actual expression used as a string.
	/** Extension methods overloading `scala.ensuring`, allowing for passing an arbitrary exception to be thrown. */
	class ensuringMethods[T](private val self :T) extends AnyVal {

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
		@inline final def apply(v1 :T)(implicit dummy :DummyImplicit) :ensuringMethods[T] = new ensuringMethods(v1)
	}
	private def newEnsuringMethodsConversion[T] =
		new PriorityConversion.Wrapped[T, ensuringMethods[T]](new ensuringMethods(_))
			with ensuringMethodsConversion[T]
	private val ensuringMethodsConversionPrototype :ensuringMethodsConversion[Any] = newEnsuringMethodsConversion

}
