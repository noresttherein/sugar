package net.noresttherein.slang.typist


import scala.reflect.runtime.universe.{typeTag, weakTypeOf, weakTypeTag, Type, TypeTag, WeakTypeTag}
import net.noresttherein.slang.typist.TypeDef.{InType, OutType}

import scala.reflect.runtime.universe


/** A simple ephemeral wrapper over a type declaration `T = X`. Together with its super types
  * [[net.noresttherein.slang.typist.TypeDef.InType]] and [[net.noresttherein.slang.typist.TypeDef.OutType]]
  * allows simulation of covariant and contravariant abstract type member declarations. It is roughly equivalent
  * to having the enclosing type accepting a type parameter `X`. Its introduction is motivated by the lack
  * of non-abstract covariant and contravariant type definitions in scala: while similar effect could be had by having
  * the 'interface' traits use type bounds `type T &lt;: X` and `type T &gt;: X`, in such a solution there is a need
  * for separate enclosing class for each desired concrete member type definition. Keeping the type bound `T &lt;: U`
  * in a container makes it instead possible to simply declare `val T :OutType[U] = TypeDef[U]` in the base class
  * and `override val T :OutType[T] = OutType[T]` for `T &lt;: U` as one would with any `val`.
  * There are several reasons why one may choose this unconventional approach over type parameters:
  *   - Reducing clutter of the type signature by removing non-crucial abstract types, especially those being implementation
  *     artifacts;
  *   - Reduces the burden of type constraints required from users of a declaring class: any code unit using a generic type
  *     with bound type parameters needs to explicitly declare those type bounds itself, too. This introduces clutter,
  *     slows development and often poses unnecessary resolvable conflicts from the point of view of client code interested
  *     only in selected fragments of the used interface;
  *   - Helping implicit resolution algorithm to find the correct order of type instantiation: types not present in
  *     the signature are naturally ignored, making the algorithm pick the 'input' types from the point of view of implicits
  *     designed to instantiate an 'output' type with desired properties;
  *
  * @author Marcin Mościcki marcin@moscicki.net
  */
class TypeDef[X](val weakTypeTag :WeakTypeTag[X]) extends AnyVal with InType[X] with OutType[X] {
	type T = X

	override def weakType :Type = weakTypeTag.tpe.dealias

	def localName(implicit tpe :TypeTag[X]) :String = tpe.tpe.dealias.toString

	override def toString :String = "TypeDef[" + weakType + "]"
}


/** Factory for [[TypeDef!]] member type wrappers.
  * @see [[TypeDef!]]
  * @see [[TypeDef.apply]]
  * @see [[TypeDef.InType]]
  * @see [[TypeDef.OutType]]
  */
object TypeDef {

	/** An ephemeral wrapper over a type declaration `T = X` simulating covariant and contravariant type declarations. */
	@inline def apply[X :WeakTypeTag] :TypeDef[X] = new TypeDef[X](weakTypeTag[X])

	/** A covariant wrapper of type `T &lt;: X` to be used as a member of another class. */
	@inline def InType[X] :InType[X] = apply[X]

	/** A contravariant wrapper of type `T &gt;: X` to be used as a member of another class. */
	@inline def OutType[X] :OutType[X] = apply[X]

	/** A [[TypeDef]] variant additionally capturing and wrapping the type tag of defined type for later use. */
	@inline def ConcreteDef[X :TypeTag] :ConcreteDef[X] = new ConcreteDef[X](typeTag[X])

	/** A simple ephemeral wrapper over type declaration `T &gt;: X`. It is roughly equivalent to having the enclosing
	  * class accept a type parameter `-X`. See [[TypeDef!]] for motivation.
	  */
	sealed trait InType[-X] extends Any {
		type T >: X

		/** String name of (a) lower bound for type `T`. Note that as the type information is given at the point of
		  * this method's call rather than instance creation, the result may be more generic than the actual originally
		  * given type argument `X`.
		  */
		def lb[L <: X](implicit tpe :TypeTag[L]) :String = tpe.tpe.toString

		/** Reflected type of `X` captured at the declaration point, which may be an abstract type. */
		def weakType :Type

	}


	/** A simple ephemeral wrapper over type declaration `T &lt;: X`. It is roughly equivalent to having the enclosing
	  * class accept a type parameter `+X`. See [[TypeDef!]] for motivation.
	  */
	sealed trait OutType[+X] extends Any {
		type T <: X

		/** String name of (an) upper bound for type `T`. Note that as the type information is given at the point of
		  * this method's call rather than instance creation, the result may be more generic than the actual originally
		  * given type argument `X`.
		  */
		def ub[U >: X](implicit tpe :TypeTag[U]) :String = tpe.tpe.dealias.toString

		/** Reflected type of `X` captured at the declaration point, which may be an abstract type. */
		def weakType :Type

	}


	/** A `TypeDef[X]` variant capturing the whole concrete type `X`.
	  * @see [[net.noresttherein.slang.typist.TypeDef]]
	  */
	class ConcreteDef[X] private[TypeDef](val typeTag :TypeTag[X]) extends AnyVal with InType[X] with OutType[X] {
		override type T = X

		/** Reflected concrete type `X` captured at the declaration point. */
		override def weakType :Type = typeTag.tpe.dealias

		override def toString :String = "ConcreteDef[" + weakType + "]"
	}
}
