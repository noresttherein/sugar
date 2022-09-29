package net.noresttherein.sugar.vars

import java.lang.invoke.{MethodHandles, VarHandle}

import scala.Specializable.Args

import net.noresttherein.sugar.vars.InOut.{SpecializedVars, TypeEquiv}
import net.noresttherein.sugar.witness.DefaultValue




/** Implementation trait for `InOut` implementations with `@volatile` atomic update operations. */
private[vars] trait VolatileLike[@specialized(SpecializedVars) T] extends InOut[T] {
	//todo: change the semantics of testAndSet and the rest from eq to equals
	protected def factory :VolatileLikeOps[VolatileLike]

	override def isDefined :Boolean = true

	/** Assigns a new value to this variable, returning a value it held at some point in the past.
	  * This method is atomic with `@volatile` access semantics.
	  */
	override def ?=(newValue :T) :T = factory.getAndSet(this, newValue)

	/** Assigns a new value to this variable providing the current value is equal to the expected value.
	  * This method is atomic with `@volatile` access semantics.
	  * @param expect   a value to compare with current value.
	  * @param newValue a new value for this variable.
	  * @return `true` if the previous value equaled `expect` and the variable has been set to `newValue`.
	  */
	override def testAndSet(expect :T, newValue :T) :Boolean =
		factory.testAndSet(this, expect, newValue)

	/** Atomically updates the value of this variable with the given function. The result is equivalent to
	  * `val res = f(this.get); this.value = res; res`, but guarantees atomicity and happens with `@volatile`
	  * memory access semantics.
	  * @param f function to apply to the value of this variable. Should have no side effects as it may be invoked
	  *          several times.
	  * @return the result of applying `f` to the current value.
	  */
	override def apply(f :T => T) :T = {
		val companion = factory
		var curr = value
		var newValue = f(curr)
		while (!companion.weakTestAndSet(this, curr, newValue)) {
			val v = value
			if (v != curr) {
				curr = v
				newValue = f(curr)
			}
		}
		newValue
	}

	/** Atomically combines the value of this variable with a value of some other type, assigning the result
	  * of application back to this variable before returning it. It uses this variable as an accumulator,
	  * updated iteratively with new values in a way similar to an in place ''foldLeft'' operation
	  * on a singleton collection; the difference from `foldLeft` is that the function's result is the type
	  * of this variable, rather than the argument.
	  *
	  * The value of this variable is read and set with `@volatile` memory access semantics.
	  * @param z accumulator value to pass as the first argument to the `f` function, together with the current
	  *          value of this variable.
	  * @param f a function applied to the argument and this variable, whose result should be set to this variable.
	  * @return the result of applying `f` to this variable and the argument.
	  */
	override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
		val companion = factory
		var curr = value
		var newValue = f(z, curr)
		while (!companion.weakTestAndSet(this, curr, newValue)) {
			val v = value
			if (v != curr) {
				curr = v
				newValue = f(z, curr)
			}
		}
		newValue
	}

	/** Atomically combines the value of this variable with a value of some other type, assigning the result
	  * of application back to this variable before returning it. It uses this variable as an accumulator,
	  * updated iteratively with new values in a way similar to an in place ''foldRight'' operation
	  * on a singleton collection; the difference from `foldRight` is that the function's result is the type
	  * of this variable, rather than the argument.
	  *
	  * The value of this variable is read and set with `@volatile` memory access semantics.
	  * @param z accumulator value to pass as the second argument to the `f` function, together with the current
	  *          value of this variable.
	  * @param f a function applied to the this variable and the argument, whose result should be set to this variable.
	  * @return the result of applying `f` to this variable and the argument.
	  */
	override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
		val companion = factory
		var curr = value
		var newValue = f(curr, z)
		while (!companion.weakTestAndSet(this, curr, newValue)) {
			val v = value
			if (v != curr) {
				curr = v
				newValue = f(curr, z)
			}
		}
		newValue
	}


	private[vars] override def bool_&=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit =
		if (!other) ev(this).value = false

	private[vars] override def bool_|=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit =
		if (other) ev(this).value = true

	//overriden to avoid the creation of a closure object capturing other
	private[vars] override def bool_&&=(other : => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit =
		if (ev(this).value && !other) //race condition doesn't change the result, as if it set this.value = false then also !(value && bool)
			ev(this).value = false

	private[vars] override def bool_||=(other : => Boolean)(implicit ev :T TypeEquiv Boolean) :Unit =
		if (!ev(this).value && other)
			ev(this).value = true

	private[vars] override def bool_^=(other :Boolean)(implicit ev :T TypeEquiv Boolean) :Unit = {
		val expect = ev(this).value
		factory.repeatBoolTestAndSet(ev(this), expect, expect ^ other, !expect ^ other)
	}

	private[vars] override def bool_!=(implicit ev :T TypeEquiv Boolean) :Boolean = {
		val expect = ev(this).value
		factory.repeatBoolTestAndSet(ev(this), expect, !expect, expect)
	}


	private[vars] override def int_+=(other :Int)(implicit ev :T TypeEquiv Int) :Int = 
		factory.intHandle(ev(this)).getAndAdd(this :AnyRef, other).asInstanceOf[Int] + other
	
	private[vars] override def int_&=(other :Int)(implicit ev :T TypeEquiv Int) :Int = 
		factory.intHandle(ev(this)).getAndBitwiseAnd(this :AnyRef, other).asInstanceOf[Int] & other
	
	private[vars] override def int_|=(other :Int)(implicit ev :T TypeEquiv Int) :Int =
		factory.intHandle(ev(this)).getAndBitwiseOr(this :AnyRef, other).asInstanceOf[Int] | other
	
	private[vars] override def int_^=(other :Int)(implicit ev :T TypeEquiv Int) :Int =
		factory.intHandle(ev(this)).getAndBitwiseXor(this :AnyRef, other).asInstanceOf[Int] ^ other


	private[vars] override def long_+=(other :Long)(implicit ev :T TypeEquiv Long) :Long =
		factory.longHandle(ev(this)).getAndAdd(this :AnyRef, other).asInstanceOf[Long] + other

	private[vars] override def long_&=(other :Long)(implicit ev :T TypeEquiv Long) :Long =
		factory.longHandle(ev(this)).getAndBitwiseAnd(this :AnyRef, other).asInstanceOf[Long] & other

	private[vars] override def long_|=(other :Long)(implicit ev :T TypeEquiv Long) :Long =
		factory.longHandle(ev(this)).getAndBitwiseOr(this :AnyRef, other).asInstanceOf[Long] | other

	private[vars] override def long_^=(other :Long)(implicit ev :T TypeEquiv Long) :Long =
		factory.longHandle(ev(this)).getAndBitwiseXor(this :AnyRef, other).asInstanceOf[Long] ^ other


//	private[vars] override def isSpecialized = true
}




@SerialVersionUID(ver)
private[vars] object VolatileLike {

	/** An unspecialized `VolatileLike` implementation overriding atomic mutator methods to compare the value
	  * using `eq`/`ne`, rather than `==`/`!=` as in `VolatileLike` (which would call `equals` on reference types,
	  * which we do not want).
	  */
	trait RefVolatileLike[T] extends VolatileLike[T] {
		override def apply(f :T => T) :T = {
			val companion = factory
			var curr = value
			var newValue = f(curr)
			while (!companion.weakTestAndSet(this, curr, newValue)) {
				val v = value
				if (v.asInstanceOf[AnyRef] ne curr.asInstanceOf[AnyRef]) {
					curr = v
					newValue = f(curr)
				}
			}
			newValue
		}
		override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
			val companion = factory
			var curr = value
			var newValue = f(z, curr)
			while (!companion.weakTestAndSet(this, curr, newValue)) {
				val v = value
				if (v.asInstanceOf[AnyRef] ne curr.asInstanceOf[AnyRef]) {
					curr = v
					newValue = f(z, curr)
				}
			}
			newValue
		}
		override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
			val companion = factory
			var curr = value
			var newValue = f(curr, z)
			while (!companion.weakTestAndSet(this, curr, newValue)) {
				val v = value
				if (v.asInstanceOf[AnyRef] ne curr.asInstanceOf[AnyRef]) {
					curr = v
					newValue = f(curr, z)
				}
			}
			newValue
		}

		private[vars] override def isSpecialized = false
	}

	/** Optimised implementation of `VolatileLike[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	trait BoolVolatileLike extends VolatileLike[Boolean] {
		override def apply(f :Boolean => Boolean) :Boolean = {
			val companion = factory
			val expect = value
			val ifExpected = f(expect)
			if (companion.boolHandle(this).weakCompareAndSet(this, expect, ifExpected))
				ifExpected
			else
				companion.repeatBoolTestAndSet(this, !expect, f(!expect), ifExpected)
		}
		override def applyRight[@specialized(Args) A](z :A)(f :(Boolean, A) => Boolean) :Boolean = {
			val companion = factory
			val expect = value
			val ifExpected = f(expect, z)
			if (companion.boolHandle(this).weakCompareAndSet(this, expect, ifExpected))
				ifExpected
			else
				companion.repeatBoolTestAndSet(this, !expect, f(!expect, z), ifExpected)
		}
		override def applyLeft[@specialized(Args) A](z :A)(f :(A, Boolean) => Boolean) :Boolean = {
			val companion = factory
			val expect = value
			val ifExpected = f(z, expect)
			if (companion.boolHandle(this).weakCompareAndSet(this, expect, ifExpected))
				ifExpected
			else
				companion.repeatBoolTestAndSet(this, !expect, f(z, !expect), ifExpected)
		}

		private[vars] override def isSpecialized :Boolean = true
	}

}






/** @define variable variable */
private[vars] abstract class VolatileLikeOps[+V[T] <: InOut[T]] {
	private[vars] def getAndSet[@specialized(SpecializedVars) T](v :InOut[T], newValue :T) :T
	private[vars] def testAndSet[@specialized(SpecializedVars) T](v :InOut[T], expect :T, newValue :T) :Boolean
	private[vars] def weakTestAndSet[@specialized(SpecializedVars) T](v :InOut[T], expect :T, newValue :T) :Boolean

	private[vars] def repeatBoolTestAndSet
	                  (bool :InOut[Boolean], expect :Boolean, ifExpected :Boolean, ifNotExpected :Boolean) :Boolean

	private[vars] def intHandle(variable :InOut[Int]) :VarHandle
	private[vars] def longHandle(variable :InOut[Long]) :VarHandle
	private[vars] def boolHandle(variable :InOut[Boolean]) :VarHandle
}



private[vars] abstract class VolatileLikeCompanion[+V[T] <: InOut[T]] extends VolatileLikeOps[V] {

	protected def newInstance[@specialized(SpecializedVars) T](init :T) : V[T]
	protected def newRefInstance[T](init :T) :V[T]
	protected def newBoolInstance(init :Boolean) :V[Boolean]


	private[vars] override def getAndSet[@specialized(SpecializedVars) T](v :InOut[T], newValue :T) :T =
		(v.getClass match {
			case CaseAny                 => anyHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Any])
			case CaseInt                 => intHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Int])
			case CaseLong                => longHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Long])
			case CaseBool | CaseBoolSpec => boolHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Boolean])
			case CaseDouble              => doubleHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Double])
			case CaseFloat               => floatHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Float])
			case CaseByte                => byteHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Byte])
			case CaseChar                => charHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Char])
			case CaseShort               => shortHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Short])
			case _                       => anyHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Any]) //CaseUnspec
		}).asInstanceOf[T]

	private[vars] override def testAndSet[@specialized(SpecializedVars) T](v :InOut[T], expect :T, newValue :T) :Boolean =
		v.getClass match {
			case CaseAny      =>
				anyHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any])
			case CaseInt      =>
				intHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Int], newValue.asInstanceOf[Int])
			case CaseLong     =>
				longHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Long], newValue.asInstanceOf[Long])
			case CaseBool | CaseBoolSpec =>
				boolHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Boolean], newValue.asInstanceOf[Boolean])
			case CaseDouble   =>
				doubleHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Double], newValue.asInstanceOf[Double])
			case CaseFloat    =>
				floatHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Float], newValue.asInstanceOf[Float])
			case CaseByte     =>
				byteHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Byte], newValue.asInstanceOf[Byte])
			case CaseChar     =>
				charHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Char], newValue.asInstanceOf[Char])
			case CaseShort    =>
				shortHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Short], newValue.asInstanceOf[Short])
			case _            =>
				anyHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any]) //CaseUnspec
		}

	private[vars] override def weakTestAndSet[@specialized(SpecializedVars) T](v :InOut[T], expect :T, newValue :T) :Boolean =
		v.getClass match {
			case CaseAny                 =>
				anyHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any])
			case CaseInt                 =>
				intHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Int], newValue.asInstanceOf[Int])
			case CaseLong                =>
				longHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Long], newValue.asInstanceOf[Long])
			case CaseBool | CaseBoolSpec =>
				boolHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Boolean], newValue.asInstanceOf[Boolean])
			case CaseDouble              =>
				doubleHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Double], newValue.asInstanceOf[Double])
			case CaseFloat               =>
				floatHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Float], newValue.asInstanceOf[Float])
			case CaseByte                =>
				byteHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Byte], newValue.asInstanceOf[Byte])
			case CaseChar                =>
				charHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Char], newValue.asInstanceOf[Char])
			case CaseShort               =>
				shortHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Short], newValue.asInstanceOf[Short])
			case _                       =>
				anyHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any]) //CaseUnspec
		}

	private[vars] override def repeatBoolTestAndSet(bool :InOut[Boolean], expect :Boolean,
	                                                ifExpected :Boolean, ifNotExpected :Boolean) :Boolean =
	{
		val handle = boolHandle(bool)
		var v = expect
		while (!handle.weakCompareAndSet(bool, if (v == expect) ifExpected else ifNotExpected))
			v = !v
		if (v == expect) ifExpected else ifNotExpected
	}


	private[vars] override def intHandle(variable :InOut[Int]) = variable.getClass match {
		case CaseInt => intHandle
		case _ => anyHandle
	}
	private[vars] override def longHandle(variable :InOut[Long]) = variable.getClass match {
		case CaseLong => longHandle
		case _ => anyHandle
	}
	private[vars] override def boolHandle(variable :InOut[Boolean]) = variable.getClass match {
		case CaseBool | CaseBoolSpec => boolHandle
		case _ => anyHandle
	}

	private[vars] val CaseByte     = newInstance(0.toByte).getClass
	private[vars] val CaseShort    = newInstance(0.toShort).getClass
	private[vars] val CaseChar     = newInstance(0.toChar).getClass
	private[vars] val CaseInt      = newInstance(0).getClass
	private[vars] val CaseLong     = newInstance(0L).getClass
	private[vars] val CaseDouble   = newInstance(0.0).getClass
	private[vars] val CaseFloat    = newInstance(0.0f).getClass
	private[vars] val CaseBool     = newInstance(false).getClass
	private[vars] val CaseBoolSpec = newBoolInstance(false).getClass
	private[vars] val CaseAny      = newRefInstance(new AnyRef).getClass
	private[vars] val CaseUnspec   = newInstance(new AnyRef).getClass

	private[vars] val byteHandle   = newHandle(CaseByte, java.lang.Byte.TYPE)
	private[vars] val shortHandle  = newHandle(CaseShort, java.lang.Short.TYPE)
	private[vars] val charHandle   = newHandle(CaseChar, java.lang.Character.TYPE)
	private[vars] val intHandle    = newHandle(CaseInt, java.lang.Integer.TYPE)
	private[vars] val longHandle   = newHandle(CaseLong, java.lang.Long.TYPE)
	private[vars] val floatHandle  = newHandle(CaseFloat, java.lang.Float.TYPE)
	private[vars] val doubleHandle = newHandle(CaseDouble, java.lang.Double.TYPE)
	private[vars] val boolHandle   = newHandle(CaseBool, java.lang.Boolean.TYPE)
	private[vars] val anyHandle    = newHandle(classOf[Volatile[Any]], classOf[Any])

	private def newHandle[T](varType :Class[_], paramType :Class[_]) =
		MethodHandles.lookup().findVarHandle(varType, fieldName(varType), paramType)

	private def fieldName(cls :Class[_]) = {
		val specSuffixStart = cls.getName.indexOf('$')
		if (specSuffixStart < 0) "x"
		else "x" + cls.getName.substring(specSuffixStart)
	}

}




private[vars] trait VolatileLikeFactory[+V[X] <: VolatileLike[X]] extends VolatileLikeCompanion[V] {
	/** Create a new $variable which can be shared by multiple threads. */
	def apply[@specialized(SpecializedVars) T](init :T) :V[T] = newInstance(init) match {
		case any if any.getClass == CaseUnspec => newRefInstance(init)
		case bool if bool.getClass == CaseBool => newBoolInstance(init.asInstanceOf[Boolean]).asInstanceOf[V[T]]
		case res => res
	}

	/** Create a new $variable which can be shared by multiple threads. */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :V[T] = apply(default.get)
}
