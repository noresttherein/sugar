package net.noresttherein.sugar.vars

import java.lang.invoke.{MethodHandles, VarHandle}

import scala.Specializable.Args
import scala.annotation.nowarn
import scala.reflect.ClassTag

import net.noresttherein.sugar.??!
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.reflect.{classes, scalaFieldName}
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.witness.DefaultValue




@SerialVersionUID(Ver)
object AtomicOps {

	/** Implementation trait for `InOut` implementations with atomic update operations.
	  * Delegates these methods to its companion [[net.noresttherein.sugar.vars.AtomicOps! AtomicOps]] instance.
	  * @see [[net.noresttherein.sugar.vars.Atomic Atomic]]
	  * @see [[net.noresttherein.sugar.vars.Volatile Volatile]]
	  */ //this is an inner class so it can have access to protected methods in AtomicOps.
	//todo: make testAndSet semantics use equals, not eq by default.
	trait AtomicVar[@specialized(SpecializedVars) T] extends InOut[T] { //or VarOps?
		protected def factory :AtomicOps[AtomicVar]

		override def isDefined :Boolean = true

		/** Atomically assigns a new value to this variable, returning a value it held at some point in the past.
		  * @return the value of this $Ref at the moment of assigning `newValue`.
		  */
		override def ?=(newValue :T) :T = factory.getAndSet(this, newValue)

		/** Atomically assigns a new value to this variable, providing the current value is equal to the expected value.
		  * @param expect   a value to compare with current value.
		  * @param newValue a new value for this variable.
		  * @return `true` if the previous value equaled `expect` and the variable has been set to `newValue`.
		  */
		override def testAndSet(expect :T, newValue :T) :Boolean =
			factory.testAndSet(this, expect, newValue)

		/** Atomically updates the value of this variable with the given function. The result is equivalent to
		  * `val res = f(this.get); this.value = res; res`, but guarantees atomicity.
		  * @param f function to apply to the value of this variable. Should have no side effects as it may be invoked
		  *          several times.
		  * @return the result of applying `f` to the current value.
		  */
//		override def update(f :T => T) :T = factory.update(this, f)
		override def update(f :T => T) :T = {
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
		  * @param z accumulator value to pass as the first argument to the `f` function, together with the current
		  *          value of this variable.
		  * @param f a function applied to the argument and this variable, whose result should be set to this variable.
		  * @return the result of applying `f` to this variable and the argument.
		  */
//		override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = factory.updateLeft(this, z, f)
		override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
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
		  * @param z accumulator value to pass as the second argument to the `f` function, together with the current
		  *          value of this variable.
		  * @param f a function applied to the this variable and the argument, whose result should be set to this variable.
		  * @return the result of applying `f` to this variable and the argument.
		  */
//		override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = factory.updateRight(this, z, f)
		override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
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

		override def mkString :String = mkString(factory.toString)
		//	private[vars] override def isSpecialized = true
	}


	/** An unspecialized `AtomicVar` implementation overriding atomic mutator methods to compare the value
	  * using `eq`/`ne`, rather than `==`/`!=` as in `AtomicVar` (which would call `equals` on reference types,
	  * which we do not want).
	  */
	trait RefAtomicVar[T] extends AtomicOps.AtomicVar[T] {
//		override def update(f :T => T) :T = factory.updateRef(this, f)
//		override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = factory.updateLeftRef(this, z, f)
//		override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = factory.updateRightRef(this, z, f)

		override def testAndSet(expect :T, newValue :T) :Boolean = factory.testAndSetRef(this, expect, newValue)

		override def update(f :T => T) :T = {
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
		override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T = {
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
		override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T = {
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


	/** Optimised implementation of `AtomicVar[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	trait BoolAtomicVar extends AtomicOps.AtomicVar[Boolean] {
		override def update(f :Boolean => Boolean) :Boolean = {
			val companion = factory
			val expect = value
			val ifExpected = f(expect)
			if (companion.weakTestAndSetBool(this, expect, ifExpected))
				ifExpected
			else
				companion.repeatTestAndSetBool(this, !expect, f(!expect), ifExpected)
		}
		override def updateRight[@specialized(Args) A](z :A)(f :(Boolean, A) => Boolean) :Boolean = {
			val companion = factory
			val expect = value
			val ifExpected = f(expect, z)
			if (companion.weakTestAndSetBool(this, expect, ifExpected))
				ifExpected
			else
				companion.repeatTestAndSetBool(this, !expect, f(!expect, z), ifExpected)
		}
		override def updateLeft[@specialized(Args) A](z :A)(f :(A, Boolean) => Boolean) :Boolean = {
			val companion = factory
			val expect = value
			val ifExpected = f(z, expect)
			if (companion.weakTestAndSetBool(this, expect, ifExpected))
				ifExpected
			else
				companion.repeatTestAndSetBool(this, !expect, f(z, !expect), ifExpected)
		}

		private[vars] override def isSpecialized :Boolean = true
	}

}






/** Static implementations of atomic operations on $Ref variables, typically extended by its companion object.
  * [[net.noresttherein.sugar.vars.AtomicOps.AtomicVar AtomicVar]] delegates to this class for lower level operations.
  * The manner of synchronization is not specified, but all compound
  * ([[net.noresttherein.sugar.vars.AtomicOps.getAndSet getAndSet]],
  * [[net.noresttherein.sugar.vars.InOut.testAndSet testAndSet]], etc.) are guaranteed to be performed atomically.
  * @define Ref `V`
  * @define ref concurrent variable
  */ //Sadly covariant because it is returned from AtomicVar and SignalVar extends SyncVar.
abstract class AtomicOps[+V[T] <: InOut[T]] { //abstract class, not trait, because the JVM is more likely to inline
	protected def getAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], newValue :T) :T

	/** Atomically set the variable `v` to `newValue` if the current value `equals expect` (value equality).  */
	protected def testAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean

	protected def weakTestAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean

	/** ''Test-and-set'' with referential equality semantics, that is comparing object pointers with `eq`. */
	protected def testAndSetRef[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean

	/** ''Test-and-set'' with referential equality (`eq`) semantics, which may randomly fail. */
	protected def weakTestAndSetRef[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean

	/** Implements ''test-and-set'' with value semantics on a variable with a reference field (typically, erased).
	  * Delegated to by `testAndSet` when `v` is a `RefAtomicVar`.
	  */
	protected def genericTestAndSet[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		testAndSetRef(v, expect, newValue) || {
			val x = v.value
			x == expect && testAndSetRef(v, x, newValue)
		}

	/** Implements weak ''test-and-set'' with value semantics on a variable with a reference field (typically, erased).
	  * Delegated to by `weakTestAndSet` when `v` is a `RefAtomicVar`.
	  */
	protected def genericWeakTestAndSet[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		weakTestAndSetRef(v, expect, newValue) || {
			val x = v.value
			x == expect && weakTestAndSetRef(v, x, newValue)
		}

	protected def weakTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean, newValue :Boolean) :Boolean

	protected def repeatTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean,
	                                   ifExpected :Boolean, ifNotExpected :Boolean) :Boolean
}



//todo: make the default companion use equality for ref types,
// make a special Ref variant for current classes, which use referential equality instead (even for value types),
// and this Frankenstein mix using 'Java' semantics.
/**
  * @note the backing field holding the value must be named `x`, as it is accessed through reflection.
  */
abstract class AtomicCompanion[+V[T] <: InOut[T]] extends AtomicOps[V] {

	/** A plain factory method equivalent to `new V[T]`. This method relies only on automatic specialization,
	  * and does not return manually specialized classes. For that, see [[AtomicCompanion.newSpecific newSpecific]].
	  * This means that the returned variable may be illegal for `T=:=Boolean` or `T <:< AnyRef`.
	  */
	protected def newInstance[@specialized(SpecializedVars) T](init :T) : V[T]
	protected def newRefInstance[T](init :T) :V[T]
	protected def newBoolInstance(init :Boolean) :V[Boolean]

	protected def newSpecific[@specialized(SpecializedVars) T](init :T) :V[T] = newInstance(init) match {
		case ref if ref.getClass == CaseRef    => ref //Essentially, new V[T](init) is a valid instance.
		case any if any.getClass == CaseUnspec => newRefInstance(init) //Manually specialized instance for reference types.
		case bool if bool.getClass == CaseBool => newBoolInstance(init.asInstanceOf[Boolean]).asInstanceOf[V[T]]
		case res => res
	}

	protected override def getAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], newValue :T) :T =
		(v.getClass match {
			case CaseRef | CaseUnspec    => refHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Any])
			case CaseInt                 => intHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Int])
			case CaseLong                => longHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Long])
			case CaseDouble              => doubleHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Double])
			case CaseBool | CaseBoolSpec => boolHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Boolean])
			case CaseFloat               => floatHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Float])
			case CaseByte                => byteHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Byte])
			case CaseChar                => charHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Char])
			case CaseShort               => shortHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Short])
			case _                       => refHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Any]) //CaseUnspec
		}).asInstanceOf[T]

	protected final def getAndSetRef[T](v :AtomicOps.AtomicVar[T], newValue :T) :T =
		refHandle.getAndSet(v :AnyRef, newValue.asInstanceOf[Any])
//
//	private def getAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], field :VarHandle, newValue :T) :T =
//		field.getAndSet(v :AnyRef, newValue)

	protected override def testAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T)
			:Boolean =
		v.getClass match {
			case CaseRef | CaseUnspec    =>
				genericTestAndSet(v, expect, newValue)
//				refHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any])
			case CaseInt      =>
				intHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Int], newValue.asInstanceOf[Int])
			case CaseLong     =>
				longHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Long], newValue.asInstanceOf[Long])
			case CaseDouble   =>
				doubleHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Double], newValue.asInstanceOf[Double])
			case CaseBool | CaseBoolSpec =>
				boolHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Boolean], newValue.asInstanceOf[Boolean])
			case CaseFloat    =>
				floatHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Float], newValue.asInstanceOf[Float])
			case CaseByte     =>
				byteHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Byte], newValue.asInstanceOf[Byte])
			case CaseChar     =>
				charHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Char], newValue.asInstanceOf[Char])
			case CaseShort    =>
				shortHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Short], newValue.asInstanceOf[Short])
			case _            =>
				refHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any]) //CaseUnspec
		}
//
//	protected def testAndSet[@specialized(SpecializedVars) T]
//	                        (v :AtomicOps.AtomicVar[T], field :VarHandle, expect :T, newValue :T) :Boolean =
//		field.compareAndSet(v :AnyRef, expect, newValue)

	protected override def weakTestAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T)
			:Boolean =
		v.getClass match {
			case CaseRef | CaseUnspec    =>
				genericWeakTestAndSet(v, expect, newValue)
//				refHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any])
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
				genericWeakTestAndSet(v, expect, newValue)
//				refHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any]) //CaseUnspec
		}

	protected override def testAndSetRef[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		refHandle.compareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any])

	protected override def weakTestAndSetRef[T](v :AtomicOps.AtomicVar[T], expect :T, newValue :T) :Boolean =
		refHandle.weakCompareAndSet(v :AnyRef, expect.asInstanceOf[Any], newValue.asInstanceOf[Any])

	protected override def weakTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean, newValue :Boolean) :Boolean =
		boolHandle(v).weakCompareAndSet(v, expect, newValue)

	protected override def repeatTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean,
	                                            ifExpected :Boolean, ifNotExpected :Boolean) :Boolean =
	{
		val handle = boolHandle(v)
		var x = expect
		while (!handle.weakCompareAndSet(v, if (x == expect) ifExpected else ifNotExpected))
			x = !x
		if (x == expect) ifExpected else ifNotExpected
	}

	private def boolHandle(variable :InOut[Boolean]) :VarHandle = variable.getClass match {
		case CaseBool | CaseBoolSpec => boolHandle
		case _                       => refHandle
	}

	private[this] val CaseByte     = newInstance(0.toByte).getClass
	private[this] val CaseShort    = newInstance(0.toShort).getClass
	private[this] val CaseChar     = newInstance(0.toChar).getClass
	private[this] val CaseInt      = newInstance(0).getClass
	private[this] val CaseLong     = newInstance(0L).getClass
	private[this] val CaseDouble   = newInstance(0.0).getClass
	private[this] val CaseFloat    = newInstance(0.0f).getClass
	private[this] val CaseBool     = newInstance(false).getClass
	private[this] val CaseBoolSpec = newBoolInstance(false).getClass
	private[this] val CaseRef      = newRefInstance(new AnyRef).getClass
	private[this] val CaseUnspec   = newInstance(new AnyRef).getClass

	private[this] val byteHandle   = newHandle(CaseByte, java.lang.Byte.TYPE)
	private[this] val shortHandle  = newHandle(CaseShort, java.lang.Short.TYPE)
	private[this] val charHandle   = newHandle(CaseChar, java.lang.Character.TYPE)
	private[this] val intHandle    = newHandle(CaseInt, java.lang.Integer.TYPE)
	private[this] val longHandle   = newHandle(CaseLong, java.lang.Long.TYPE)
	private[this] val floatHandle  = newHandle(CaseFloat, java.lang.Float.TYPE)
	private[this] val doubleHandle = newHandle(CaseDouble, java.lang.Double.TYPE)
	private[this] val boolHandle   = newHandle(CaseBool, java.lang.Boolean.TYPE)
	private[this] val refHandle    = newHandle(newRefInstance(null).getClass, classOf[Any])

	private def newHandle[T](varType :Class[_], paramType :Class[_]) =
		MethodHandles.lookup().findVarHandle(varType, scalaFieldName(varType, valueVariableName), paramType)

	protected val valueVariableName = "x"
}



abstract class AtomicRefCompanion[+V[X] <: AtomicOps.AtomicVar[X]] extends AtomicCompanion[V] {
	protected override def newRefInstance[T](init :T) :V[T] = newInstance(init)
	protected override def newBoolInstance(init :Boolean) :V[Boolean] = newInstance(init)
	protected override def newSpecific[@specialized(SpecializedVars) T](init :T) :V[T] = newInstance(init)

	protected override def getAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T], newValue :T) :T =
		getAndSetRef(v, newValue)

	protected override def testAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T],
	                                                                   expect :T, newValue :T) :Boolean =
		testAndSetRef(v, expect, newValue)

	protected override def weakTestAndSet[@specialized(SpecializedVars) T](v :AtomicOps.AtomicVar[T],
	                                                                       expect :T, newValue :T) :Boolean =
		weakTestAndSetRef(v, expect, newValue)

	protected override def weakTestAndSetBool(v :AtomicOps.AtomicVar[Boolean], expect :Boolean, newValue :Boolean)
			:Boolean =
		??!

	protected override def repeatTestAndSetBool(v :AtomicOps.AtomicVar[Boolean],
	                                            expect :Boolean, ifExpected :Boolean, ifNotExpected :Boolean) :Boolean =
		??!
}



/**
  * @define ApplyDoc In a specialized context, the returned $Ref will be a `@specialized` class
  *                  with a field of type `T`. When `T` is generic (not known at call site), the created variable
  *                  will have an erased reference field. For implementations which use referential identity
  *                  rather than value equality, the semantics of instances created by specialized
  *                  and non specialized calls may differ.
  */
abstract class AtomicFactory[+V[X] <: AtomicOps.AtomicVar[X]] extends AtomicCompanion[V] {

	/** Create a new $ref of the specified initial value, which can be shared by multiple threads. $ApplyDoc
	  */
	def apply[@specialized(SpecializedVars) T](init :T) :V[T] = newSpecific(init)

	/** Create a new $ref which can be shared by multiple threads. The variable will be initialized
	  * with the default value for type `T`, provided by its type class. $ApplyDoc
	  */
	def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :V[T] = apply(default.get)

	/** Creates a properly `@specialized` $ref in a generic context based on implicit class information for `T`. */
	def generic[T](init :T)(implicit tag :ClassTag[T]) :V[T] = {
		val cls = tag.runtimeClass
		if (cls.isPrimitive) (cls : @nowarn) match {
			case classes.Int     => apply(init.asInstanceOf[Int]).asInstanceOf[V[T]]
			case classes.Long    => apply(init.asInstanceOf[Long]).asInstanceOf[V[T]]
			case classes.Double  => apply(init.asInstanceOf[Double]).asInstanceOf[V[T]]
			case classes.Boolean => apply(init.asInstanceOf[Boolean]).asInstanceOf[V[T]]
			case classes.Char    => apply(init.asInstanceOf[Char]).asInstanceOf[V[T]]
			case classes.Byte    => apply(init.asInstanceOf[Byte]).asInstanceOf[V[T]]
			case classes.Float   => apply(init.asInstanceOf[Float]).asInstanceOf[V[T]]
			case classes.Short   => apply(init.asInstanceOf[Short]).asInstanceOf[V[T]]
		} else
			apply(init)
	}


	/** Creates a properly `@specialized` $ref in a generic context based on an implicit class information for `T`. */
	def generic[T](implicit default :DefaultValue[T]) :V[T] = (default.get : @nowarn) match {
		case _ if !default.get.getClass.isBox => apply(default.get)
		case v :Integer     => apply[Int](v).asInstanceOf[V[T]]
		case v :Long        => apply[Long](v).asInstanceOf[V[T]]
		case v :Double      => apply[Double](v).asInstanceOf[V[T]]
		case v :Float       => apply[Float](v).asInstanceOf[V[T]]
		case v :Char        => apply[Char](v).asInstanceOf[V[T]]
		case v :Boolean     => apply[Boolean](v).asInstanceOf[V[T]]
		case v :Byte        => apply[Byte](v).asInstanceOf[V[T]]
		case v :Short       => apply[Short](v).asInstanceOf[V[T]]
	}
}
