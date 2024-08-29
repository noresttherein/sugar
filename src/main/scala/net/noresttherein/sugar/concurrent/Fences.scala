package net.noresttherein.sugar.concurrent

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.lang.reflect.Modifier

import net.noresttherein.sugar.illegalState_!




//consider: making it @inline direct delegation to VarHandle
object Fences {
	/** Ensures that loads before the fence will not be reordered with loads and stores after the fence. */
	def acquireFence() :Unit = AcquireFence.invoke()

	/** Inserts an [[net.noresttherein.sugar.concurrent.Fences.acquireFence acquireFence]] before executing the
	  * argument expression and returning its value. It makes sure that all operations in the executed block
	  * (and those following this call in program order) ''happen after'' any memory read
	  * preceding the call in program order. If the argument expression reads the state of one or more contested objects
	  * (in whole or in part) through references obtained before this call in program order, this guarantees
	  * that the seen state is not newer than at the time the reference was assigned, and unaffected by any writes
	  * this process may perform in `value` or after this method returns.
	  */
	@inline def acquire[T](value : => T) :T = { acquireFence(); value }

	/** Ensures that loads and stores before the fence will not be reordered with stores after the fence. */
	def releaseFence() :Unit = ReleaseFence.invoke()

	/** Inserts a [[net.noresttherein.sugar.concurrent.Fences.releaseFence releaseFence]] before returning the argument.
	  * It makes sure that all operations preceding the call in program order (including the execution
	  * of the expression whose value is passed as the argument to this method) ''happen before'' any memory write
	  * following the call in program order. If the returned object is shared with other threads, those threads
	  * will see the effects of all writes executed by this thread before this method call, but its construction
	  * will not be affected by any writes executed by those threads before the object's reference was shared
	  * (returned by this method).
	  */
	@inline def release[T](value: T) :T = { releaseFence(); value }

	/** Ensures that loads and stores before the fence will not be reordered with loads and stores after the fence. */
	def fullFence() :Unit = FullFence.invoke()

	private[this] val ReleaseFence :MethodHandle = mkHandle("releaseFence", "storeFence")
	private[this] val AcquireFence :MethodHandle = mkHandle("acquireFence", "loadFence")
	private[this] val FullFence    :MethodHandle = mkHandle("fullFence", "fullFence")

	private def mkHandle(varHandleMethod :String, unsafeMethod :String) = {
		val lookup = MethodHandles.lookup
		try
			lookup.findStatic(
				Class.forName("java.lang.invoke.VarHandle"), varHandleMethod, MethodType.methodType(Void.TYPE)
			)
		catch {
			case e @ (_ :NoSuchMethodException | _ :ClassNotFoundException) =>
				try {
					val unsafeClass = Class.forName("sun.misc.Unsafe")
					lookup.findVirtual(unsafeClass, unsafeMethod, MethodType.methodType(Void.TYPE))
					      .bindTo(findUnsafe(unsafeClass))
				} catch {
					case e1 @ (_ :NoSuchMethodException | _ :ClassNotFoundException | _ :IllegalAccessException) =>
						val error = new ExceptionInInitializerError(e1)
						error.addSuppressed(e)
						throw error
				}
			case e :IllegalAccessException =>
				throw new ExceptionInInitializerError(e)
		}
	}

	private def findUnsafe(unsafeClass :Class[_]) :AnyRef = {
		for (field <- unsafeClass.getDeclaredFields) {
			if ((field.getModifiers & Modifier.STATIC) != 0 && (field.getType eq unsafeClass)) {
				field.setAccessible(true)
				return field.get(null)
			}
		}
		illegalState_!("No sun.misc.Unsafe found")
	}

}
