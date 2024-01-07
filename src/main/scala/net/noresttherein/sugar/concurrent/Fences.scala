package net.noresttherein.sugar.concurrent

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}




private[sugar] object Fences {

	def acquireFence() :Unit = AcquireFence.invoke()
	def releaseFence() :Unit = ReleaseFence.invoke()

	private val ReleaseFence :MethodHandle = mkHandle("releaseFence", "storeFence")
	private val AcquireFence :MethodHandle = mkHandle("acquireFence", "loadFence")

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
			if (field.getType eq unsafeClass) {
				field.setAccessible(true)
				return field.get(null)
			}
		}
		throw new IllegalStateException("No sun.misc.Unsafe found")
	}

}
