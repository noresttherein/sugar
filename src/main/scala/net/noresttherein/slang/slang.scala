package net.noresttherein

import scala.reflect.{classTag, ClassTag}
import scala.util.Try

/**
  * @author Marcin Mościcki
  */
package object slang {

	/** A minimal pipe which adds a `>>` method to objects of any type which passes the enriched object to the given function. */
	implicit class >>[X](private val self: X) extends AnyVal {
		/** Passes `this` argument to the given function, returning the value. */
		@inline def >>[Y](f :X => Y) :Y = f(self)
	}



	def !?? :Nothing = throw new ImplementationError

	class ImplementationError(msg :String, reason :Throwable) extends Error(msg, reason) {
		def this(msg :String) = this(msg, null)

		def this() = this("Implementation error", null)
	}



	private[slang] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (String) or (String, Throwable).",
				ex
			)
		}).get

	private[slang] def raise[E <: Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance("").asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance("", null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (), (String) or (String, Throwable).",
				ex
			)
		}).get






	private[slang] def publishMutable() :Unit = java.lang.invoke.VarHandle.releaseFence()

}
