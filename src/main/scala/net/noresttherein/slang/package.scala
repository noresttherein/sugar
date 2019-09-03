package net.noresttherein

import scala.reflect.{classTag, ClassTag}
import scala.util.Try

/**
  * @author Marcin Mościcki
  */
package object slang {

	def !?? :Nothing = throw new ImplementationError

	class ImplementationError(msg :String, reason :Throwable) extends Error(msg, reason) {
		def this(msg :String) = this(msg, null)

		def this() = this("Implementation error", null)
	}

	private[slang] final def raise[E<:Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure", ex)
		}).get

	private[slang] def raise[E<:Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure", ex)
		}).get


}
