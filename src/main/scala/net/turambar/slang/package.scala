package net.turambar

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

/**
  * @author Marcin Mościcki
  */
package object slang {
//	object ValidatedBlockExpression {
//		@inline final def apply[T](block: =>T) :T = block
//	}

	private[slang] final def raise[E<:Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure", ex)
		}).get

	private[slang] def raise[E<:Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.newInstance().asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure", ex)
		}).get


//	def required(condition :Boolean, msg : =>Any)
/*
	@inline final def validate[E<:Throwable :ClassTag](condition :Boolean, msg: =>String) :ValidatedBlockExpression.type =
		if (condition) {
			ValidatedBlockExpression
		} else raise[E](msg)

	@inline final def validate[E<:Throwable :ClassTag](condition :Boolean) :ValidatedBlockExpression.type =
		if (condition)
			ValidatedBlockExpression
		else raise[E]
*/

}
