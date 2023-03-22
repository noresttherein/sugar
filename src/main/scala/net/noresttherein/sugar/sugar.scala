package net.noresttherein

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION


/**
  * @see [[net.noresttherein.sugar.imports]]
  * @author Marcin Mościcki
  */
package object sugar extends sugar.imports {
	final val Ver = 1L

	@elidable(ASSERTION) @inline def notNull[X](x :X, msg: => String) :X =
		if (x == null) throw new AssertionError(msg)
		else x

	@elidable(ASSERTION) @inline def notNull[X](x :X) :X =
		if (x == null) throw new AssertionError("null")
		else x
}
//todo: add SerialVersionUID/Serializable base trait to all objects, witness classes, YesNo, EqSet,
