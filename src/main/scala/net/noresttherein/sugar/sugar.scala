package net.noresttherein

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION


/**
  * @see [[net.noresttherein.sugar.imports]]
  * @author Marcin Mościcki
  */
package object sugar extends sugar.imports {
	private[sugar] final val Ver = 1L

	@elidable(ASSERTION) @inline def notNull[X](x :X, msg: => String) :X =
		if (x == null) throw new AssertionError(msg)
		else x

	@elidable(ASSERTION) @inline def notNull[X](x :X) :X =
		if (x == null) throw new AssertionError("null")
		else x

	private final val ShowCollectionsInExceptionsProperty = this.getClass.getName + ".showCollectionsInExceptions"

	/** A debugging flag telling the collections to include full argument information in thrown exceptions if possible.
	  *  Warning: may produce huge strings! Do not use in production code.
	  * Set by system property `net.noresttherein.sugar.showCollectionsInExceptions`. Defaults to `false`.
	  */ //todo: use this flag in util.errorString
	private[sugar] val ShowCollectionsInExceptions :Boolean =
		System.getProperty(ShowCollectionsInExceptionsProperty, "false").toBoolean
}
//todo: add SerialVersionUID/Serializable base trait to all objects, witness classes, YesNo, EqSet,
