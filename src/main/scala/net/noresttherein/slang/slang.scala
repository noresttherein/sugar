package net.noresttherein






/**
  * @see [[net.noresttherein.slang.slangImports]]
  * @author Marcin Mościcki
  */
package object slang extends slangImports {

	class ThisShouldBeImpossibleException(msg :String, reason :Throwable) extends Error(msg, reason) {
		def this(msg :String) = this(msg, null)

		def this() = this("Implementation error", null)
	}



	private[slang] def publishMutable() :Unit = java.lang.invoke.VarHandle.releaseFence()

}
