package net.noresttherein.sugar

import java.util.logging.{Handler, Level=>JLevel}




package object logging {
	private[logging] final val Ver = 1L

	type JavaLogger = java.util.logging.Logger
	type JavaLevel  = java.util.logging.Level

	val BlackHoleHandler = new BlackHoleHandler
}

package logging {
	import java.util.logging.LogRecord


	@SerialVersionUID(Ver)
	class BlackHoleHandler extends Handler {
		setLevel(JLevel.OFF)

		override def publish(record :LogRecord) :Unit = ()
		override def flush() :Unit = ()
		override def close() :Unit = ()
	}
}
