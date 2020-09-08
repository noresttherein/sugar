package net.noresttherein.slang.logging

import java.util.logging.{LogRecord, Level => JLevel, Logger => JLogger}
import java.util.logging.Level._
import net.noresttherein.slang.logging.Logger.Level


/** Simple syntactic wrapper over `java.util.logging.Logger` available by the
  *  [[net.noresttherein.slang.logging.Logger#toJava toJava]] property.
  *  All logging methods investigate the frame stack to find the first class from outside the
  *  `net.noresttherein.slang.logging` package and fill the log record with the file name and line number
  *  in addition to (available by default) class and method name. Note that, depending on the java `LogManager`
  *  configuration, this information may be approximate, as some classes and packages can be excluded
  *  from the frame stack, including any system and reflected calls.
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Logger(val toJava :JLogger) extends AnyVal {

	/** The name given to this logger and used for configuration. */
	@inline def name :String = toJava.getName

	/** The level at which and above this logger currently logs. */
	def level :Level = toJava.getLevel match {
		case null => parent.get.level
		case lvl => lvl
	}

	/** Parent logger or `None` for the root logger. */
	@inline def parent :Option[Logger] = toJava.getParent match {
		case null => None
		case log => Some(new Logger(log))
	}


	/** Checks if this logger logs at the given level. */
	@inline def logsAt(Level :Level) :Boolean = toJava.isLoggable(level)


	/** Logs the message at the given level. */
	@inline def log(level :Level, msg: =>Any) :Unit =
		if (toJava.isLoggable(level)) {
			val record = new LogRecord(level, msg.toString)
			FindCaller.fill(record)
			toJava.log(record)
		}

	/** Logs the given message with an attached exception at the specified level. */
	@inline def log(level :Level, msg: =>Any, e :Throwable) :Unit =
		if (toJava.isLoggable(level)) {
			val record = new LogRecord(level, msg.toString)
			record.setThrown(e)
			FindCaller.fill(record)
			toJava.log(record)
		}


	@inline def severe(msg: => Any) :Unit = log(SEVERE, msg)

	@inline def severe(msg: => Any, e :Throwable) :Unit = log(SEVERE, msg, e)

	@inline def warning(msg: => Any) :Unit = log(WARNING, msg)

	@inline def warning(msg: => Any, e :Throwable) :Unit = log(WARNING, msg, e)

	@inline def info(msg: => Any) :Unit = log(INFO, msg)

	@inline def info(msg: => Any, e :Throwable) :Unit = log(INFO, msg, e)

	@inline def config(msg: => Any) :Unit = log(CONFIG, msg)

	@inline def config(msg: => Any, e :Throwable) :Unit = log(CONFIG, msg, e)

	@inline def fine(msg: => Any) :Unit = log(FINE, msg)

	@inline def fine(msg: => Any, e :Throwable) :Unit = log(FINE, msg, e)

	@inline def finer(msg: =>Any) :Unit = log(FINER, msg)

	@inline def finer(msg: => Any, e :Throwable) :Unit = log(FINER, msg, e)

	@inline def finest(msg: => Any) :Unit = log(FINEST, msg)

	@inline def finest(msg: => Any, e :Throwable) :Unit = log(FINEST, msg, e)


}



object Logger {

	/** Simple wrapper over `java.util.logging.Level` providing logging functionality with use of an implicit logger.  */
	class Level private[Logger](val toJava :JLevel) extends AnyVal {
		@inline def isOn(implicit logger :Logger) :Boolean = logger.logsAt(this)

		@inline final def apply(msg: => Any)(implicit logger :Logger) :Unit =
			logger.log(this, msg)

		@inline final def apply(msg: =>Any, e :Throwable)(implicit logger :Logger) :Unit =
			logger.log(this, msg, e)
	}

	object Level {
		final val Severe :Level = JLevel.SEVERE
		final val Warn :Level = JLevel.WARNING
		final val Info :Level = JLevel.INFO
		final val Config :Level = JLevel.CONFIG
		final val Fine :Level = JLevel.FINE
		final val Finer :Level = JLevel.FINER
		final val Finest :Level = JLevel.FINEST

		implicit def fromJava(level :JLevel) :Level = new Level(level)

		implicit def toJava(level :Level) :JLevel = level.toJava
	}

	@inline def apply(logger :JLogger) :Logger = new Logger(logger)

	@inline def apply(name :String) :Logger = new Logger(JLogger.getLogger(name))

	@inline def apply(cls :Class[_]) :Logger = new Logger(JLogger.getLogger(cls.getName))


	/** Default format for use with `java.util.logging.SimpleFormatter`. */
	final val SimpleFormat = "[%1$Tm.%1$Td %1$TH:%1$TM:%1$TS.%1$TL] %4$-6s %2$s: %5$s%n%6$s"

}

