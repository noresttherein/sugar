package net.noresttherein.slang.logging

import java.util.logging.{LogRecord, Level => JLevel, Logger => JLogger}
import java.util.logging.Level._

import net.noresttherein.slang.logging.Logger.Level
import net.noresttherein.slang.logging.Logger.NamingScheme.DemangledClassName
import net.noresttherein.slang.prettyprint.{abbrevClassNameOf, classNameOf, fullNameOf}




/** Simple syntactic wrapper over `java.util.logging.Logger` available by the
  * [[net.noresttherein.slang.logging.Logger.toJava toJava]] property.
  * All logging methods investigate the frame stack to find the first class from outside the
  * `net.noresttherein.slang.logging` package and fill the log record with the file name and line number
  * in addition to (available by default) class and method name. Note that, depending on the java `LogManager`
  * configuration, this information may be approximate, as some classes and packages can be excluded
  * from the frame stack, including any system and reflected calls.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
class Logger(val toJava :JLogger) extends AnyVal with Serializable {

	/** The name given to this logger and used for configuration. */
	@inline def name :String = toJava.getName

	/** The level at which and above this logger currently logs. */
	def level :Level = toJava.getLevel match {
		case null => parent.get.level
		case lvl => lvl
	}

	/** Parent logger or `None` for the root logger. */
	def parent :Option[Logger] = toJava.getParent match {
		case null => None
		case log => Some(new Logger(log))
	}


	/** Checks if this logger logs at the given level. */
	@inline def logsAt(level :Level) :Boolean = toJava.isLoggable(level)


	/** Logs the message at the given level. */
	def log(level :Level, msg: =>Any) :Unit =
		if (toJava.isLoggable(level)) {
			val record = new LogRecord(level, msg.toString)
			FindCaller.fill(record)
			toJava.log(record)
		}

	/** Logs the given message with an attached exception at the specified level. */
	def log(level :Level, msg: =>Any, e :Throwable) :Unit =
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

	@inline def apply(logger :JLogger) :Logger = new Logger(logger)

	@inline def apply(name :String) :Logger = new Logger(JLogger.getLogger(name))

	@inline def apply(cls :Class[_]) :Logger = new Logger(JLogger.getLogger(fullNameOf(cls)))

	@inline def apply(owner :Any)(implicit naming :NamingScheme = DemangledClassName) :Logger =
		new Logger(JLogger.getLogger(naming(owner)))



	/** Simple wrapper over `java.util.logging.Level` providing logging functionality by the use of an implicit logger.
	  * Constants for standard Java logging levels are available in the companion object. Note that using level objects
	  * for logging offers a very simple way of performing arbitrary mappings of these levels to a more tranditional set
	  * by simple value definitions such as
	  * {{{
	  *     final val debug = Level.Finer
	  *     final val info = Level.Info
	  *     final val warning = Level.Warn
	  *     final val error = Level.Severe
	  * }}}
	  */
	@SerialVersionUID(1L)
	class Level private[Logger](val toJava :JLevel) extends AnyVal with Serializable {
		@inline def isOn(implicit logger :Logger) :Boolean = logger.logsAt(this)

		@inline final def apply(msg: => Any)(implicit logger :Logger) :Unit =
			logger.log(this, msg)

		@inline final def apply(msg: =>Any, e :Throwable)(implicit logger :Logger) :Unit =
			logger.log(this, msg, e)
	}

	object Level {
		final val All :Level = JLevel.ALL
		final val Severe :Level = JLevel.SEVERE
		final val Warn :Level = JLevel.WARNING
		final val Info :Level = JLevel.INFO
		final val Config :Level = JLevel.CONFIG
		final val Fine :Level = JLevel.FINE
		final val Finer :Level = JLevel.FINER
		final val Finest :Level = JLevel.FINEST
		final val Off :Level = JLevel.OFF

		def apply(level :JLevel) :Level = new Level(level)

		implicit def fromJava(level :JLevel) :Level = new Level(level)

		implicit def toJava(level :Level) :JLevel = level.toJava
	}



	/** A SAM type producing the name of the logger, given its owning object.
	  * @see [[net.noresttherein.slang.prettyprint]]
	  */
	trait NamingScheme extends Serializable {
		def apply(owner :Any) :String
	}

	object NamingScheme {
		final val ClassName          :NamingScheme = _.getClass.getName
		final val DemangledClassName :NamingScheme = classNameOf
		final val AbbrevClassName    :NamingScheme = abbrevClassNameOf
	}



	/** Default format for use with `java.util.logging.SimpleFormatter`. */
	final val SimpleFormat = "[%1$Tm.%1$Td %1$TH:%1$TM:%1$TS.%1$TL] %4$-6s %2$s: %5$s%n%6$s"

}

