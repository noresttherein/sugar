package net.noresttherein.slang.logging

import java.util.logging.{Level, LogRecord, Logger => JLogger}

//todo use jav:a.lang.StackWalker to provide caller information

/** Simple syntactic wrapper over `java.util.logging.Logger` available by the [[Logger.jlogger]] property.
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Logger(val jlogger :JLogger) extends AnyVal {
	@inline def name :String = jlogger.getName

	def level :Level = jlogger.getLevel match {
		case null => parent.get.level
		case lvl => lvl
	}

	@inline def parent :Option[Logger] = jlogger.getParent match {
		case null => None
		case log => Some(new Logger(log))
	}


	@inline def logsAt(level :Level) :Boolean = jlogger.isLoggable(level)




	@inline def severe(msg: => Any) :Unit =
		if (jlogger.isLoggable(Level.SEVERE))
			jlogger.severe(msg.toString)

	@inline def severe(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.SEVERE))
			jlogger.log(Level.SEVERE, msg.toString, e)

	@inline def warning(msg: => Any) :Unit =
		if (jlogger.isLoggable(Level.WARNING))
			jlogger.warning(msg.toString)

	@inline def warning(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.WARNING))
			jlogger.log(Level.WARNING, msg.toString, e)

	@inline def info(msg: => Any) :Unit =
		if (jlogger.isLoggable(Level.INFO))
			jlogger.info(msg.toString)

	@inline def info(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.INFO))
			jlogger.log(Level.INFO, msg.toString, e)

	@inline def config(msg: => Any) :Unit =
		if (jlogger.isLoggable(Level.CONFIG))
			jlogger.config(msg.toString)

	@inline def config(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.CONFIG))
			jlogger.log(Level.CONFIG, msg.toString, e)

	@inline def fine(msg: => Any) :Unit =
		if (jlogger.isLoggable(Level.FINE))
			jlogger.fine(msg.toString)

	@inline def fine(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.FINE))
			jlogger.log(Level.FINE, msg.toString, e)

	@inline def finer(msg: =>Any) :Unit =
		if (jlogger.isLoggable(Level.FINER))
			jlogger.finer(msg.toString)

	@inline def finer(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.FINER))
			jlogger.log(Level.FINER, msg.toString, e)

	@inline def finest(msg: => Any) :Unit =
		if (jlogger.isLoggable(Level.FINEST))
			jlogger.finest(msg.toString)

	@inline def finest(msg: => Any, e :Throwable) :Unit =
		if (jlogger.isLoggable(Level.FINEST))
			jlogger.log(Level.FINEST, msg.toString, e)


}



object Logger {

	@inline def apply(logger :JLogger) :Logger = new Logger(logger)

	@inline def apply(name :String) :Logger = new Logger(JLogger.getLogger(name))

}

