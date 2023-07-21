package net.noresttherein.sugar.loggingtests

import java.util.logging.{Handler, LogRecord, SimpleFormatter}

import scala.collection.mutable.Buffer

import org.scalacheck.Prop.AnyOperators
import org.scalacheck.{Properties, Test}
import org.scalacheck.util.ConsoleReporter

import net.noresttherein.sugar.extensions.StringExtension
import net.noresttherein.sugar.logging.Logger
import net.noresttherein.sugar.logging.Logger.Level.{Config, Fine, Finer, Finest, Info, Severe, Warn}
import net.noresttherein.sugar.logging.Logger.{Level, NamingScheme}
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.testing.scalacheck.extensions.BooleanAsPropExtension




//need to put it in a different package, as Logger ignores package logging.
object LoggerSpec extends Properties("Logger") {
	def sourceName(method :String, line :Int) = s"${getClass.getName}(LoggerSpec.scala:$line) $method"

	def severe(log :Logger, msg :() => String) = { log.severe(msg()); sourceName("severe", 25) }
	def warn  (log :Logger, msg :() => String) = { log.warn(msg());   sourceName("warn", 26) }
	def info  (log :Logger, msg :() => String) = { log.info(msg());   sourceName("info", 27) }
	def config(log :Logger, msg :() => String) = { log.config(msg()); sourceName("config", 28) }
	def fine  (log :Logger, msg :() => String) = { log.fine(msg());   sourceName("fine", 29) }
	def finer (log :Logger, msg :() => String) = { log.finer(msg());  sourceName("finer", 30) }
	def finest(log :Logger, msg :() => String) = { log.finest(msg()); sourceName("finest", 31) }

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(3, 140))

	val TestFormat = "%4$-6s %2$s: %5$s%n%6$s"
	System.setProperty(classOf[SimpleFormatter].getName + ".format", TestFormat)

	implicit val logger :Logger = Logger(this)

	property("Logger(String)") = Logger("dumb").name ?= "dumb"

	property("Logger()") = Logger().name ?= getClass.name

	property("Logger(Class[_])") = Logger(getClass).name ?= getClass.name

	property("Logger(Any)") =
		(Logger(this)(NamingScheme.ClassName).name ?= getClass.getName) :| "NamingScheme.ClassName" &&
		(Logger(this)(NamingScheme.DemangledClassName).name ?= getClass.name) :| "NamingScheme.DemangledClassName" &&
		(Logger(this)(NamingScheme.AbbrevClassName).name ?= getClass.abbrevName) :| "NamingScheme.AbbrevClassName"


	property("severe") = loggerProperty(severe, Severe)
	property("warn")   = loggerProperty(warn, Warn)
	property("info")   = loggerProperty(info, Info)
	property("config") = loggerProperty(config, Config)
	property("fine")   = loggerProperty(fine, Fine)
	property("finer")  = loggerProperty(finer, Finer)
	property("finest") = loggerProperty(finest, Finest)

	def loggerProperty(log :(Logger, () => String) => String, level :Level) = {
		val handler = new TestHandler
		val name = "LoggerSpec.test." + level.toString
		val logger = Logger(name)
		logger.toJava.setUseParentHandlers(false)
		logger.toJava.getHandlers.foreach(logger.toJava.removeHandler)
		logger.toJava.addHandler(handler)
		logger.level = Level(level.toInt - 1) //isLoggable()
		val msg = logger.level.toString + " message 1"
		val source = log(logger, () => msg)
		logger.level = Level(level.toInt + 1)
		var evaluated = false
		log(logger, () => { evaluated = true; logger.level.toString + " message 2" })

		(handler.buffer.map("'" + _ + "'").toSeq ?= Seq(
			"'" + level.toString.rpad(6) + " " + source + ": " + msg + "\n'"
		)) && (!evaluated lbl
			"message at level " + (level.toInt + 1) + " was evaluated on a logger with level " + logger.level.toInt
		)
	}

}




class TestHandler extends Handler {
	val formatter = new SimpleFormatter
	val buffer = Buffer.empty[String]

	override def publish(record :LogRecord) :Unit = buffer += formatter.format(record)
	override def flush() :Unit = ()
	override def close() :Unit = ()
}
