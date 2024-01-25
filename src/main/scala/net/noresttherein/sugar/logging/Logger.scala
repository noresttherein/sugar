package net.noresttherein.sugar.logging

import java.lang.StackWalker.StackFrame
import java.util.logging.{LogRecord, Level => JLevel, Logger => JLogger}
import java.util.logging.Level._

import net.noresttherein.sugar.logging.Logger.Level
import net.noresttherein.sugar.logging.Logger.Level.{Config, Fine, Finer, Finest, Info, Severe, Warn}
import net.noresttherein.sugar.logging.Logger.NamingScheme.DemangledClassName
import net.noresttherein.sugar.reflect.CallerFrame
import net.noresttherein.sugar.reflect.prettyprint.{abbrevClassNameOf, classNameOf, demangledName, fullNameOf}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




/** Simple syntactic wrapper over `java.util.logging.Logger` available by the
  * [[net.noresttherein.sugar.logging.Logger.toJava toJava]] property.
  * All logging methods investigate the frame stack to find the first class from outside the
  * `net.noresttherein.sugar.logging` sugar and fill the log record with the file name and line number
  * in addition to (available by default) class and method name. Note that, depending on the java `LogManager`
  * configuration, this information may be approximate, as some classes and packages can be excluded
  * from the frame stack, including any system and reflected calls.
  * @author Marcin Mościcki marcin@moscicki.net
  */
class Logger(val toJava :JLogger) extends AnyVal with Serializable {

	/** The name given to this logger and used for configuration. */
	@inline def name :String = toJava.getName

	/** The level at which and above this logger currently logs. */
	@inline def level :Level = toJava.getLevel match {
		case null => parent.get.level
		case lvl  => lvl
	}
	@inline def level_=(level :Level) :Unit = toJava.setLevel(level.toJava)

	/** Parent logger or `None` for the root logger. */
	@inline def parent :Maybe[Logger] = toJava.getParent match {
		case null => No
		case log  => Yes(new Logger(log))
	}


	/** Checks if this logger logs at the given level. */
	@inline def logsAt(level :Level) :Boolean = toJava.isLoggable(level)

	//consider: using an implicit LogContext
	/** Logs the message at the given level. */
	@inline def apply(level :Level, msg: => Any) :Unit =
		if (toJava.isLoggable(level))
			level.log(msg, No)(this, LogContext.reflected)

	/** Logs the given message with an attached exception at the specified level. */
	@inline def apply(level :Level, msg: => Any, thrown :Throwable) :Unit =
		if (toJava.isLoggable(level))
			level.log(msg, Maybe(thrown))(this, LogContext.reflected)

	@inline def apply(level :Level, method :String, msg: => Any) :Unit =
		if (toJava.isLoggable(level))
			level.log(msg, No)(this, new LogContext(method))
	
	@inline def apply(level :Level, method :String, msg: => Any, thrown :Throwable) :Unit =
		if (toJava.isLoggable(level))
			level.log(msg, Maybe(thrown))	(this, new LogContext(method))
	
	@inline def severe(msg: => Any) :Unit = apply(Severe, msg)
	@inline def severe(msg: => Any, thrown :Throwable) :Unit = apply(Severe, msg, thrown)
	@inline def severe(method :String, msg: => Any) :Unit = apply(Severe, method, msg)
	@inline def severe(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Severe, method, msg, thrown)

	/** Same as [[net.noresttherein.sugar.logging.Logger.severe severe]]`(msg)`. */
	@inline def error(msg: => Any) :Unit = apply(Severe, msg)

	/** Same as [[net.noresttherein.sugar.logging.Logger.severe severe]]`(msg, e)`. */
	@inline def error(msg: => Any, thrown :Throwable) :Unit = apply(Severe, msg, thrown)
	
	/** Same as [[net.noresttherein.sugar.logging.Logger.severe severe]]`(msg, e)`. */
	@inline def error(method :String, msg: => Any) :Unit = apply(Severe, method, msg)
	
	/** Same as [[net.noresttherein.sugar.logging.Logger.severe severe]]`(msg, e)`. */
	@inline def error(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Severe, method, msg, thrown)

	@inline def warn(msg: => Any) :Unit = apply(Warn, msg)
	@inline def warn(msg: => Any, thrown :Throwable) :Unit = apply(Warn, msg, thrown)
	@inline def warn(method :String, msg: => Any) :Unit = apply(Warn, method, msg)
	@inline def warn(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Warn, method, msg, thrown)

	@inline def info(msg: => Any) :Unit = apply(Info, msg)
	@inline def info(msg: => Any, thrown :Throwable) :Unit = apply(Info, msg, thrown)
	@inline def info(method :String, msg: => Any) :Unit = apply(Info, method, msg)
	@inline def info(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Info, method, msg, thrown)

	@inline def config(msg: => Any) :Unit = apply(Config, msg)
	@inline def config(msg: => Any, thrown :Throwable) :Unit = apply(Config, msg, thrown)
	@inline def config(method :String, msg: => Any) :Unit = apply(Config, method, msg)
	@inline def config(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Config, method, msg, thrown)

	@inline def fine(msg: => Any) :Unit = apply(Fine, msg)
	@inline def fine(msg: => Any, thrown :Throwable) :Unit = apply(Fine, msg, thrown)
	@inline def fine(method :String, msg: => Any) :Unit = apply(Fine, method, msg)
	@inline def fine(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Fine, method, msg, thrown)

	@inline def finer(msg: => Any) :Unit = apply(Finer, msg)
	@inline def finer(msg: => Any, thrown :Throwable) :Unit = apply(Finer, msg, thrown)
	@inline def finer(method :String, msg: => Any) :Unit = apply(Finer, method, msg)
	@inline def finer(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Finer, method, msg, thrown)

	@inline def finest(msg: => Any) :Unit = apply(Finest, msg)
	@inline def finest(msg: => Any, thrown :Throwable) :Unit = apply(Finest, msg, thrown)
	@inline def finest(method :String, msg: => Any) :Unit = apply(Finest, method, msg)
	@inline def finest(method :String, msg: => Any, thrown :Throwable) :Unit = apply(Finest, method, msg, thrown)

	override def toString :String = "Logger(" + name + ")"
}



@SerialVersionUID(Ver)
object Logger {

	/** Creates a proxy logger to a [[java.util.logging.Logger]] named after the class of the caller of this method. */
	def apply() :Logger =
		new Logger(JLogger.getLogger(demangledName(CallerFrame.outside(PackageName).getClassName)))

	/** Creates a proxy logger the given `java.util.logging.Logger`. */
	def apply(logger :JLogger) :Logger = new Logger(logger)

	/** Creates a logger of the given name, matching the global logging configuration. */
	def apply(name :String) :Logger = {
		if (name == null)
			throw new NullPointerException("Cannot create a Logger with a null name.")
		val log = JLogger.getLogger(name)
		if (log == null)
			throw new NullPointerException("No logger named '" + name + "'")
		new Logger(log)
	}

	/** Creates a logger named after the given class. */
	def apply(cls :Class[_]) :Logger = {
		val log = JLogger.getLogger(fullNameOf(cls))
		if (log == null)
			throw new NullPointerException("No logger for class '" + cls.getName + "'.")
		new Logger(log)
	}

	/** Creates a logger named after the owning object. The name is derived from `owner` using an implicit
	  * naming strategy, which defaults to prettified class name. Alternative strategies are defined
	  * in */
	@inline def apply(owner :Any)(implicit naming :NamingScheme = DemangledClassName) :Logger =
		Logger(naming(owner))


	/** Simple wrapper over `java.util.logging.Level` providing logging functionality by the use of an implicit logger.
	  * Constants for standard Java logging levels are available in the companion object. Note that using level objects
	  * for logging offers a very simple way of performing arbitrary mappings of these levels to a more traditional set
	  * by simple value definitions such as
	  * {{{
	  *     final val debug   = Level.Finer
	  *     final val info    = Level.Info
	  *     final val warning = Level.Warn
	  *     final val error   = Level.Severe
	  * }}}
	  * By default, all log records will be filled with reflected information, containing the caller's class,
	  * calling method name, as well as the exact source file name and line, if available.
	  * However, as it incurs some overhead, the calling class may opt to specify the name of the calling method
	  * itself. It is passed as an implicit [[net.noresttherein.sugar.logging.LogContext LogContext]] parameter
	  * to logging [[net.noresttherein.sugar.logging.Logger.Level.apply apply]] methods of this class.
	  * This is normally done by surrounding method's source code with a `LogContext` block:
	  * {{{
	  *     implicit val logger = Logger(this)
	  *     def attack(monster :Monster) = Log("attack", monster) { implicit method =>
	  *         val dmg = random.Int(primaryWeapon.minDamage, primaryWeapon.maxDamage)
	  *         Finer(s"dealt $dmg damage to $monster")
	  *         monster.withHitPoints(monster.hitPoints - dmg)
	  *     }
	  * }}}
	  */
	@SerialVersionUID(Ver)
	class Level private[Logger](val toJava :JLevel) extends AnyVal with Serializable {
		@inline def isOn(implicit logger :Logger) :Boolean = logger.logsAt(this)

		@inline def toInt :Int = toJava.intValue

		@inline final def apply(msg: => Any)(implicit logger :Logger, method :LogContext) :Unit =
			if (logger.toJava.isLoggable(toJava))
				log(msg, No)

		@inline final def apply(msg: => Any, e :Throwable)(implicit logger :Logger, method :LogContext) :Unit =
			if (logger.toJava.isLoggable(toJava))
				log(msg, Maybe(e))

		private[Logger] def log(msg :Any, thrown :Maybe[Throwable])(implicit logger :Logger, method :LogContext) :Unit = {
			val record = composer(logger, Yes(method.methodName), this, Maybe(msg), thrown)
			logger.toJava.log(record)
		}

		override def toString :String = toJava.toString
	}




	@SerialVersionUID(Ver)
	object Level {
		final val All    :Level = JLevel.ALL
		final val Severe :Level = JLevel.SEVERE
		final val Warn   :Level = JLevel.WARNING
		final val Info   :Level = JLevel.INFO
		final val Config :Level = JLevel.CONFIG
		final val Fine   :Level = JLevel.FINE
		final val Finer  :Level = JLevel.FINER
		final val Finest :Level = JLevel.FINEST
		final val Off    :Level = JLevel.OFF

		def apply(level :JLevel) :Level = new Level(level)

		def apply(level :String) :Level = new Level(JLevel.parse(level))

		def apply(level :Int) :Level = new Level(JLevel.parse(level.toString))

		implicit def LevelFromJava(level :JLevel) :Level = new Level(level)
		implicit def LevelToJava(level :Level) :JLevel = level.toJava
	}



	/** A SAM type producing the name of the logger, given its owning object.
	  * Several strategies are defined in the companion object
	  * [[net.noresttherein.sugar.logging.Logger.NamingScheme$ NamingScheme]], and implicit values
	  * can also be imported from `NamingScheme.`[[net.noresttherein.sugar.logging.Logger.NamingScheme.implicits implicits]].
	  * Used in conjunction with [[net.noresttherein.sugar.logging.Logger$ Logger]]'s `apply` method.
	  * @see [[net.noresttherein.sugar.reflect.prettyprint]]
	  */
	trait NamingScheme extends Serializable {
		def apply(owner :Any) :String
	}

	@SerialVersionUID(Ver)
	object NamingScheme {
		final val ClassName          :NamingScheme = _.getClass.getName
		final val DemangledClassName :NamingScheme = classNameOf
		final val AbbrevClassName    :NamingScheme = abbrevClassNameOf

		object implicits {
			implicit final val AfterOwnerClassName          :NamingScheme = ClassName
			implicit final val AfterOwnerClassDemangledName :NamingScheme = DemangledClassName
			implicit final val AfterOwnerClassAbbrevName    :NamingScheme = AbbrevClassName
		}
	}


	private val composer :RecordComposer = RecordComposer()


	/** Default format for use with [[java.util.logging.SimpleFormatter]]. */
	final val SimpleFormat = "[%1$Tm.%1$Td %1$TH:%1$TM:%1$TS.%1$TL] %4$-6s %2$s: %5$s%n%6$s"

	private final val PackageName   = "net.noresttherein.sugar.logging"
	private final val PackagePrefix = "net.noresttherein.sugar.logging."

//	private val currentMethod = new ThreadLocal[String]
}



/** Factory of `LogRecord`s based on the `Logger` and logged information.
  * Can be swapped out by setting system property `"net.noresttherein.sugar.logging.RecordComposer"`
  * to the name of a `RecordComposer` subclass.
  */
@SerialVersionUID(Ver)
class RecordComposer extends Serializable {
	def apply(logger :Logger, method :Maybe[String], level :Level, msg :Maybe[Any], thrown :Maybe[Throwable]) :LogRecord = {
		var record = new LogRecord(level.toJava, msg.map(_.toString).orNull)
		record.setLoggerName(level.toJava.getName)
		record.setThrown(thrown.orNull)
		if (method.isEmpty || method.get == LogContext.reflected.methodName)
			record = fill(record, CallerFrame.outside(RecordComposer.PackagePrefix))
		else {
			record.setSourceClassName(logger.name)
			record.setSourceMethodName(method.get)
		}
		record
	}

	def fill(record :LogRecord, frame :StackFrame) :LogRecord = {
		record.setSourceClassName(frame.getClassName + "(" + frame.getFileName + ":" + frame.getLineNumber + ")")
		record.setSourceMethodName(frame.getMethodName)
		record
	}
}


@SerialVersionUID(Ver)
private object RecordComposer {
	def apply() :RecordComposer = {
		var className :String = null
		try {
			className = System.getProperty(RecordComposerProperty)
			if (className != null)
				Class.forName(className).getConstructor().newInstance().asInstanceOf[RecordComposer]
			else
				new RecordComposer
		} catch {
			case e :Exception if className == null =>
				Warn("Failed to get system property \"" + RecordComposerProperty + "\"", e)(
					Logger(Logger), new LogContext("RecordComposer")
				)
				new RecordComposer
			case e :Exception =>
				Warn("Failed to instantiate class " + className + " specified by system property \"" +
					RecordComposerProperty + "\", or it is not a subclass of " + classOf[RecordComposer].getName + ".",
					e
				)(Logger(Logger), new LogContext("RecordComposer"))
				new RecordComposer
		}
	}

	/** Name of the system property with a class name of an optional
	  * [[net.noresttherein.sugar.logging.RecordComposer RecordComposer]] implementation.
	  * Equals `"net.noresttherein.sugar.logging.RecordComposer"`.
	  */
	final val RecordComposerProperty = classOf[RecordComposer].getName

	private final val PackagePrefix = "net.noresttherein.sugar.logging."
}
