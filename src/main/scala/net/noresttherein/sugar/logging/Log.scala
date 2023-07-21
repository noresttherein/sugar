package net.noresttherein.sugar.logging

import java.util.logging.{Level=>JLevel}

import net.noresttherein.sugar.logging.Logger.Level




@SerialVersionUID(Ver)
object Log {
	/** Executes the given block, passing a `LogContext` for initialized with the given method name as the argument.
	  * If the argument is declared as implicit, all calls to
	  * [[net.noresttherein.sugar.logging.Logger.Level Level]]`.`[[net.noresttherein.sugar.logging.Logger.Level.apply apply]]
	  * will set the `sourceMethodName` property of the logged [[java.util.logging.LogRecord LogRecord]]
	  * to the specified value, instead of reflecting the information by an inspection of the calling stack.
	  * @return the value returned by `body`.
	  */
	@inline def apply[T](methodName :String)(body :LogContext => T) :T =
		body(new LogContext(methodName))

	/** Logs the information about entering a method of the specified name.
	  * The level at which the fact is logged can be specified either per file, by declaring an implicit
	  * `LevelDef["methodEntry"]`, or per application by setting system property
	  * `"net.noresttherein.sugar.logging.methodEntryLevel"` to a level name, as understood by
	  * [[java.util.logging.Level.parse Level.parse]] method of `java.util.logging.Level`.
	  *
	  * This method is designed to surround the whole body of a method, given as the `body` function.
	  * A `LogContext` initialized with the given method name is passed as the argument to the function.
	  * If the argument is declared as implicit, all calls to
	  * [[net.noresttherein.sugar.logging.Logger.Level Level]]`.`[[net.noresttherein.sugar.logging.Logger.Level.apply apply]]
	  * will set the `sourceMethodName` property of the logged [[java.util.logging.LogRecord LogRecord]]
	  * to the specified value, instead of reflecting the information by an inspection of the calling stack.
	  *
	  * Usage example:
	  * {{{
	  *     implicit val logger = Logger(this)
	  *     def enter(dungeon :Dungeon) :Loot = Log.entry("enter", dungeon) { implicit method =>
	  *         dungeon.monsters.foldLeft(Loot.empty) { (loot, monster) =>
	  *             fight(monster)
	  *             if (hitPoints <= 0) {
	  *                 Severe(s"Killed by $monster!") //logs using "enter" as the method name
	  *                 throw DeathException()
	  *             }
	  *             loot ++ monster.loot
	  *         }
	  *     }
	  * }}}
	  * @param methodName the name of the surrounded method `body`.
	  * @param args       the arguments given to the calling method.
	  * @param body       the body of the logging method.
	  * @param logger     the logger belonging to the object whose methods is called.
	  * @param level      a wrapper of [[net.noresttherein.sugar.logging.Logger.Level Level]], specifying
	  *                   the level at which the information about entering the method is logged.
	  * @see [[net.noresttherein.sugar.logging.Log.surround surround]]
	  */
	@inline def entry[T](methodName :String, args :Any*)(body :LogContext => T)
	                    (implicit logger :Logger, level :LevelDef["methodEntry"]) :T =
	{
		implicit val method :LogContext = new LogContext(methodName)
		level.level(args.mkString("called with (", ", ", ")"))
		body(method)
	}

	/** Surrounds a body of a method, logging information about entering it and returning.
	  * This method is designed to surround the whole body of a method, given as the `body` function.
	  * A `LogContext` initialized with the given method name is passed as the argument to the function.
	  * If the argument is declared as implicit, all calls to
	  * [[net.noresttherein.sugar.logging.Logger.Level Level]]`.`[[net.noresttherein.sugar.logging.Logger.Level.apply apply]]
	  * will set the `sourceMethodName` property of the logged [[java.util.logging.LogRecord LogRecord]]
	  * to the specified value, instead of reflecting the information by an inspection of the calling stack.
	  *
	  * Before executing `body`, a log record containing method's parameters is logged using the implicit logger,
	  * at the level specified by `entryLevel` implicit argument. When `body` returns, the returned value
	  * is similarly logged at level `returnLevel`. If, instead, `body` throws an exception, it is logged,
	  * without its stack trace, at level `errorLevel`.
	  *
	  * All level arguments have default implicit values, which can be set globally by setting corresponding
	  * system properties. It is, however, also easy to override any of them for any sub-file scope
	  * by declaring implicit values of the appropriate type.
	  * @usecase {{{
	  *             implicit val logger = Logger(this)
	  *             def quaffAle(ale :Tankard) :Boast = Log.surround("quaffAle", ale) { implicit method =>
	  *                 intoxication = intoxication + ale.content.strength * ale.capacity max 100
	  *                 Fine(s"I'm $intoxication% drunk!") //logs using "enter" as the method name
	  *                 Boast.randomBoast(credulity = intoxication)
	  *             }
	  *          }}}
	  * @param methodName  the name of the surrounded method `body`.
	  * @param args        the arguments given to the calling method.
	  * @param body        the body of the logging method.
	  * @param logger      the logger belonging to the object whose methods is called.
	  * @param entryLevel  a wrapper of [[net.noresttherein.sugar.logging.Logger.Level Level]], specifying
	  *                    the level at which the information about entering the method is logged.
	  *                    Can be set globally as a system property `"net.noresttherein.sugar.logging.methodEntryLevel"`,
	  *                    equal to a known level name, or a numeric value defining the granularity of the level.
	  * @param returnLevel a wrapper of [[net.noresttherein.sugar.logging.Logger.Level Level]], specifying
	  *                    the level at which the information about normal exiting the method is logged.
	  *                    Can be set globally as a system property `"net.noresttherein.sugar.logging.methodReturnLevel"`,
	  *                    equal to a known level name, or a numeric value defining the granularity of the level.
	  * @param errorLevel  a wrapper of [[net.noresttherein.sugar.logging.Logger.Level Level]], specifying
	  *                    the level at which the information about the method throwing an exception is logged.
	  *                    Can be set globally as a system property `"net.noresttherein.sugar.logging.methodErrorLevel"`,
	  *                    equal to a known level name, or a numeric value defining the granularity of the level.
	  * @see [[net.noresttherein.sugar.logging.Log.entry entry]]
	  */
	@inline def surround[T](methodName :String, args :Any*)(body :LogContext => T)
	                       (implicit logger :Logger, entryLevel :LevelDef["methodEntry"],
	                                 returnLevel :LevelDef["methodReturn"], errorLevel :LevelDef["methodError"]) :T =
	{
		implicit val method :LogContext = new LogContext(methodName)
		try {
			if (logger.toJava.isLoggable(entryLevel.javaLevel))
				entryLevel.level(args.mkString("called with (", ", ", ")"))
			val res = body(method)
			if (logger.toJava.isLoggable(returnLevel.javaLevel))
				returnLevel.level("returning (" + res + ")")
			res
		} catch {
			case e :Exception if logger.toJava.isLoggable(errorLevel.javaLevel) =>
				//Doesn't log the stack trace as it will be logged eventually,
				// when the exception is caught by the application.
				errorLevel.level("throwing " + e)
				throw e
		}
	}

	final val MethodEntryLoggingLevelProperty  = getClass.getPackage.getName + ".methodEntryLevel"
	final val MethodReturnLoggingLevelProperty = getClass.getPackage.getName + ".methodReturnLevel"
	final val MethodErrorLoggingLevelProperty = getClass.getPackage.getName + ".methodErrorLevel"

	/** Specifies the logging level for operation whose name is given as the string literal type parameter. */
	@SerialVersionUID(Ver)
	class LevelDef[O <: String with Singleton](val javaLevel :JLevel) extends AnyVal {
		@inline def level :Level = Level(javaLevel)
		override def toString :String = javaLevel.toString
	}

	/** The default level at which methods [[net.noresttherein.sugar.logging.Log.entry entry]]
	  * and [[net.noresttherein.sugar.logging.Log.surround surround]] log parameters given to its method.
	  * Can be specified globally through a system property
	  * [[net.noresttherein.sugar.logging.Log.MethodEntryLoggingLevelProperty MethodEntryLoggingLevelProperty]]
	  * (`"net.noresttherein.sugar.logging.methodErrorLevel"`).
	  */
	implicit val MethodEntryLoggingLevel :LevelDef["methodEntry"] =
		loggingLevelProperty(MethodEntryLoggingLevelProperty, JLevel.FINER)

	/** The default level at which method [[net.noresttherein.sugar.logging.Log.surround surround]]
	  * logs the value returned by a method. Can be specified globally through a system property
	  * [[net.noresttherein.sugar.logging.Log.MethodReturnLoggingLevelProperty MethodReturnLoggingLevelProperty]]
	  * (`"net.noresttherein.sugar.logging.methodReturnLevel"`).
	  */
	implicit val MethodReturnLevel :LevelDef["methodReturn"] =
		loggingLevelProperty(MethodReturnLoggingLevelProperty, JLevel.FINEST)

	/** The default level at which method [[net.noresttherein.sugar.logging.Log.surround surround]]
	  * logs the information that a method exits by throwing an exception.
	  * Can be specified globally through a system property
	  * [[net.noresttherein.sugar.logging.Log.MethodErrorLoggingLevelProperty MethodErrorLoggingLevelProperty]]
	  * (`"net.noresttherein.sugar.logging.methodErrorLevel"`).
	  */
	implicit val MethodErrorLevel :LevelDef["methodError"] =
		loggingLevelProperty(MethodErrorLoggingLevelProperty, JLevel.FINER)

	private def loggingLevelProperty[O <: String with Singleton](property :String, default :JLevel) :LevelDef[O] = {
		var levelName :String = null
		try {
			levelName = System.getProperty(property, default.toString)
			new LevelDef(JLevel.parse(levelName))
		} catch {
			case e :Exception if levelName != null =>
				//Providing the logger and method name explicitly, because automatic naming ignores package logging.
				Level.Warn(
					"Invalid logging level '" + property + "' property value: '" + levelName + "'.", e
				)(Logger(classOf[Log.type]), new LogContext("loggingLevelProperty"))
				new LevelDef(default)
			case e :Exception =>
				//Providing the logger and method name explicitly, because automatic naming ignores package logging.
				Level.Warn(
					"Could not find logging level property '" + property + "'. Going with default " + default + ".", e
				)(Logger(classOf[Log.type]), new LogContext("loggingLevelProperty"))
				new LevelDef(default)
		}
	}

}




/** Contextual information added to every [[java.util.logging.LogRecord LogRecord]] in presence of an implicit value.
  * Specifies the name of the currently executed method. A special, default implicit value exists,
  * which will cause the logging class, method, and name be retrieved by inspecting the call stack.
  * A locally declared implicit will however override it, allowing to specify the name of the calling method explicitly.
  * It is primarily used in conjunction with [[net.noresttherein.sugar.logging.Log Log]]'s methods
  * [[net.noresttherein.sugar.logging.Log.apply apply]], [[net.noresttherein.sugar.logging.Log.entry entry]],
  * and [[net.noresttherein.sugar.logging.Log.surround surround]] methods.
  */
@SerialVersionUID(Ver)
class LogContext(val methodName :String) extends AnyVal

@SerialVersionUID(Ver)
object LogContext {
	implicit val reflected :LogContext = new LogContext(classOf[LogContext].getName + ".reflected")
}
