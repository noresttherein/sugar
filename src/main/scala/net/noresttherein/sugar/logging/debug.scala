package net.noresttherein.sugar.logging

import java.util.logging.{Logger => JLogger}




/** Traditional ad-hoc methods which log a value for debugging purposes and immediately return it.
  * Rather than importing enclosed identifiers, consider importing this object and invoking the methods explicitly -
  * this way one can use the infix call notation potentially without parenthesis's, making it easier to quickly
  * add and remove the debug messages:
  * {{{
  *     debug outln x
  *     debug("x^2=") outln x * x
  *     debug(log) fine x * x
  * }}}
  */
@SerialVersionUID(Ver)
object debug {
	/** Log the given value to `Console.out` before returning it. */
	@inline def out[X](x :X) :X = { Console.out.print(x); x }

	/** Log the given value, followed by line end, to `Console.out` and return it. */
	@inline def outln[X](x :X) :X = { Console.out.println(x); x }

	/** Log the given value `Console.err` and return it. */
	@inline def err[X](x :X) :X = { Console.err.print(x); x }

	/** Log the given value, followed by line end, to `Console.out` and return it. */
	@inline def errln[X](x :X) :X = { Console.err.println(x); x }


	/** Wraps the expression in a `try-catch` block and prints any caught exceptions to the console
	  * before rethrowing them.
	  */
	def thrown[X](block: => X) :X =
		try block catch {
			case e :Exception =>
				Console.err.println(e)
				e.printStackTrace(Console.err)
				throw e
		}

	/** An object with the same printing methods as `debug`, but which prefixes every logging call
	  * with the specified prefix:
	  * {{{
	  *     debug("what is this, I don't know") outln stuff
	  * }}}
	  */
	def apply(prefix :String) :PrefixedPrinter = new PrefixedPrinter(prefix)
	
	class PrefixedPrinter(private val prefix :String) extends AnyVal {
		@inline def out[X](x :X) :X = { Console.out.print(prefix); Console.out.print(x); x }
		@inline def outln[X](x :X) :X = { Console.out.print(prefix); Console.out.println(x); x }
		@inline def err[X](x :X) :X = { Console.err.print(prefix); Console.err.print(x); x }
		@inline def errln[X](x :X) :X = { Console.err.print(prefix); Console.err.println(x); x }
	}


	/** An object with proxy logging methods to [[java.util.logging.Logger Logger]], which log the argument values
	  * immediately before returning them, just as `debug.outln` and the rest:
	  * {{{
	  *     debug(log) info printMeWhenIamDone()
	  * }}}
	  */
	@inline def apply(log :JLogger) :LogPrinter = new LogPrinter(log)

	class LogPrinter private[debug] (private val log :JLogger) extends AnyVal {
		@inline def severe[X](x :X) :X = { log.severe(x.toString); x }
		@inline def warning[X](x :X) :X = { log.warning(x.toString); x }
		@inline def info[X](x :X) :X = { log.info(x.toString); x }
		@inline def config[X](x :X) :X = { log.config(x.toString); x }
		@inline def fine[X](x :X) :X = { log.fine(x.toString); x }
		@inline def finer[X](x :X) :X = { log.finer(x.toString); x }
	}


	/** An object with proxy logging methods to [[java.util.logging.Logger Logger]], which log the argument values,
	  * prefixed with `prefix`, immediately before returning them:
	  * {{{
	  *     debug(log, "No idea what this amounts to: ") info 2 + 2
	  * }}}
	  */
	@inline def apply(log :JLogger, prefix :String) :PrefixedLogger = new PrefixedLogger(prefix, log)

	class PrefixedLogger private[debug](prefix :String, log :JLogger) {
		@inline def severe[X](x :X) :X = { log.severe(prefix + x); x }
		@inline def warning[X](x :X) :X = { log.warning(prefix + x); x }
		@inline def info[X](x :X) :X = { log.info(prefix + x); x }
		@inline def config[X](x :X) :X = { log.config(prefix + x); x }
		@inline def fine[X](x :X) :X = { log.fine(prefix + x); x }
		@inline def finer[X](x :X) :X = { log.finer(prefix + x); x }
	}
}
