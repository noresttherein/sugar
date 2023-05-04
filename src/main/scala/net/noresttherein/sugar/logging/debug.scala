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
	/** Log the given value to `System.out` before returning it. */
	def out[X](x :X) :X = { System.out.print(x); x }

	/** Log the given value, followed by line end, to `System.out` and return it. */
	def outln[X](x :X) :X = { System.out.println(x); x }

	/** Log the given value `System.err` and return it. */
	def err[X](x :X) :X = { System.err.print(x); x }

	/** Log the given value, followed by line end, to `System.out` and return it. */
	def errln[X](x :X) :X = { System.err.println(x); x }


	/** Wraps the expression in a `try-catch` block and prints any caught exceptions to the console
	  * before rethrowing them.
	  */
	def thrown[X](block: => X) :X =
		try block catch {
			case e :Exception =>
				System.err.println(e)
				e.printStackTrace(System.err)
				throw e
		}
	
	
	def apply(prefix :String) :PrefixedPrinter = new PrefixedPrinter(prefix)
	
	class PrefixedPrinter(private val prefix :String) extends AnyVal {
		def out[X](x :X) :X = { System.out.print(prefix); System.out.print(x); x }
		
		def outln[X](x :X) :X = { System.out.print(prefix); System.out.println(x); x }
		
		def err[X](x :X) :X = { System.err.print(prefix); System.err.print(x); x }
		
		def errln[X](x :X) :X = { System.err.print(prefix); System.err.println(x); x }
	}
	

	
	
	def apply(log :JLogger) :LogPrinter = new LogPrinter(log)

	class LogPrinter private[debug] (private val log :JLogger) extends AnyVal {
		def severe[X](x :X) :X = { log.severe(x.toString); x }
		def warning[X](x :X) :X = { log.warning(x.toString); x }
		def info[X](x :X) :X = { log.info(x.toString); x }
		def config[X](x :X) :X = { log.config(x.toString); x }
		def fine[X](x :X) :X = { log.fine(x.toString); x }
		def finer[X](x :X) :X = { log.finer(x.toString); x }
	}




	def apply(log :JLogger, prefix :String) :PrefixedLog = new PrefixedLog(prefix, log)

	class PrefixedLog private[debug] (prefix :String, log :JLogger) {
		def severe[X](x :X) :X = { log.severe(prefix + x); x }
		def warning[X](x :X) :X = { log.warning(prefix + x); x }
		def info[X](x :X) :X = { log.info(prefix + x); x }
		def config[X](x :X) :X = { log.config(prefix + x); x }
		def fine[X](x :X) :X = { log.fine(prefix + x); x }
		def finer[X](x :X) :X = { log.finer(prefix + x); x }
	}
}
