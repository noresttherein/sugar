package net.noresttherein.sugar.exceptions

import java.io.{PrintWriter, StringWriter}

import scala.Exception

import net.noresttherein.sugar.extensions.ThrowableExtension
import org.scalacheck.{Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter



class TestException(msg :String, cause :Throwable, val rethrows :Int) extends Exception(msg, cause) {
	def this(msg :String, cause :TestException) = this(msg, cause, cause.rethrows + 1)
	def this(msg :String) = this(msg, null, 0)
	def this() = this("Root cause")
}

object TestException {
	def throwAndCatch(callDepth :Int = 5, rethrows :Int = 5, suppressed :Int = 2, suppressedDepth :Int = 2) :Exception =
		try {
			recurse(0, 0, callDepth, rethrows, suppressed, suppressedDepth, "Root ", "Rethrow ")
			??!
		} catch {
			case e :TestException => e
		}
	private def recurse(currentCallDepth :Int, tryCatch :Int, callDepth: Int, rethrows: Int,
	                    suppressed :Int, suppressedDepth :Int,
	                    rootCaption :String, rethrowCaption :String) :Int =
		if (currentCallDepth == callDepth)
			if (tryCatch >= rethrows)
				throwRoot(callDepth, suppressed, suppressedDepth, rootCaption) + 1
			else
				rethrowCaught(
					tryCatch + 1, callDepth, rethrows, suppressed, suppressedDepth, rootCaption, rethrowCaption
				) + 1
		else
			recurse(
				currentCallDepth + 1, tryCatch,
				callDepth, rethrows, suppressed, suppressedDepth, rootCaption, rethrowCaption
			) + 1

	private def throwRoot(callDepth :Int, suppressed :Int, suppressedDepth :Int, msg :String) :Int = {
		val root = new TestException(msg)
		(0 until suppressed) foreach { i =>
			try {
				recurse(0, 0, callDepth, suppressedDepth, suppressed - 1, suppressedDepth - 1,
					"Suppressed root " + i, "Wrapped suppressed ")
			} catch {
				case e :TestException =>
					root.addSuppressed(e)
			}
		}
		throw root
	}

	private def rethrowCaught(tryCatch :Int, callDepth :Int, rethrows :Int,
	                          suppressed :Int, suppressedDepth :Int, rootCaption :String, rethrowCaption :String) :Int =
		try {
			recurse(
				0, tryCatch, callDepth, rethrows,
				suppressed, suppressedDepth, rootCaption, rethrowCaption
			)
		} catch {
			case e :TestException =>
				val rethrown = new TestException(rethrowCaption + (e.rethrows + 1), e)
				(0 until suppressed - 1) foreach { i =>
					try {
						recurse(0, 0, callDepth, suppressedDepth - 1, 0, 0,
							"Suppressed " + i, "Rethrow suppressed"
						)
					} catch {
						case e :TestException =>
							rethrown.addSuppressed(e)
					}
				}
				throw rethrown
		}

}



object SugaredExceptionSpec extends Properties("SugaredException") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(3, 140))//.withMinSuccessfulTests(100)

	//	val e = TestException.throwAndCatch(2, 2)
	//	Console.out.println(e.reverseStackTraceString)

	private def stackTraceStringProperty(depth :Int, rethrows :Int) = {
		val e = TestException.throwAndCatch(depth, rethrows)
		val writer = new StringWriter
		val printer = new PrintWriter(writer)
		e.printStackTrace(printer)
		printer.close()
		e.stackTraceString ?= writer.toString
	}

	property("stackTraceString.single") = stackTraceStringProperty(5, 0)
	property("stackTraceString.rethrown") = stackTraceStringProperty(5, 3)
}
