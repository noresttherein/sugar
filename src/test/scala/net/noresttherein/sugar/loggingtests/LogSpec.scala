package net.noresttherein.sugar.loggingtests

import java.util.logging.SimpleFormatter

import org.scalacheck.Prop.AnyOperators
import org.scalacheck.{Properties, Test}
import org.scalacheck.util.ConsoleReporter
import net.noresttherein.sugar.extensions.StringExtension
import net.noresttherein.sugar.logging.Log.LevelDef
import net.noresttherein.sugar.logging.{Log, Logger}
import net.noresttherein.sugar.logging.Logger.Level.{Finest, Severe}
import net.noresttherein.sugar.unsupported_!




object LogSpec extends Properties("Log") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(3, 140))

	val TestFormat = "%4$-6s %2$s: %5$s"
	System.setProperty(classOf[SimpleFormatter].getName + ".format", TestFormat)

	private def testLogger(name :String) = {
		val logger = Logger(name)
		val handler = new TestHandler
		logger.toJava.getHandlers.foreach(logger.toJava.removeHandler)
		logger.toJava.addHandler(handler)
		logger.toJava.setUseParentHandlers(false)
		logger.level = Finest
		(logger, handler.buffer)
	}

	property("apply") = Log("applyTest") { implicit m =>
		val name = "LogSpec.test.apply"
		val msg  = "apply test"
		implicit val (logger, buffer) = testLogger(name)
		Severe(msg)
		buffer.map("'" + _ + "'").toSeq ?= Seq(
			s"'SEVERE $name applyTest: $msg'"
		)
	}

	val entryMsg = "called with (boo)"
	val testMsg  = "Go for the eyes!"
	property("entry") = {
		val name = "LogSpec.test.entry"
		implicit val (logger, buffer) = testLogger(name)
		val entryLevel = implicitly[LevelDef["methodEntry"]]
		Log.entry("entryTest", "boo") { implicit m =>
			Severe(testMsg)
			buffer.map("'" + _ + "'").toSeq ?= Seq(
				"'" + entryLevel.level.toString.rpad(6) + " " + name + " entryTest: " + entryMsg + "'",
				"'SEVERE " + name + " entryTest: " + testMsg + "'"
			)
		}
	}

	val returnMsg = "returning (Minsc)"
	property("surround") = {
		val name = "LogSpec.test.surround"
		implicit val (logger, buffer) = testLogger(name)
		val entryLevel  = implicitly[LevelDef["methodEntry"]]
		val returnLevel = implicitly[LevelDef["methodReturn"]]
		Log.surround("surroundTest", "boo") { implicit m =>
			Severe(testMsg)
			"Minsc"
		}
		buffer.map("'" + _ + "'").toSeq ?= Seq(
			"'" + entryLevel.level.toString.rpad(6) + " " + name + " surroundTest: " + entryMsg + "'",
			"'SEVERE " + name + " surroundTest: " + testMsg + "'",
			"'" + returnLevel.level.toString.rpad(6) + " " + name + " surroundTest: " + returnMsg + "'"
		)
	}

	val errorMsg = "throwing java.lang.UnsupportedOperationException: No eyes!"
	property("surround.throw") = {
		val name = "LogSpec.test.surround"
		implicit val (logger, buffer) = testLogger(name)
		val entryLevel  = implicitly[LevelDef["methodEntry"]]
		val errorLevel  = implicitly[LevelDef["methodError"]]
		try {
			Log.surround("surroundTest", "boo") { implicit m =>
				Severe(testMsg)
				unsupported_!("No eyes!")
			}
		} catch {
			case _ :UnsupportedOperationException =>
		}
		buffer.map("'" + _ + "'").toSeq ?= Seq(
			"'" + entryLevel.level.toString.rpad(6) + " " + name + " surroundTest: " + entryMsg + "'",
			"'SEVERE " + name + " surroundTest: " + testMsg + "'",
			"'" + errorLevel.level.toString.rpad(6) + " " + name + " surroundTest: " + errorMsg + "'"
		)
	}
}
