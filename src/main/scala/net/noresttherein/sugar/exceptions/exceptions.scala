package net.noresttherein.sugar

import java.lang.reflect.Constructor

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ArraySeq.ofRef
import scala.collection.mutable

import net.noresttherein.sugar.collections.MutableEqSet
import net.noresttherein.sugar.extensions.throwableExtension
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




package exceptions {
	private[exceptions] trait markerStackTraceElements {
		/* WHENEVER YOU EDIT THIS FILE UPDATE rethrowStackTraceElement WITH A CORRECT LINE NUMBER! */

		private[exceptions] def eval[T](action: => T) :T = action

		/** This method is never called. It is used as an artificial top stack trace element
		  * of cheaper [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] exceptions created and thrown
		  * by method [[net.noresttherein.sugar.imports.rethrow rethrow]], solely to provide this information
		  * here. These exceptions do not have their stack trace filled by the virtual machine, but initialize it instead
		  * with frames of the [[Throwable]] caught by `rethrow`, leading up to the call of `rethrow` itself.
		  * In order to minimize confusion coming from a stack trace leading to code which throws no exception,
		  * a final frame for this method is added to point programmers to this documentation.
		  */ //remember to update the line number if you edit these docs!
		@nowarn private def conjureThrowable :Nothing = ??!

		private[exceptions] final val evalStackTraceElement =
			new StackTraceElement(classOf[markerStackTraceElements].getName, "eval", "exceptions.scala", 22)

		private[exceptions] final val conjureThrowableStackTraceElement =
			new StackTraceElement(classOf[markerStackTraceElements].getName, "conjureThrowable", "exceptions.scala", 32)

		private[exceptions] final val fillInStackTraceStackTraceElement =
			new StackTraceElement(classOf[Rethrowable].getName, "fillInStackTrace", "SugaredThrowable.scala", -1)

	}
}




package object exceptions extends exceptions.imports with exceptions.markerStackTraceElements {
	final val Ver = 1L


	/** Suffix (earlier frames) of the stack trace of the first cause with a non empty stack trace, starting with
	  * the first (most recent) frame for `imports.rethrow`.
	  */
	@tailrec private[exceptions] def stackTraceSuffix(e :Throwable) :Array[StackTraceElement] = e.getCause match {
		case null => e match {
			case e :SugaredThrowable => e.stackTrace match {
				case seq :ofRef[StackTraceElement] => seq.unsafeArray
				case seq => seq.toArray
			}
			case _ => e.getStackTrace
		}
		case cause =>
			val causeTrace = cause match {
				case e :SugaredThrowable => e.stackTrace match {
					case seq :ofRef[StackTraceElement] => seq.unsafeArray
					case other => other.toArray
				}
				case _ => cause.getStackTrace
			}
			if (causeTrace.isEmpty)
				stackTraceSuffix(cause)
			else {
				val causeTraceLength = causeTrace.length
				@tailrec def prefix(i :Int) :Array[StackTraceElement] =
					if (i >= causeTraceLength)
						EmptyStackTraceArray
					else if (matches(causeTrace(i), evalStackTraceElement)) {
//						causeTrace.drop(i)
						val sharedFrames = causeTrace.length - i
						val res = new Array[StackTraceElement](sharedFrames + 1)
						System.arraycopy(causeTrace, i, res, 1, sharedFrames)
						res(0) = conjureThrowableStackTraceElement
						res
					} else
						prefix(i + 1)
				prefix(0)
			}
	}

	/** Drops the top (leading) `fillInStackTrace` and `<init>` call frames from the stack. */
	private[exceptions] def dropFillInStackTraceFrames(trace :Array[StackTraceElement]) :Array[StackTraceElement] = {
		var i = 0
		while (i < trace.length && {
			val frame = trace(i)
			val method = frame.getMethodName
			val file = frame.getFileName
			(method.endsWith("fillInStackTrace") || method.endsWith("fillInStackTrace$")) &&
				(file == null || file == "SugaredThrowable.scala") || method.endsWith("<init>")
		}) i += 1

		i match {
			case 0 => trace
			case n if n == trace.length => EmptyStackTraceArray
			case n => trace.drop(n)
		}
	}

	@inline private def matches(frame :StackTraceElement, pattern :StackTraceElement) :Boolean =
		frame.getClassName == pattern.getClassName && frame.getMethodName == pattern.getMethodName &&
			(frame.getLineNumber < 0 || pattern.getLineNumber < 0 || frame.getLineNumber == pattern.getLineNumber) &&
			(frame.getFileName == null || pattern.getFileName == null || frame.getFileName == pattern.getFileName)

	/** Prints stack trace using `printer` argument.
	  * Used by [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] as this method
	  * doesn't attempt to use the internal initialization of the stack trace, but rather uses the public
	  * [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]]`.`[[net.noresttherein.sugar.exceptions.SugaredThrowable.stackTrace stackTrace]]
	  * or `Throwable.`[[Throwable.getStackTrace getStackTrace]] to access the stack, allowing the delegating exception
	  * to provide a different stack - in the case of `Rethrowable`, one initialized from its cause's stack trace.
	  *
	  * Must be externally synchronized on the outlet of `printer`.
	  * The code is shamelessly nicked and adapted from the reference implementation of [[Throwable]].
	  */
	private[exceptions] def printStackTrace(e :Throwable, enclosingTrace :Seq[StackTraceElement],
	                                        printer :AnyRef => Unit,
	                                        dejaVu :mutable.Set[Throwable] = dejaVuSet,
	                                        prefix :String = "", caption :String = "") :Unit =
		if (dejaVu(e)) {
			printer("\t[CIRCULAR REFERENCE:" + e + "]")
		} else {
			dejaVu += e
			// Compute number of frames in common between this and enclosing trace
			val trace = e.stackTrace
			var ourFrame = trace.length - 1
			var enclosingFrame = enclosingTrace.length - 1
			while (ourFrame >= 0 && enclosingFrame >= 0 && trace(ourFrame) == enclosingTrace(enclosingFrame)) {
				ourFrame -= 1; enclosingFrame -= 1
			}
			val sharedFrames = trace.length - 1 - ourFrame

			// Print e.stackTrace
			val framePrefix = prefix + "\tat "
			printer(prefix + caption + e)
			trace foreach { frame => printer(framePrefix + frame) }
			if (sharedFrames != 0)
				printer(prefix + "\t... " + sharedFrames + " more")

			// Print suppressed exceptions, if any
			e.suppressed foreach { suppressed =>
				printStackTrace(suppressed, trace, printer, dejaVu, prefix + "\t", SuppressedCaption)
			}
			// Print cause, if any
			val cause = e.getCause
			if (cause != null)
				printStackTrace(cause, trace, printer, dejaVu, prefix, CauseCaption)
		}


	private[this] final val SuppressedCaption = "Suppressed: "
	private[this] final val CauseCaption      = "Caused by: "
	private[this] final val EmptyStackTraceArray = new Array[StackTraceElement](0)
	private[exceptions] final val EmptyStackTrace = ArraySeq.unsafeWrapArray(EmptyStackTraceArray)

	private[exceptions] final val NoArgs = Array[Class[_]]()
	private[exceptions] final val StringArg = Array[Class[_]](classOf[String])
	private[exceptions] final val ThrowableArg = Array[Class[_]](classOf[Throwable])
	private[exceptions] final val StringThrowableArgs = Array[Class[_]](classOf[String], classOf[Throwable])
	private[exceptions] final val StringThrowableBoolArgs =
		Array[Class[_]](classOf[String], classOf[Throwable], classOf[Boolean])
	private[exceptions] final val StringThrowableBoolBoolArgs =
		Array[Class[_]](classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])

	private[exceptions] final def findConstructor(clazz :Class[_], paramTypes :Array[Class[_]]) :Opt[Constructor[_]] = {
		val constructors = clazz.getDeclaredConstructors
		var i = constructors.length - 1
		while (i >= 0 && !arraysEqual(constructors(i).getParameterTypes, paramTypes))
			i -= 1
		if (i >= 0) Got(constructors(i))
		else Lack
	}
	private[exceptions] final def arraysEqual(a :Array[_], b :Array[_]) :Boolean =
		a.length == b.length && {
			var i = a.length - 1
			while (i >= 0 && a(i) == b(i))
				i -= 1
			i < 0
		}


	private[exceptions] def dejaVuSet :mutable.Set[Throwable] = MutableEqSet.empty

	/** Reverses the order of the exception cause list defined by [[Throwable.getCause getCause]] method,
	  * placing the first (and last) exception without a cause as the first element of the returned list.
	  */
	@tailrec private[exceptions] def causeQueue(e :Throwable, result :List[Throwable] = Nil,
	                                            dejaVu :mutable.Set[Throwable] = dejaVuSet)
			:Seq[Throwable] =
		e.getCause match {
			case null => e::result
			case cause if dejaVu(cause) =>
				causeCycleException(e)::e::result
			case cause =>
				dejaVu += e
				causeQueue(cause, e::result, dejaVu)
		}
	private def causeCycleException(e :Throwable) =
		new IllegalStateException(s"Cycle detected in the exception stack: ${e.getCause} already present.")

	/** Descends down the cause list following the [[Throwable.getCause getCause]] methods to find the first exception
	  * without a cause. Guards against malicious cycle attempts.
	  */
	@tailrec private[exceptions] def originalCause(e :Throwable, dejaVu :mutable.Set[Throwable] = dejaVuSet) :Throwable =
		 e.getCause match {
			case null => e
			case cause if dejaVu(cause) => causeCycleException(e)
			case cause =>
				dejaVu += cause
				originalCause(cause, dejaVu)
		}
}


