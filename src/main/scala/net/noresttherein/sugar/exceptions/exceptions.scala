package net.noresttherein.sugar

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ArraySeq.ofRef
import scala.collection.mutable

import net.noresttherein.sugar.collection.MutableEqSet



package object exceptions extends exceptions.imports with exceptions.extensions {
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
		new SugarException(s"Cycle detected in the exception stack: ${e.getCause} already present.")

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

	/** Suffix (earlier frames) of the stack trace of the first cause with non empty stack trace, starting with
	  * the first (most recent) frame for `imports.rethrow`.
	  */
	@tailrec private[exceptions] def stackTraceSuffix(e :Throwable) :Array[StackTraceElement] = e.getCause match {
		case null => e match {
			case e :AbstractThrowable => e.stackTrace match {
				case seq :ofRef[StackTraceElement] => seq.unsafeArray
				case seq => seq.toArray
			}
			case _ => e.getStackTrace
		}
		case cause =>
			val causeTrace = cause match {
				case e :AbstractThrowable => e.stackTrace match {
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
					else if (matches(causeTrace(i), rethrowStackTraceElement)) {
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
				(file == null || file == "AbstractThrowable.scala") || method.endsWith("<init>")
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
	  * [[net.noresttherein.sugar.exceptions.AbstractThrowable AbstractThrowable]]`.`[[net.noresttherein.sugar.exceptions.AbstractThrowable.stackTrace stackTrace]]
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
			printer("\t[CIRCULAR REFERENCE:" + e + "]");
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

}


