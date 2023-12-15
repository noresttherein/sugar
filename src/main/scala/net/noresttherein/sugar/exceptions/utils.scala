package net.noresttherein.sugar.exceptions

import java.lang.System.arraycopy

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Buffer}

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.collections.MutableEqSet
import net.noresttherein.sugar.exceptions.extensions.ThrowableExtension
import net.noresttherein.sugar.text.EOL




private object utils {

	private[exceptions] def dejaVuSet :mutable.Set[Throwable] = MutableEqSet.empty

	/** Reverses the order of the exception cause list defined by [[Throwable.getCause getCause]] method,
	  * placing the first (and last) exception without a cause as the first element of the returned list.
	  */ //todo: use an ExportBuffer
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
	@tailrec private[exceptions] def rootCause(e :Throwable, dejaVu :mutable.Set[Throwable] = dejaVuSet) :Throwable =
		 e.getCause match {
			case null => e
			case cause if dejaVu(cause) => causeCycleException(e)
			case cause =>
				dejaVu += cause
				rootCause(cause, dejaVu)
		}

	@tailrec private[exceptions] def joinedStackTrace(
	                                 e :Throwable,
	                                 fullLastStackTrace :StackTrace = StackTrace.empty,
	                                 buffer :Buffer[StackTrace] = new ArrayBuffer[StackTrace],
	                                 dejaVu :mutable.Set[Throwable] = dejaVuSet)
			:Seq[StackTrace] =
	{
		val stack = e.stackTrace
		val stackSize = stack.length
		buffer += stack.dropSharedFrames(fullLastStackTrace)
//		if (stack.isEmpty)
//			buffer += Nil
//		else {
//			var last = buffer.length
//			val previousStackSize = fullLastStackTrace.length
//			if (previousStackSize == 0)
//				buffer += ArraySeq.unsafeWrapArray(stack)
//			else if (!matches(fullLastStackTrace.last, stack.last))
//				buffer += ArraySeq.unsafeWrapArray(stack)
//			else if (previousStackSize <= stackSize
//				&& matches(stack(stackSize - previousStackSize - 1), fullLastStackTrace(0))
//			)
//				buffer += ErasedArray.Wrapped.Slice.unsafe(stack, 0, previousStackSize - stackSize)
//			else { //bin search for the point where the stacks diverge
//				val length = math.min(stack.length, fullLastStackTrace.length)
//				var unmatched = length - 1
//				var matched = 0
//				while (matched != unmatched) {
//					val middle = (matched + unmatched + 1) >>> 1
//					if (matches(stack(stackSize - middle), fullLastStackTrace(previousStackSize - middle)))
//						matched = middle
//					else
//						unmatched = middle - 1
//				}
//				buffer += ErasedArray.Wrapped.Slice.unsafe(stack, 0, stackSize - matched)
//			}
//		}
		e.getCause match {
			case null =>
				buffer to ArraySeq
			case cause =>
				if (dejaVu.add(cause))
					joinedStackTrace(cause, if (stackSize == 0) fullLastStackTrace else stack, buffer, dejaVu)
				else {
					val cycleException = causeCycleException(cause)
					buffer += cycleException.stackTrace
					buffer to ArraySeq
				}
		}
	}

	/** Suffix (earlier frames) of the stack trace of the first cause with a non empty stack trace, starting with
	  * the first (most recent) frame for `imports.rethrow`.
	  */
	@tailrec private[exceptions] def stackTraceSuffix(e :Throwable) :Array[StackTraceElement] = e.getCause match {
		case null => e match {
			case e :SugaredThrowable => e.stackTrace match {
				case seq :ArrayStackTrace => seq.unsafeArray
				case seq => seq.toArray
			}
			case _ => e.getStackTrace
		}
		case cause =>
			val causeTrace = cause match {
				case e :SugaredThrowable => e.stackTrace match {
					case seq :ArrayStackTrace => seq.unsafeArray
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
						arraycopy(causeTrace, i, res, 1, sharedFrames)
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
	private[exceptions] def printStackTrace(e :Throwable, printer :AnyRef => Unit,
	                                        enclosingTrace :StackTrace = StackTrace.empty,
	                                        dejaVu :mutable.Set[Throwable] = dejaVuSet,
	                                        prefix :String = "", caption :String = "") :Unit =
		if (dejaVu(e)) {
			printer("\t[CIRCULAR REFERENCE:" + e + "]")
		} else {
			dejaVu += e
			// Compute number of frames in common between this and enclosing trace
			val trace = e.stackTrace
			val sharedFrames = trace.sharedSuffixLength(enclosingTrace)
			val stackSize    = trace.length - sharedFrames
			// Print e.stackTrace
			val framePrefix = prefix + "\tat "
			printer(prefix + caption + e)
			var i = 0
			while (i < stackSize) {
				printer(framePrefix + trace(i))
				i += 1
			}
			if (sharedFrames != 0)
				printer(prefix + "\t... " + sharedFrames + " more")

			// Print suppressed exceptions, if any
			e.suppressed foreach { suppressed =>
				printStackTrace(suppressed, printer, trace, dejaVu, prefix + "\t", SuppressedCaption)
			}
			// Print cause, if any
			val cause = e.getCause
			if (cause != null)
				printStackTrace(cause, printer, trace, dejaVu, prefix, CauseCaption)
		}

	/** Prints stack trace in the reverse order, using `printer` argument.
	  * Similar to [[Throwable.printStackTrace printStackTrace]], but the root cause is printed first, followed
	  * by any wrapping exceptions.
	  */
	private[exceptions] def printReverseStackTrace(e :Throwable, printer :AnyRef => Unit,
	                                               dejaVu :mutable.Set[Throwable] = dejaVuSet,
	                                               enclosingTrace :StackTrace = StackTrace.empty,
	                                               prefix :String = "", rootCaption :String = "") :Unit =
		if (dejaVu(e)) {
			printer("\t[CIRCULAR REFERENCE:" + e + "]")
		} else {
			dejaVu += e
			val cause = e.getCause
			val trace = e.stackTrace
			val caption =
				if (cause == null)
					rootCaption
				else { //first, print the cause
					printReverseStackTrace(cause, printer, dejaVu, enclosingTrace, prefix, rootCaption)
					WrappedCaption
				}
			printer(prefix + caption + e)
			// Compute number of frames in common between this and the wrapped exception
			val sharedFrames =
				if (cause == null) trace.sharedSuffixLength(enclosingTrace)
				else trace.sharedSuffixLength(cause.stackTrace)
			val stackSize = trace.length - sharedFrames
			// Print e.stackTrace
			val framePrefix = prefix + "\tat "
			var i = 0
			while (i < stackSize) {
				printer(framePrefix + trace(i))
				i += 1
			}
			if (sharedFrames != 0)
				printer(prefix + "\t... " + sharedFrames + " more")

			// Print suppressed exceptions, if any
			e.suppressed foreach { suppressed =>
				printReverseStackTrace(suppressed, printer, dejaVu, trace,prefix + "\t", SuppressedCaption)
			}
		}

	/** Formats the whole stack trace of `e` as a `String`
	  * in the same way as [[Throwable.printStackTrace printStackTrace]].
	  */
	private[exceptions] def stackTraceString(e :Throwable) :String = {
		val res = new JStringBuilder
		printStackTrace(e, res append _ append EOL)
		res.toString
	}

	private[exceptions] def reverseStackTraceString(e :Throwable) :String = {
		val res = new JStringBuilder
		printReverseStackTrace(e, res append _ append EOL)
		res.toString
	}
//
//	/** Formats `e` together with its stack trace in the standard format.
//	  * The first line of the returned `String` is equal to `e.toString`; the following lines
//	  * are the same as would be printed with `e.`[[Throwable.printStackTrace Throwable.printStackTrace]].
//	  */
//	private[exceptions] def formatWithStackTrace(e :Throwable) :String = {
//		val writer  = new StringWriter
//		val printer = new PrintWriter(writer)
//		printer.println(e)
//		printStackTrace(e, StackTrace.empty, printer.println)
//		printer.close()
//		writer.toString
//	}


	private[this] final val SuppressedCaption    = "Suppressed: "
	private[this] final val CauseCaption         = "Caused by: "
	private[this] final val WrappedCaption       = "Wrapped by: "
	private[this] final val EmptyStackTraceArray = new Array[StackTraceElement](0)
//	private[this] final val EmptyStackTrace      = ArraySeq.unsafeWrapArray(EmptyStackTraceArray)

}
