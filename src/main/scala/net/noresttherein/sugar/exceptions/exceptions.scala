package net.noresttherein.sugar

import java.lang.System.arraycopy
import java.lang.reflect.Constructor

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ArraySeq.ofRef
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.collections.MutableEqSet
import net.noresttherein.sugar.extensions.{ThrowableExtension, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




package exceptions {
	private[exceptions] trait markerStackTraceElements {
		/* WHENEVER YOU EDIT THIS FILE UPDATE evalStackTraceElement WITH A CORRECT LINE NUMBER! */

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

//		private[exceptions] final val fillInStackTraceStackTraceElement =
//			new StackTraceElement(classOf[Rethrowable].getName, "fillInStackTrace", "SugaredThrowable.scala", 326)

	}
}




package object exceptions extends exceptions.imports with exceptions.markerStackTraceElements {
	private[exceptions] final val Ver = 1L


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
	private[exceptions] final val StringLazyStringThrowableArgs =
		Array[Class[_]](classOf[String], classOf[() => String], classOf[Throwable])
	private[exceptions] final val StringLazyStringThrowableBoolArgs =
		Array[Class[_]](classOf[String], classOf[() => String], classOf[Throwable], classOf[Boolean])
	private[exceptions] final val StringThrowableBoolBoolArgs =
		Array[Class[_]](classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
	private[exceptions] final val StringLazyStringThrowableBoolBoolArgs =
		Array[Class[_]](classOf[String], classOf[() => String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
	private[exceptions] final val LazyStringArg =
		Array[Class[_]](classOf[() => String])
	private[exceptions] final val LazyStringThrowableArgs =
		Array[Class[_]](classOf[() => String], classOf[Throwable])
	private[exceptions] final val LazyStringThrowableBoolArgs =
		Array[Class[_]](classOf[() => String], classOf[Throwable], classOf[Boolean])
	private[exceptions] final val LazyStringThrowableBoolBoolArgs =
		Array[Class[_]](classOf[() => String], classOf[Throwable], classOf[Boolean], classOf[Boolean])


	private[exceptions] final def defaultConstructor[T :ClassTag] :Opt[() => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(NoArgs).map(cons => () => cons.newInstance())
		).orElse(
			findConstructor(StringThrowableArgs).map { cons => () => cons.newInstance(null, null) }
		).orElse(
			findConstructor(StringArg).map { cons => () => cons.newInstance(null) }
		).orElse(
			findConstructor(StringThrowableBoolBoolArgs).map { cons => () => cons.newInstance(null, null, true, true) }
		).orElse(
			findConstructor(ThrowableArg).map { cons => () => cons.newInstance(null) }
		).orElse(
			findConstructor(LazyStringThrowableArgs).map { cons => () => cons.newInstance(null, null) }
		).orElse(
			findConstructor(LazyStringArg).map { cons => () => cons.newInstance(null) }
		).orElse(
			findConstructor(LazyStringThrowableBoolBoolArgs).map {
				cons => () => cons.newInstance(null, null, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => () => cons.newInstance(null, null, null, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map { cons => () => cons.newInstance(null, null, null) }
		)
	}

	private[exceptions] final def throwableConstructor[T <: Throwable :ClassTag] :Opt[Throwable => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(ThrowableArg).map { cons => cons.newInstance(_ :Throwable) }
		).orElse(
			findConstructor(StringThrowableArgs).map { cons => cons.newInstance(null, _ :Throwable) }
		).orElse(
			findConstructor(StringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(null, _ :Throwable, true, true)
			}
		).orElse(
			findConstructor(LazyStringThrowableArgs).map { cons => cons.newInstance(null, _ :Throwable) }
		).orElse(
			findConstructor(LazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(null, _ :Throwable, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(null, null, _ :Throwable)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(null, null, _ :Throwable, true, true)
			}
		)
	}

	private[exceptions] final def stringConstructor[T :ClassTag] :Opt[String => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(StringThrowableArgs).map { cons => cons.newInstance(_ :String, null) }
		).orElse(
			findConstructor(StringArg).map { cons => cons.newInstance(_ :String) }
		).orElse(
			findConstructor(StringThrowableBoolBoolArgs).map { cons => cons.newInstance(_ :String, null, true, true) }
		).orElse(
			findConstructor(LazyStringThrowableArgs).map { cons => msg :String => cons.newInstance(() => msg, null) }
		).orElse(
			findConstructor(LazyStringArg).map { cons => msg :String => cons.newInstance(() => msg) }
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :String, null, null, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map { cons => cons.newInstance(_ :String, null, null) }
		)
	}

	private[exceptions] final def stringThrowableConstructor[T <: Throwable :ClassTag] :Opt[(String, Throwable) => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(StringThrowableArgs).map { cons => cons.newInstance(_ :String, _ :Throwable) }
		).orElse(
			findConstructor(StringArg).map { cons => cons.newInstance(_ :String).initCause(_ :Throwable).downcastTo[T] }
		).orElse(
			findConstructor(StringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :String, _ :Throwable, true, true)
			}
		).orElse(
			findConstructor(LazyStringThrowableArgs).map {
				cons => (s :String, e :Throwable) => cons.newInstance(() => s, e) }
		).orElse(
			findConstructor(LazyStringArg).map {
				cons => (s :String, e :Throwable) => cons.newInstance(s).initCause(e).downcastTo[T]
			}
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(_ :String, null, _ :Throwable)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :String, null, _ :Throwable, true, true)
			}
		)
	}

	private[exceptions] final def stringThrowableBoolBoolConstructor[T <: Throwable :ClassTag]
			:Opt[(String, Throwable, Boolean, Boolean) => T] =
	{
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(StringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :String, _ :Throwable, _ :Boolean, _ :Boolean)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :String, null, _ :Throwable, _ :Boolean, _ :Boolean) }
		).orElse(
			findConstructor(LazyStringThrowableBoolBoolArgs).map {
				cons => (s :String, e :Throwable, es :Boolean, ws :Boolean) => cons.newInstance(() => s, e, es, ws)
			}
		)
	}

	private[exceptions] final def lazyStringConstructor[T :ClassTag] :Opt[(() => String) => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(LazyStringThrowableArgs).map { cons => cons.newInstance(_ :() => String, null) }
		).orElse(
			findConstructor(LazyStringArg).map { cons => cons.newInstance(_ :() => String) }
		).orElse(
			findConstructor(LazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :() => String, null, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(null, _ :() => String, null, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(null, _ :() => String, null)
			}
		)
	}

	private[exceptions] final def lazyStringThrowableConstructor[T <: Throwable :ClassTag]
			:Opt[(() => String, Throwable) => T] =
	{
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(LazyStringThrowableArgs).map { cons => cons.newInstance(_ :() => String, _ :Throwable) }
		).orElse(
			findConstructor(LazyStringArg).map {
				cons => cons.newInstance(_ :() => String).initCause(_ :Throwable).downcastTo[T]
			}
		).orElse(
			findConstructor(LazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :() => String, _ :Throwable, true, true)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(_ :() => String, null, _ :Throwable)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(null, _ :() => String, _ :Throwable, true, true)
			}
		)
	}

	private[exceptions] final def lazyStringThrowableBoolBoolConstructor[T <: Throwable :ClassTag]
			:Opt[(() => String, Throwable, Boolean, Boolean) => T] =
	{
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(LazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :() => String, _ :Throwable, _ :Boolean, _ :Boolean)
			}
		).orElse(
			findConstructor(StringLazyStringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(null, _ :() => String, _ :Throwable, _ :Boolean, _ :Boolean)
			}
		)
	}

	private[exceptions] final def defaultRethrowableConstructor[T <: Rethrowable :ClassTag] :Opt[() => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(NoArgs).map(cons => () => cons.newInstance()).filterNot(_().isRethrown)
		).orElse(
			findConstructor(StringArg).map(cons => () => cons.newInstance("")).filterNot(_().isRethrown)
		).orElse(
			findConstructor(StringThrowableBoolArgs).map {
				cons => () => cons.newInstance("", null, false)
			}.filterNot(_().isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableBoolArgs).map {
				cons => () => cons.newInstance(() => "", null, false)
			}.filterNot(_().isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => () => cons.newInstance("", null, null)
			}.filterNot(_().isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableBoolArgs).map {
				cons => () => cons.newInstance("", null, null, false)
			}.filterNot(_().isRethrown)
		)
	}

	private[exceptions] final def newRethrowableConstructor[T <: Rethrowable :ClassTag] :Opt[String => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		(
			findConstructor(StringArg).map(cons => cons.newInstance(_ :String)).filterNot(_("").isRethrown)
		).orElse(
			findConstructor(StringThrowableBoolArgs).map {
				cons => cons.newInstance(_ :String, null, false)
			}.filterNot(_("").isRethrown)
		).orElse(
			findConstructor(StringThrowableArgs).map {
				cons => cons.newInstance(_ :String, null)
			}.filterNot(_("").isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableBoolArgs).map {
				cons => cons.newInstance(_ :String, null, null, false)
			}.filterNot(_("").isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(_ :String, null, null)
			}.filterNot(_("").isRethrown)
		).orElse(
			findConstructor(LazyStringArg).map {
				cons => (s :String) => cons.newInstance(() => s)
			}.filterNot(_("").isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableBoolArgs).map {
				cons => (s :String) => cons.newInstance(() => s, null, false)
			}
		).orElse(
			findConstructor(LazyStringThrowableArgs).map {
				cons => (s :String) => cons.newInstance(() => s, null)
			}.filterNot(_("").isRethrown)
		)
	}

	private[exceptions] final def newLazyRethrowableConstructor[T <: Rethrowable :ClassTag] :Opt[(() => String) => T] = {
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		val testArg = () => ""
		(
			findConstructor(LazyStringArg).map {
				cons => cons.newInstance(_ :() => String)
			}.filterNot(_(testArg).isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableBoolArgs).map {
				cons => cons.newInstance(_ :() => String, null, false)
			}.filterNot(_(testArg).isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableArgs).map {
				cons => cons.newInstance(_ :() => String, null)
			}.filterNot(_(testArg).isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableBoolArgs).map {
				cons => cons.newInstance(null, _ :() => String, null, false)
			}.filterNot(_(testArg).isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(null, _ :() => String, null)
			}.filterNot(_(testArg).isRethrown)
		)
	}

	private[exceptions] final def rethrownRethrowableConstructor[T <: Rethrowable :ClassTag]
			:Opt[(String, Throwable) => T] =
	{
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		val testArg = new Exception()
		(
			findConstructor(StringThrowableArgs).map {
				cons => cons.newInstance(_ :String, _ :Throwable)
			}.filter(_("", testArg).isRethrown)
		).orElse(
			findConstructor(StringThrowableBoolArgs).map {
				cons => cons.newInstance(_ :String, _ :Throwable, true)
			}.filter(_("", testArg).isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableBoolArgs).map {
				cons => cons.newInstance(_ :String, null, _ :Throwable, true)
			}.filter(_("", testArg).isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(_ :String, null, _ :Throwable)
			}.filter(_("", testArg).isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableArgs).map {
				cons => (s :String, e :Throwable) => cons.newInstance(() => s, e)
			}.filter(_("", testArg).isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableBoolArgs).map {
				cons => (s :String, e :Throwable) => cons.newInstance(() => s, e, true)
			}.filter(_("", testArg).isRethrown)
		)
	}

	private[exceptions] final def rethrownLazyRethrowableConstructor[T <: Rethrowable :ClassTag]
			:Opt[(() => String, Throwable) => T] =
	{
		implicit val constructors :Array[Constructor[T]] =
			classTag[T].runtimeClass.getDeclaredConstructors.castParam[Constructor[T]]
		val testArg1 = () => ""
		val testArg2 = new Exception()
		(
			findConstructor(LazyStringThrowableArgs).map {
				cons => cons.newInstance(_ :() => String, _ :Throwable)
			}.filter(_(testArg1, testArg2).isRethrown)
		).orElse(
			findConstructor(LazyStringThrowableBoolArgs).map {
				cons => cons.newInstance(_ :() => String, _ :Throwable, true)
			}.filter(_(testArg1, testArg2).isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableBoolArgs).map {
				cons => cons.newInstance(null, _ :() => String, _ :Throwable, true)
			}.filter(_(testArg1, testArg2).isRethrown)
		).orElse(
			findConstructor(StringLazyStringThrowableArgs).map {
				cons => cons.newInstance(null, _ :() => String, _ :Throwable)
			}.filter(_(testArg1, testArg2).isRethrown)
		)
	}


	private[exceptions] final def findConstructor[T](paramTypes :Array[Class[_]])
	                                                (implicit constructors :Array[Constructor[T]]) =
	{
		var i = 0
		val count = constructors.length
		while (i < count && !arraysEqual(constructors(i).getParameterTypes, paramTypes))
			i += 1
		if (i == count) Lack else Got(constructors(i))
/*
		var constructor :Constructor[T] = null
		var found = false
		while (i < count && !found) {
			constructor = constructors(i)
			val params = constructor.getParameterTypes
			found = params.length == paramTypes.length && {
				var i = params.length
				while (i > 0 && { i -= 1; params(i).isAssignableFrom(paramTypes(i)) })
					{}
				i == 0
			}
		}
		if (found) Got(constructor) else Lack
*/
	}


	//todo: create instead methods for each argument list, which search for any constructor which can be used -
	// much faster.
	private[exceptions] final def findConstructor[T](clazz :Class[T], paramTypes :Array[Class[_]]) :Opt[Constructor[T]] =
		findConstructor(paramTypes)(clazz.getDeclaredConstructors.castParam[Constructor[T]])
/*
	private[exceptions] final def findConstructor(clazz :Class[_], paramTypes :Array[Class[_]]) :Opt[Constructor[_]] = {
		val constructors = clazz.getDeclaredConstructors
		var i = constructors.length - 1
		while (i >= 0 && !arraysEqual(constructors(i).getParameterTypes, paramTypes))
			i -= 1
		if (i >= 0) Got(constructors(i))
		else Lack
	}
*/
	private final def arraysEqual(a :Array[_], b :Array[_]) :Boolean =
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


