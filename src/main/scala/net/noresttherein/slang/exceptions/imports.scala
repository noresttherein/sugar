package net.noresttherein.slang.exceptions

import scala.reflect.{classTag, ClassTag}

import net.noresttherein.slang.vars.Opt
import net.noresttherein.slang.extensions.{classExtension, downcastTypeParam}




/** Definitions of various methods throwing or creating exceptions. Extended by
  * [[net.noresttherein.slang.exceptions exceptions]] package object and (indirectly,
  * through [[net.noresttherein.slang.imports slang.imports]] by package object [[net.noresttherein.slang slang]].
  */
trait imports {
	/* WHENEVER YOU EDIT THIS FILE UPDATE rethrowStackTraceElement WITH A CORRECT LINE NUMBER! */

	/** Executes `action` expression, catching exceptions thrown by it, and rethrows them with an additional
	  * error message. This allows code lower on the call stack to provide additional context about the situation
	  * which lead to the error, such as values of parameters, which is typically not present at the place
	  * where the error is discovered and the original exception is thrown. The rethrown exception will be
	  * of the same class as the one caught in order to allow calling code to handle it. It will either be
	  * a new instance with the caught exception as its cause and the provided error message, or the same one
	  * with the error message added as a [[Throwable.addSuppressed suppressed]] error.
	  *
	  * The `catch` clause is defined by partial function
	  * [[net.noresttherein.slang.exceptions.pushErrorMessage pushErrorMessage]], which returns the exception
	  * to be thrown in place of the caught one. The implementation provided in
	  * [[net.noresttherein.slang.exceptions.imports exceptions.imports]] is defined at any `Throwable`
	  * (i.e. it is in a effect a normal function).
	  *   1. If the caught [[Throwable]] is a [[StackableThrowable StackableThrowable]],
	  *      its [[net.noresttherein.slang.exceptions.StackableThrowable.addInfo addInfo]] method,
	  *      introduced for this purpose, is used to create the rethrown exception.
	  *      A [[net.noresttherein.slang.exceptions.Rethrowable Rethrowable]] will attempt to create
	  *      a new instance with non-writeable stack trace and initialize it instead from the stack trace
	  *      of the caught exception, dropping top frames following the call to this method.
	  *   1. If the class of the `Throwable` equals one of the predefined entries in
	  *      [[net.noresttherein.slang.exceptions.imports.ThrowableCloning, ThrowableCloning]], the function
	  *      associated with it is used as the constructor of the new exception.
	  *   1. If the `Throwable` is of an unknown class, an attempt is made to create a new instance by reflection:
	  *      if the class defines at least one constructor with the standard `Throwable` signature, i.e.,
	  *      `(String)`, `(String, Throwable)`, `(String, Throwable, Boolean, Boolean)`, it will be invoked
	  *      by reflection.
	  *   1. If the above invocation fails with an exception (or no appropriate public constructor is found),
	  *      the method will resort to adding an artificial [[Throwable.addSuppressed suppressed]] `Throwable`
	  *      with the message given here.
	  *
	  * In cases 1), 2) and 3), the constructor is given as the arguments pair `(errorMessage, cause)`, where `cause`
	  * is the caught exception. The above policy can be changed (for application code) by extending
	  * `slang.exceptions.imports` and overriding `pushErrorMessage`. Depending on performance requirements,
	  * the applications may choose to skip the reflective call, or short-circuit that method to always
	  * add a suppressed [[net.noresttherein.slang.exceptions.RethrowContext RethrowContext]] (or any other `Throwable`)
	  * with the error message. This may be particularly worthwhile
	  *
	  * This heuristic does not guarantee that the provided error message will be indeed included in the exception:
	  * exceptions from external libraries without accessible one of the standard constructors and disabled suppression
	  * will be rethrown without change. This should not be however a common occurrence.
	  *
	  * (all exceptions with type aliases/classes in the `scala` package),
	  * with the exception of [[Exception]], [[RuntimeException]], [[Error]], [[Throwable]]
	  * and rethrowing them with an additional error message. The rethrown exception will be a new instance
	  * of the caught class, with `errorMessage` as its `message` and the original exception as its `cause`.
	  * Note that any subclasses of these exceptions will be caught together with their base class,
	  * and the rethrown exception thus can be more general than the original.
	  * Also note that, contrary to the convention, this method will catch
	  * [[AbstractMethodError]] and [[AssertionError]].
	  *
	  * This method is useful for adding context information about the executed action which, most likely,
	  * is unavailable in location where the exception is thrown, without changing the exception thrown,
	  * in case code higher up the call stack wishes to catch.
	  *
	  *   1. If the caught exception is a [[net.noresttherein.slang.exceptions.StackableThrowable StackableThrowable]],
	  *      its [[StackableThrowable.addInfo addInfo]] is used to create its copy
	  *      with the given message and the original exception as its cause.
	  *   1. Otherwise, if the exception is one of standard Java/Scala exceptions, then a new instance of that class
	  *      is created and rethrown statically (handling all subclasses of the given exception at the same time).
	  *   1. If the class of the caught exception defines either a `(String)`, `(String, Throwable)`
	  *      or `(String, Throwable, Boolean, Boolean)` constructor, then a new instance is created through reflection.
	  *   1. Finally, if the previous point fails with an exception,
	  *      a [[net.noresttherein.slang.exceptions.RethrowContext RethrownContext]] with the error message
	  *      is added as a suppressed exception.
	  */
	final def rethrow[T](action: => T)(errorMessage: => String) :T =
		try { action } catch pushErrorMessage(errorMessage).andThen(throw _)

	/** This method is never called. It is used as an artificial top stack trace element
	  * of cheaper [[net.noresttherein.slang.exceptions.Rethrowable Rethrowable]] exceptions created and thrown
	  * by method [[net.noresttherein.slang.imports.rethrow rethrow]], solely to provide this information
	  * here. These exceptions do not have their stack trace filled by the virtual machine, but initialize it instead
	  * with frames of the [[Throwable]] caught by `rethrow`, leading up to the call of `rethrow` itself.
	  * In order to minimize confusion coming from a stack trace leading to code which throws no exception,
	  * a final frame for this method is added to point programmers to this documentation.
	  */ //remember to update the line number if you edit these docs!
	private def conjureThrowable :Nothing = ??!

	private[exceptions] final val rethrowStackTraceElement =
		new StackTraceElement(classOf[imports].getName, "rethrow", "imports.scala", 84)

	private[exceptions] final val conjureThrowableStackTraceElement =
		new StackTraceElement(classOf[imports].getName, "conjureThrowable", "imports.scala", 97)

	private[exceptions] final val fillInStackTraceStackTraceElement =
		new StackTraceElement(classOf[Rethrowable].getName, "fillInStackTrace", "AbstractThrowable.scala", -1)

	/** A partial function attempting to create a `Throwable` of the same type as the argument,
	  * with `msg` as its message and the argument `Throwable` as its cause.
	  *   1. If the argument is a [[net.noresttherein.slang.exceptions.StackableThrowable StackableThrowable]],
	  *      it delegates to its [[net.noresttherein.slang.exceptions.StackableThrowable.addInfo addInfo]] method.
	  *   1. Otherwise, if it is one of the standard, common Scala/Java exceptions, a new instance of its class
	  *      is created explicitly.
	  *   1. Otherwise, reflection is used to find a `(String, Throwable)` or a `(String)` constructor in the
	  *      class of the throwable argument, which is invoked through reflection.
	  *   1. If the above fails, the cause of the failure, containing `msg`, is added
	  *      as a [[Throwable.addSuppressed suppressed]] exception to the argument, which is returned instead
	  *      of a new instance.
	  */
	def pushErrorMessage(msg: => String) :PartialFunction[Throwable, Throwable] = {
		case e :StackableThrowable => e.addInfo(msg)
		case e => ThrowableCloning.getOrElse(e.getClass, cloneThrowable _)(msg, e)
		//		case e if e.getClass == classOf[NumberFormatException]           =>
		//			new NumberFormatException(msg).initCause(e)
		//		case e if e.getClass == classOf[IllegalArgumentException]        =>
		//			new IllegalArgumentException(msg, e)
		//		case e if e.getClass == classOf[UnsupportedOperationException]   =>
		//			new UnsupportedOperationException(msg, e)
		//		case e if e.getClass == classOf[NoSuchElementException]          =>
		//			new NoSuchElementException(msg).initCause(e)
		//		case e if e.getClass == classOf[IllegalStateException]           =>
		//			new IllegalStateException(msg, e)
		//		case e if e.getClass == classOf[ArrayIndexOutOfBoundsException]  =>
		//			new ArrayIndexOutOfBoundsException(msg).initCause(e)
		//		case e if e.getClass == classOf[StringIndexOutOfBoundsException] =>
		//			new StringIndexOutOfBoundsException(msg).initCause(e)
		//		case e if e.getClass == classOf[IndexOutOfBoundsException]       =>
		//			new IndexOutOfBoundsException(msg).initCause(e)
		//		case e if e.getClass == classOf[ClassCastException]              =>
		//			new ClassCastException(msg).initCause(e)
		//		case e if e.getClass == classOf[NullPointerException]            =>
		//			new NullPointerException(msg).initCause(e)
		//		case e if e.getClass == classOf[InterruptedException]            =>
		//			new InterruptedException(msg).initCause(e)
		//		case e if e.getClass == classOf[AbstractMethodError]             =>
		//			new AbstractMethodError(msg).initCause(e)
		//		case e if e.getClass == classOf[AssertionError]                  =>
		//			new AssertionError(msg, e)
		//		case e => try {
		//			newThrowable[Throwable](msg, e)(ClassTag(e.getClass))
		//		} catch {
		//			case e1 :Exception => e.addSuppressed(e1); e
		//		}
	}

	/** An alternative implementation of [[net.noresttherein.slang.imports.pushErrorMessage pushErrorMessage]]
	  * which restricts itself simply to adding a suppressed throwable with the message given as the argument
	  * to this method. May be delegated to by overriden `pushErrorMessage` by more performance conscious code.
	  * Note that this method will be a no-op if the caught exception disabled suppression.
	  */
	def suppressErrorMessage(msg: => String) :PartialFunction[Throwable, Throwable] = {
		e => e.addSuppressed(new RethrowContext(msg)); e
	}

	private def addRethrowContext(msg :String, cause :Throwable) = {
		cause.addSuppressed(new RethrowContext(msg, cause)); cause
	}
	private def cloneThrowable(msg :String, cause :Throwable) =
		try {
			newThrowable[Throwable](msg, cause)(ClassTag(cause.getClass))
		} catch {
			case _ :Exception => addRethrowContext(msg, cause)
		}


	protected val ThrowableCloning :Map[Class[_], (String, Throwable) => Throwable] = Map(
		(classOf[ArrayIndexOutOfBoundsException],  new ArrayIndexOutOfBoundsException(_).initCause(_)),
		(classOf[ArrayStoreException],             new ArrayStoreException(_).initCause(_)),
		(classOf[ClassCastException],              new ClassCastException(_).initCause(_)),
		(classOf[ClassNotFoundException],          new ClassNotFoundException(_, _)),
		(classOf[CloneNotSupportedException],      new CloneNotSupportedException(_).initCause(_)),
		(classOf[EnumConstantNotPresentException], addRethrowContext),
		(classOf[IllegalAccessException],          new IllegalAccessException(_).initCause(_)),
		(classOf[IllegalArgumentException],        new IllegalArgumentException(_, _)),
		(classOf[IllegalCallerException],          new IllegalCallerException(_, _)),
		(classOf[IllegalMonitorStateException],    new IllegalMonitorStateException(_).initCause(_)),
		(classOf[IllegalStateException],           new IllegalStateException(_, _)),
		(classOf[IllegalThreadStateException],     new IllegalThreadStateException(_).initCause(_)),
		(classOf[IndexOutOfBoundsException],       new IndexOutOfBoundsException(_).initCause(_)),
		(classOf[InstantiationException],          new InstantiationException(_).initCause(_)),
		(classOf[InterruptedException],            new InterruptedException(_).initCause(_)),
		(classOf[LayerInstantiationException],     new LayerInstantiationException(_, _)),
		(classOf[NoSuchElementException],          new NoSuchElementException(_).initCause(_)),
		(classOf[NegativeArraySizeException],      new NegativeArraySizeException(_).initCause(_)),
		(classOf[NoSuchFieldException],            new NoSuchFieldException(_).initCause(_)),
		(classOf[NoSuchMethodException],           new NoSuchMethodException(_).initCause(_)),
		(classOf[NullPointerException],            new NullPointerException(_).initCause(_)),
		(classOf[NumberFormatException],           new NumberFormatException(_).initCause(_)),
		(classOf[ReflectiveOperationException],    new ReflectiveOperationException(_, _)),
		(classOf[RuntimeException],                new RuntimeException(_, _)),
		(classOf[SecurityException],               new SecurityException(_, _)),
		(classOf[StringIndexOutOfBoundsException], new StringIndexOutOfBoundsException(_).initCause(_)),
		(classOf[TypeNotPresentException],         new TypeNotPresentException(_, _)),
		(classOf[UnsupportedOperationException],   new UnsupportedOperationException(_, _)),

		(classOf[AssertionError],                  new AssertionError(_, _)),
		(classOf[ExceptionInInitializerError],     new ExceptionInInitializerError(_).initCause(_)),

		(classOf[SlangException],                  new SlangException(_, _)),
		(classOf[StackableException],              new StackableException(_, _)),
		(classOf[RethrowableException],            new RethrowableException(_, _, true))
	)



	/** A 'WTF' method throwing an [[ImpossibleError ImpossibleError]].
	  * Intended for code which should, to the best of the programmer's - but not compiler's - knowledge, be unreachable.
	  * Placed after infinite loops, as the body of methods which are never called (but, for example, remain
	  * for binary compatibility), or methods of sealed classes which are overriden by subclasses and similar
	  * circumstances. Better than `???` for this purpose as it makes clear that no code is actually missing
	  * and does not trigger any lint warnings.
	  */
	def ??! :Nothing = throw new ImpossibleError

	/** Throws a programming error with the given message, to indicate a situation which should not have happened
	  * and indicates a bug in the executed code.
	  */
	def oops(msg :String) :Nothing = throw new ProgrammingError(msg)

	/** Throws a programming error with the given message, to indicate a situation which should not have happened
	  * and indicates a bug in the executed code.
	  */
	def oops(msg :String, cause :Throwable) :Nothing = throw new ProgrammingError(msg)

	/** Throws an [[UnsupportedOperationException]]. */
	@inline final def unsupported_! :Nothing =
		throw new UnsupportedOperationException

	/** Throws an [[UnsupportedOperationException]]. */
	@inline final def unsupported_!(msg :String) :Nothing =
		throw new UnsupportedOperationException(msg)

	/** Throws a [[NoSuchElementException]]. */
	@inline final def noSuch_!(msg :String) :Nothing =
		throw new NoSuchElementException(msg)

	/** Throws an [[IllegalArgumentException]]. */
	@inline final def illegal_!(msg :String) :Nothing =
		throw new IllegalArgumentException(msg)

	/** Throws an [[ImpossibleError]].  */
	@inline final def impossible_!(msg :String) :Nothing = throw new ImpossibleError(msg)




	/** Creates a new instance of the [[Throwable]] class specified as the type parameter by using reflection.
	  * This method attempts to find constructors `()`, `(String)`, `(String, Throwable)`
	  * or `(String, Throwable, Boolean, Boolean)` and provides the same arguments that `Throwable`'s constructor would.
	  */
	private[slang] def getThrowable[E <: Throwable :ClassTag] :Opt[E] = {
		val E = classTag[E].runtimeClass
		Opt(
			try {
				E.getDeclaredConstructor().newInstance()
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String]).newInstance("")
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable]).newInstance("", null)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
					.newInstance("", null, true, true)
			} catch { case _ :Throwable => null }
		).downcastParam[E]
	}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * by using reflection. This method attempts to find constructors `(String)`, `(String, Throwable)`
	  * or `(String, Throwable, Boolean, Boolean)` and provides as arguments `msg` and default values
	  * as in `Throwable`'s constructors.
	  */
	private[slang] def getThrowable[E <: Throwable :ClassTag](msg :String) :Opt[E] = {
		val E = classTag[E].runtimeClass
		Opt(
			try {
				E.getDeclaredConstructor(classOf[String]).newInstance(msg)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			 try {
				 E.getDeclaredConstructor(classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
					 .newInstance(msg, null, true, true)
			 } catch { case _ :Throwable => null }
		).downcastParam[E]
	}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * and cause by using reflection. This method attempts to find constructors `(String)`, `(String, Throwable)`
	  * or `(String, Throwable, Boolean, Boolean)` and provides as arguments `msg`, `cause` and, optionally, `true`
	  * for the `Boolean` parameters presumed to be `enableSuppression` and `writeableStackTrace`.
	  */
	private[slang] def getThrowable[E <: Throwable :ClassTag](msg :String, cause :Throwable) :Opt[E] = {
		val E = classTag[E].runtimeClass
		Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable]).newInstance(msg, cause)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String]).newInstance(msg).asInstanceOf[E].initCause(cause)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
					.newInstance(msg, cause, true, true)
			} catch { case _ :Throwable => null }
		).downcastParam[E]
	}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given cause
	  * by using reflection. This method attempts to find constructors `(Throwable)`, `()`, `(String, Throwable)`
	  * or `(String, Throwable, Boolean, Boolean)` and provides as arguments an empty message (if required),
	  * the given cause and, optionally, `true` for the `Boolean` parameters presumed to be the standard flags
	  * `enableSuppression` and `writeableStackTrace`.
	  */
	private[slang] def getThrowable[E <: Throwable :ClassTag](cause :Throwable) :Opt[E] = {
		val E = classTag[E].runtimeClass
		Opt(
			try {
				E.getDeclaredConstructor(classOf[Throwable]).newInstance(cause)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable]).newInstance("", cause)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor().newInstance().asInstanceOf[E].initCause(cause)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String]).newInstance("").asInstanceOf[E].initCause(cause)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
					.newInstance("", cause, true, true)
			} catch { case _ :Throwable => null }
		).downcastParam[E]
	}

	/** Creates a new instance of the [[net.noresttherein.slang.exceptions.Rethrowable]] class
	  * specified as the type parameter with, the given message and cause,
	  * by using reflection. This method attempts to find and use constructors with the following signatures:
	  *   1. `(String, Throwable, Boolean)`, applied to `(msg, cause, true)` - the last parameter is assumed
	  *      to be the [[net.noresttherein.slang.exceptions.Rethrowable.isRethrown isRethrown]] flag.
	  *   1. `(String, Throwable, Boolean, Boolean)`, applied to `(msg, cause, true, false)` -
	  *      the last parameters are presumed to be flags `enableSuppression`  and `writeableStackTrace`.
	  *   1. If none of the specialized constructors is available, the default `(String, Throwable)` is checked;
	  *   1. Finally, the method looks for the simplest `(String)` constructor and uses
	  *      [[Throwable.initCause initCause]] to set the cause.
	  *
	  * In case of the first two constructors, the resulting exception should have non-writeable stack trace;
	  * instead, it will be initialized with the suffix of `cause`'s stack trace starting with most recent call to
	  * [[net.noresttherein.slang.exceptions.imports imports]]`.`[[net.noresttherein.slang.exceptions.imports.rethrow rethrow]].
	  *
	  * This method differs from the `getThrowable` methods in that it specifically looks to disable automatic filling
	  * of the stacktrace. It is used (indirectly) by `rethrow`, attempting to reduce the cost of throwing and handling
	  * the exception.
	  */
	private[slang] def getRethrowable[E <: Rethrowable :ClassTag](msg :String, cause :E) :Opt[E] = {
		val E = classTag[E].runtimeClass;
		Opt(
			try {
				val instance = E.getDeclaredConstructor(classOf[String], classOf[Throwable], classOf[Boolean])
					.newInstance(msg, cause, true).asInstanceOf[E]
				if (instance.isRethrown) instance else null.asInstanceOf[E]
			} catch { case _ :Throwable => null.asInstanceOf[E] }
		) orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable], classOf[Boolean], classOf[Boolean])
					.newInstance(msg, cause, true, false)
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String], classOf[Throwable])
					.newInstance(msg, cause).asInstanceOf[E]
			} catch { case _ :Throwable => null }
		).downcastParam[E] orElse Opt(
			try {
				E.getDeclaredConstructor(classOf[String]).newInstance(msg).asInstanceOf[E].initCause(cause)
			}catch { case _ :Throwable => null }
		).downcastParam[E]
	}


	/** Creates a new exception of the class specified as the type parameter through reflection. */
	@throws[IllegalArgumentException]("if the class specifies none of (), (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[slang] def newThrowable[E <: Throwable :ClassTag] :E =
		getThrowable[E] orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + ": " +
			"no (), (String), (String, Throwable) or (String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing `msg` as the exception message argument.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	def newThrowable[E <: Throwable :ClassTag](msg :String) :E =
		getThrowable[E](msg) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + "): " +
			"no (String), (String, Throwable) or (String, Throwable, Boolean Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given error message and cause as the arguments to the constructor.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[slang] def newThrowable[E <: Throwable :ClassTag](msg :String, cause :Throwable) :E =
		getThrowable[E](msg, cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + ", " + cause + "): " +
			"no (String, Throwable), (String), or (String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given [[Throwable]] as the cause to the constructor.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (), (String), (String, Throwable) " +
	                                  "or (String, Throwable, Boolean, Boolean) constructors.")
	private[slang] def newThrowable[E <: Throwable :ClassTag](cause :Throwable) :E =
		getThrowable[E](cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + cause + "): " +
			"no (Throwable), (String, Throwable), (), (String) or (String, Throwable, Boolean Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given message and cause as the constructor arguments. If possible,
	  * the exception will be created without automatic filling of the stack trace and instead initialize it
	  * by truncating the top frames of the `cause`'s stack trace, following a call to
	  * [[net.noresttherein.slang.exceptions.imports.rethrow rethrow]].
	  */
	@throws[IllegalArgumentException]("if the class specifies neither of (), (String), (String, Throwable) " +
	                                  "or (String, Throwable, Boolean, Boolean) constructors.")
	private[slang] def newRethrowable[E <: Rethrowable :ClassTag](msg :String, cause :E) :E =
		getRethrowable[E](msg, cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + ", " + cause + "): " +
			"no (String, Throwable, Boolean), (String, Throwable, Boolean, Boolean) or (String, Throwable) constructor."
		}


	private[slang] final def raise[E <: Throwable :ClassTag] :Nothing =
		throw newThrowable[E]

	private[slang] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw newThrowable[E](msg)

}
