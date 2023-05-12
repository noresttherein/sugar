package net.noresttherein.sugar.exceptions

import java.lang.reflect.Constructor

import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.extensions.{downcastTypeParamMethodsd, castingMethods, castTypeParamMethods, ClassExtension}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** Definitions of various methods throwing or creating exceptions. Extended by
  * [[net.noresttherein.sugar.exceptions exceptions]] package object and (indirectly,
  * through [[net.noresttherein.sugar.imports sugar.imports]] by package object [[net.noresttherein.sugar sugar]].
  */
trait imports {

	/** Executes `action` expression, catching exceptions thrown by it, and rethrows them with an additional
	  * error message. This allows code lower on the call stack to provide additional context about the situation
	  * which lead to the error, such as values of parameters, which is typically not present at the place
	  * where the error is discovered and the original exception is thrown. The rethrown exception will be
	  * of the same class as the one caught in order to allow calling code to handle it. It will either be
	  * a new instance with the caught exception as its cause and the provided error message, or the same one
	  * with the error message added as a [[Throwable.addSuppressed suppressed]] error.
	  *
	  * The `catch` clause is defined by partial function
	  * [[net.noresttherein.sugar.exceptions.pushErrorMessage pushErrorMessage]], which returns the exception
	  * to be thrown in place of the caught one. The implementation provided in
	  * [[net.noresttherein.sugar.exceptions.imports exceptions.imports]] is defined at any `Throwable`
	  * (i.e. it is in a effect a normal function).
	  *   1. If the caught [[Throwable]] is a [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]],
	  *      its [[net.noresttherein.sugar.exceptions.SugaredThrowable.addInfo addInfo]] method,
	  *      introduced for this purpose, is used to create the rethrown exception.
	  *      A [[net.noresttherein.sugar.exceptions.Rethrowable Rethrowable]] will attempt to create
	  *      a new instance with non-writeable stack trace and initialize it instead from the stack trace
	  *      of the caught exception, dropping top frames following the call to this method.
	  *   1. If the class of the `Throwable` equals one of the predefined entries in
	  *      [[net.noresttherein.sugar.exceptions.imports.ThrowableCloning, ThrowableCloning]], the function
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
	  * `sugar.exceptions.imports` and overriding `pushErrorMessage`. Depending on performance requirements,
	  * the applications may choose to skip the reflective call, or short-circuit that method to always
	  * add a suppressed [[net.noresttherein.sugar.exceptions.RethrowContext RethrowContext]] (or any other `Throwable`)
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
	  *   1. If the caught exception is a [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]],
	  *      its [[SugaredThrowable.addInfo addInfo]] is used to create its copy
	  *      with the given message and the original exception as its cause.
	  *   1. Otherwise, if the exception is one of standard Java/Scala exceptions, then a new instance of that class
	  *      is created and rethrown statically (handling all subclasses of the given exception at the same time).
	  *   1. If the class of the caught exception defines either a `(String)`, `(String, Throwable)`
	  *      or `(String, Throwable, Boolean, Boolean)` constructor, then a new instance is created through reflection.
	  *   1. Finally, if the previous point fails with an exception,
	  *      a [[net.noresttherein.sugar.exceptions.RethrowContext RethrownContext]] with the error message
	  *      is added as a suppressed exception.
	  */
	final def rethrow[T](action: => T)(errorMessage: => String) :T =
		try { eval(action) } catch pushErrorMessage(errorMessage).andThen(throw _)

	/** A variant of [[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]] which guards only against one
	  * exception type, specified as the type parameter. The returned object defines method
	  * [[net.noresttherein.sugar.exceptions.RethrowGuard.apply apply]] with the same signature as `rethrow`.
	  * An expression:
	  * {{{
	  *     guard[ArithmeticException](1 / 0)("oh no!")
	  * }}}
	  * is equivalent to:
	  * {{{
	  *     try { 1 / 0 } catch {
	  *         case e :ArithmeticException => throw new ArithmeticException("oh no!", e)
	  *     }
	  * }}}
	  * The rethrown exception is created using [[net.noresttherein.sugar.exceptions.pushErrorMessage pushErrorMessage]]
	  * as in `rethrow`, defaulting to rethrowing the same exception and adding the error message
	  * as a ''suppressed'' exception (providing suppression is not disabled).
	  */
	@inline final def guard[E <: Throwable] = new RethrowGuard[E](this)


	/** A partial function attempting to create a `Throwable` of the same type as the argument,
	  * with `msg` as its message and the argument `Throwable` as its cause.
	  *   1. If the argument is a [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]],
	  *      it delegates to its [[net.noresttherein.sugar.exceptions.SugaredThrowable.addInfo addInfo]] method.
	  *   1. Otherwise, if it is one of the standard, common Scala/Java exceptions, a new instance of its class
	  *      is created explicitly.
	  *   1. Otherwise, if exception suppression is not disabled,
	  *      a new [[net.noresttherein.sugar.exceptions.RethrowContext RethrowContext]] is added to the list
	  *      of suppressed exception and the original cause is returned
	  *   1. Otherwise, reflection is used to find a `(String, Throwable)` or a `(String)` constructor in the
	  *      class of the throwable argument, which is invoked through reflection.
	  * For other arguments the result is undefined.
	  */
	def pushErrorMessage(msg: => String) :PartialFunction[Throwable, Throwable] = {
		case e :SugaredThrowable => e.addInfo(msg)
		case e @ CloneableException(clone) => clone(msg, e)
		case e if { e.addSuppressed(new RethrowContext(msg)); e.getSuppressed.length != 0 } => e
		case e if findConstructor(e.getClass, LazyStringThrowableArgs).isDefined =>
			e.getClass.getDeclaredConstructor(LazyStringThrowableArgs :_*).newInstance(() => msg, e)
		case e if findConstructor(e.getClass, LazyStringArg).isDefined =>
			e.getClass.getDeclaredConstructor(LazyStringArg :_*).newInstance(() => msg).initCause(e)
		case e if findConstructor(e.getClass, LazyStringThrowableBoolBoolArgs).isDefined =>
			e.getClass.getDeclaredConstructor(LazyStringThrowableBoolBoolArgs :_*).newInstance(() => msg, e, true, true)
		case e if findConstructor(e.getClass, StringThrowableArgs).isDefined =>
			e.getClass.getDeclaredConstructor(StringThrowableArgs :_*).newInstance(msg, e)
		case e if findConstructor(e.getClass, StringArg).isDefined =>
			e.getClass.getDeclaredConstructor(StringArg :_*).newInstance(msg, e).initCause(e)
		case e if findConstructor(e.getClass, StringThrowableBoolBoolArgs).isDefined =>
			e.getClass.getDeclaredConstructor(StringThrowableBoolBoolArgs :_*).newInstance(msg, e, true, true)
		case e if findConstructor(e.getClass, StringLazyStringThrowableBoolBoolArgs).isDefined =>
			e.getClass.getDeclaredConstructor(StringLazyStringThrowableBoolBoolArgs :_*)
			          .newInstance(null, () => msg, e, true, true)
	}

	/** An alternative implementation of [[net.noresttherein.sugar.imports.pushErrorMessage pushErrorMessage]]
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


	private val CloneableException = { a :Any => ThrowableCloning.get(a.getClass) }.unlift

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
		(classOf[StackOverflowError],              new StackOverflowError(_).initCause(_)),
		(classOf[ExceptionInInitializerError],     new ExceptionInInitializerError(_).initCause(_)),

		(classOf[RethrowableException],            new RethrowableException(_, _, true)),
		(classOf[SugaredException],                (msg, exception) => exception.asInstanceOf[SugaredException].addInfo(msg))
	)



//	def rethrowWith[E <: Throwable :ClassTag] = new RethrowWith[E](implicitly[ClassTag[E]])
//
//	class RethrowWith[E <: Throwable](private val tag :ClassTag[E]) extends AnyVal {
//		def apply[T](block: => T) :T =
//			try { block } catch {
//				case e :Exception => //instrument an implementation of e.getClass with tag.runtimeClass
//			}
//	}



	/** A 'WTF' method throwing an [[ImpossibleError ImpossibleError]].
	  * Intended for code which should, to the best of the programmer's - but not compiler's - knowledge, be unreachable.
	  * Placed after infinite loops, as the body of methods which are never called (but, for example, remain
	  * for binary compatibility), or methods of sealed classes which are overriden by subclasses and similar
	  * circumstances.
	  */
	def ??! :Nothing = throw new ImpossibleError

	/** Throws an [[java.lang.AssertionError AssertionError]]. */
	def !!! :Nothing = throw new AssertionError

	/** Throws an [[java.lang.AssertionError AssertionError]] with the given message. */
	def !!!(msg :String) :Nothing = throw new AssertionError(msg)

	/** Throws an [[net.noresttherein.sugar.exceptions.Oops Oops]] with the given message,
	  * to indicate a situation which should not have happened and is due to a bug in the executed code.
	  */
	def oops(msg :String) :Nothing = throw new Oops(msg)

	/** Throws an [[net.noresttherein.sugar.exceptions.Oops Oops]] with the given message,
	  * to indicate a situation which should not have happened and is due to a bug in the executed code.
	  */
	def oops(msg :String, cause :Throwable) :Nothing = throw new Oops(msg, cause)

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

	/** Throws an [[net.noresttherein.sugar.exceptions.ImpossibleError ImpossibleError]].  */
	@inline final def impossible_!(msg :String) :Nothing = throw new ImpossibleError(msg)

	/** Throws an [[IndexOutOfBoundsException]]. */
	@inline final def outOfBounds_!(idx :Int, size :Int) :Nothing =
		throw new IndexOutOfBoundsException(idx.toString + " out of " + size)

	/** Throws an [[IndexOutOfBoundsException]]. */
	@inline final def outOfBounds(msg :String) :Nothing =
		throw new IndexOutOfBoundsException(msg)



	/** Creates a new instance of the [[Throwable]] class specified as the type parameter by using reflection.
	  * This method attempts to find constructors `()`, `(String)`, `(String, Throwable)`,
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides the same arguments that `Throwable`'s default constructor would.
	  */
	private[sugar] final def getThrowable[E <: Throwable :ClassTag] :Opt[E] =
		defaultConstructor[E].map(_())

/*
	private[sugar] final def getThrowable[E <: Throwable :ClassTag] :Opt[E] = {
		val E = classTag[E].runtimeClass
		(findConstructor(E, NoArgs) match {
			case Got(cons) => Got(cons.newInstance())
			case _ => Lack
		}).orElse(findConstructor(E, StringArg) match {
			case Got(cons) => Got(cons.newInstance(""))
			case _ => Lack
		}).orElse(findConstructor(E, ThrowableArg) match {
			case Got(cons) => Got(cons.newInstance(null))
			case _ => Lack
		}).orElse(findConstructor(E, StringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance("", null))
			case _ => Lack
		}).orElse(findConstructor(E, StringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance("", null, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance("", null, null, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringArg) match {
			case Got(cons) => Got(cons.newInstance(() => ""))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(() => "", null))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(() => "", null, true, true))
			case _ => Lack
		}).downcastParam[E]
	}
*/

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * by using reflection. This method attempts to find constructors `(String)`, `(String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides as an argument `msg` and same arguments that `Throwable`'s default constructor would.
	  */
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :String) :Opt[E] =
		stringConstructor[E].map(_(msg))
/*
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :String) :Opt[E] = {
		val E = classTag[E].runtimeClass
		(findConstructor(E, StringArg) match {
			case Got(cons) => Got(cons.newInstance(msg))
			case _ => Lack
		}).orElse(findConstructor(E, StringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, null))
			case _ => Lack
		}).orElse(findConstructor(E, StringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, null, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, null, null, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringArg) match {
			case Got(cons) => Got(cons.newInstance(() => msg))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(() => msg, null))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(() => msg, null, true, true))
			case _ => Lack
		}).downcastParam[E]
	}
*/

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * by using reflection. This method attempts to find constructors `(() => String)`, `(() => String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, or `(String, () => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides as an argument `msg` and same arguments that `Throwable`'s default constructor would.
	  */
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :() => String) :Opt[E] =
		lazyStringConstructor[E].map(_(msg))
/*
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :() => String) :Opt[E] = {
		val E = classTag[E].runtimeClass
		(findConstructor(E, LazyStringArg) match {
			case Got(cons) => Got(cons.newInstance(msg))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, null))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, null, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(null, msg, null, true, true))
			case _ => Lack
		}).downcastParam[E]
	}
*/

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * and cause by using reflection. This method attempts to find constructors `(String)`, `(String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides as arguments `msg`, `cause`, and, optionally, `true` for the `Boolean` parameters
	  * presumed to be `enableSuppression` and `writeableStackTrace`.
	  */
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :String, cause :Throwable) :Opt[E] =
		stringThrowableConstructor[E].map(_(msg, cause))
/*
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :String, cause :Throwable) :Opt[E] = {
		val E = classTag[E].runtimeClass
		(findConstructor(E, StringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(E, StringArg) match {
			case Got(cons) => Got(cons.newInstance(msg).asInstanceOf[E].initCause(cause))
			case _ => Lack
		}).orElse(findConstructor(E, StringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, null, cause, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringArg) match {
			case Got(cons) => Got(cons.newInstance(() => msg).asInstanceOf[E].initCause(cause))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(() => msg, cause))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(() => msg, cause, true, true))
			case _ => Lack
		}).downcastParam[E]
	}
*/

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * and cause by using reflection. This method attempts to find constructors `(() => String)`,
	  * `(() => String, Throwable)` `(() => String, Throwable, Boolean, Boolean)`,
	  * or `(String, () => String, Throwable, Boolean, Boolean)` (note that in the byte code `() => String`
	  * is equivalent to `=> String`), and provides as arguments `msg`, `cause`, and, optionally,
	  * `true` for the `Boolean` parameters presumed to be `enableSuppression` and `writeableStackTrace`.
	  */
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :() => String, cause :Throwable) :Opt[E] =
		lazyStringThrowableConstructor[E].map(_(msg, cause))
/*
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](msg :() => String, cause :Throwable) :Opt[E] = {
		val E = classTag[E].runtimeClass
		(findConstructor(E, LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringArg) match {
			case Got(cons) => Got(cons.newInstance(msg).asInstanceOf[E].initCause(cause))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(null, msg, null, cause, true, true))
			case _ => Lack
		}).downcastParam[E]
	}
*/

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given cause
	  * by using reflection. This method attempts to find constructors `(Throwable)`, `()`, `(String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * and provides as arguments an empty message (if required), the given cause and, optionally,
	  * `true` for the `Boolean` parameters presumed to be the standard flags `enableSuppression` and `writeableStackTrace`.
	  */
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](cause :Throwable) :Opt[E] =
		throwableConstructor[E].map(_(cause))
/*
	private[sugar] final def getThrowable[E <: Throwable :ClassTag](cause :Throwable) :Opt[E] = {
		val E = classTag[E].runtimeClass
		(findConstructor(E, ThrowableArg) match {
			case Got(cons) => Got(cons.newInstance(cause))
			case _ => Lack
		}).orElse(findConstructor(E, NoArgs) match {
			case Got(cons) => Got(cons.newInstance().asInstanceOf[E].initCause(cause))
		}).orElse(findConstructor(E, StringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance("", cause))
			case _ => Lack
		}).orElse(findConstructor(E, StringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance("", cause, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance("", null, cause, true, true))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(null, cause))
			case _ => Lack
		}).orElse(findConstructor(E, LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) => Got(cons.newInstance(null, cause, true, true))
			case _ => Lack
		}).downcastParam[E]
	}
*/

	/** Creates a new instance of the [[net.noresttherein.sugar.exceptions.Rethrowable]] class
	  * specified as the type parameter with, the given message and cause,
	  * by using reflection. This method attempts to find and use constructors with the following signatures:
	  *   1. `(String, Throwable, Boolean)` or `(() => String, Throwable, Boolean)`, applied to `(msg, cause, true)` -
	  *      the last parameter is assumed
	  *      to be the [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]] flag.
	  *   1. `(String, Throwable, Boolean, Boolean)` or `(() => String, Throwable, Boolean, Boolean)`,
	  *      applied to `(msg, cause, true, false)` - the last parameters are presumed to be flags
	  *      `enableSuppression`  and `writeableStackTrace`.
	  *   1. `(String, () => String, Throwable, Boolean)`, applied to `(msg, null, cause, true)` -
	  *      just as for `(String, Throwable, Boolean)`.
	  *   1. `(String, () => String, Throwable, Boolean Boolean)`, applied to `(msg, null, cause, true, true)`,
	  *      just as for `(String, Throwable, Boolean, Boolean)`.
	  *   1. If none of the specialized constructors is available, the default `(String, Throwable)`
	  *      and `(() => String, Throwable)` are checked;
	  *   1. Finally, the method looks for the simplest `(String)` (or `(() => String)`) constructor and uses
	  *      [[Throwable.initCause initCause]] to set the cause.
	  *
	  * In case of the first two constructors, the resulting exception should have non-writeable stack trace;
	  * instead, it will be initialized with the suffix of `cause`'s stack trace starting with most recent call to
	  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
	  *
	  * This method differs from the `getThrowable` methods in that it specifically looks to disable automatic filling
	  * of the stacktrace. It is used (indirectly) by `rethrow`, attempting to reduce the cost of throwing and handling
	  * the exception.
	  */
	private[sugar] final def getRethrowable[E <: Rethrowable :ClassTag](msg :String, cause :E) :Opt[E] = {
		implicit val constructors :Array[Constructor[E]] =
			classTag[E].runtimeClass.getDeclaredConstructors.castParam[Constructor[E]]
		(findConstructor(StringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(StringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, true)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, true, false)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringArg) match {
			case Got(cons) =>
				val e = cons.newInstance(msg).initCause(cause).downcastTo[E]
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, null, cause, true, false)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(() => msg, cause, true)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(() => msg, cause, true, false)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringArg) match {
			case Got(cons) =>
				val e = cons.newInstance(() => msg).initCause(cause).downcastTo[E]
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		})
	}

	/** Creates a new instance of the [[net.noresttherein.sugar.exceptions.Rethrowable]] class
	  * specified as the type parameter with, the given message and cause,
	  * by using reflection. This method attempts to find and use constructors with the following signatures:
	  *   1. `(() => String, Throwable, Boolean)`, applied to `(msg, cause, true)` - the last parameter is assumed
	  *      to be the [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]] flag.
	  *   1. `(() => String, Throwable, Boolean, Boolean)`, applied to `(msg, cause, true, false)` -
	  *      the last parameters are presumed to be flags `enableSuppression`  and `writeableStackTrace`.
	  *   1. `(String, () => String, Throwable, Boolean)`, applied to `(msg, null, cause, true)` -
	  *      just as for `(() => String, Throwable, Boolean)`.
	  *   1. `(String, () => String, Throwable, Boolean Boolean)`, applied to `(msg, null, cause, true, true)`,
	  *      just as for `(() => String, Throwable, Boolean, Boolean)`.
	  *   1. If none of the specialized constructors is available, `(() => String, Throwable)` is checked;
	  *   1. Finally, the method looks for `(() => String)` constructor and uses
	  *      [[Throwable.initCause initCause]] to set the cause.
	  *
	  * In case of the first two constructors, the resulting exception should have non-writeable stack trace;
	  * instead, it will be initialized with the suffix of `cause`'s stack trace starting with most recent call to
	  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
	  *
	  * This method differs from the `getThrowable` methods in that it specifically looks to disable automatic filling
	  * of the stacktrace. It is used (indirectly) by `rethrow`, attempting to reduce the cost of throwing and handling
	  * the exception.
	  */
	private[sugar] final def getRethrowable[E <: Rethrowable :ClassTag](msg :() => String, cause :E) :Opt[E] = {
		implicit val constructors :Array[Constructor[E]] =
			classTag[E].runtimeClass.getDeclaredConstructors.castParam[Constructor[E]]
		(findConstructor(LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, true)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, true, true)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringArg) match {
			case Got(cons) =>
				val e = cons.newInstance(msg).initCause(cause).downcastTo[E]
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(null, msg, cause, true, true)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		})
	}


	/** Creates a new exception of the class specified as the type parameter through reflection. */
	@throws[IllegalArgumentException]("if the class specifies none of (), (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newThrowable[E <: Throwable :ClassTag] :E =
		getThrowable[E] orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + ": " +
			"no (), (String), (String, Throwable), (String, Throwable, Boolean, Boolean), " +
			"(() => String), (() => String, Throwable), (() => String, Throwable, Boolean, Boolean), " +
			"or(String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing `msg` as the exception message argument.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newThrowable[E <: Throwable :ClassTag](msg :String) :E =
		getThrowable[E](msg) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + "): " +
			"no (String), (String, Throwable), (String, Throwable, Boolean Boolean), " +
			"(() => String), (() => String, Throwable), (() => String, Throwable, Boolean, Boolean), " +
			"or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing `msg` as the exception message argument.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newThrowable[E <: Throwable :ClassTag](msg :() => String) :E =
		getThrowable[E](msg) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + "): " +
			"no (() => String), (() => String, Throwable), (() => String, Throwable, Boolean Boolean), " +
			"or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given error message and cause as the arguments to the constructor.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newThrowable[E <: Throwable :ClassTag](msg :String, cause :Throwable) :E =
		getThrowable[E](msg, cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + ", " + cause + "): " +
			"no (String, Throwable), (String), (String, Throwable, Boolean, Boolean), " +
			"or (() => String, Throwable), (() => String), (() => String, Throwable, Boolean, Boolean), " +
			"or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given error message and cause as the arguments to the constructor.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (String), (String, Throwable) " +
		                              "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newThrowable[E <: Throwable :ClassTag](msg :() => String, cause :Throwable) :E =
		getThrowable[E](msg, cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + ", " + cause + "): " +
			"no (() => String, Throwable), (() => String), (() => String, Throwable, Boolean, Boolean), " +
			"or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given [[Throwable]] as the cause to the constructor.
	  */
	@throws[IllegalArgumentException]("if the class specifies none of (), (String), (String, Throwable) " +
	                                  "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newThrowable[E <: Throwable :ClassTag](cause :Throwable) :E =
		getThrowable[E](cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + cause + "): " +
			"no (Throwable), (String, Throwable), (), (String), (String, Throwable, Boolean Boolean), " +
			"or (() => String, Throwable), (() => String), (() => String, Throwable, Boolean, Boolean), " +
			"or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given message and cause as the constructor arguments. If possible,
	  * the exception will be created without automatic filling of the stack trace and instead initialize it
	  * by truncating the top frames of the `cause`'s stack trace, following a call to
	  * [[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
	  */
	@throws[IllegalArgumentException]("if the class specifies neither of (), (String), (String, Throwable) " +
	                                  "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newRethrowable[E <: Rethrowable :ClassTag](msg :String, cause :E) :E =
		getRethrowable[E](msg, cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + ", " + cause + "): " +
			"no (String, Throwable, Boolean), (String, Throwable, Boolean, Boolean), (String, Throwable), " +
			"or (() => String, Throwable, Boolean), (() => String, Throwable, Boolean, Boolean), " +
			"(() => String, Throwable), or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}

	/** Creates a new exception of the class specified as the type parameter through reflection,
	  * providing the given message and cause as the constructor arguments. If possible,
	  * the exception will be created without automatic filling of the stack trace and instead initialize it
	  * by truncating the top frames of the `cause`'s stack trace, following a call to
	  * [[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
	  */
	@throws[IllegalArgumentException]("if the class specifies neither of (), (String), (String, Throwable) " +
	                                  "or (String, Throwable, Boolean, Boolean) constructors.")
	private[sugar] final def newRethrowable[E <: Rethrowable :ClassTag](msg :() => String, cause :E) :E =
		getRethrowable[E](msg, cause) orIllegal {
			"Cannot create an instance of " + classTag[E].runtimeClass.name + "(" + msg + ", " + cause + "): " +
			"no (() => String, Throwable, Boolean), (() => String, Throwable, Boolean, Boolean), " +
			"(() => String, Throwable), or (String, () => String, Throwable, Boolean, Boolean) constructor."
		}


	private[sugar] final def raise[E <: Throwable :ClassTag] :Nothing =
		throw newThrowable[E]

	private[sugar] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw newThrowable[E](msg)

	private[sugar] final def raise[E <: Throwable :ClassTag](msg :() => String) :Nothing =
		throw newThrowable[E](msg)

}


//todo: remove eval and use the stack frames for both rethrow and apply here. Remember this will be a synthetic method!
class RethrowGuard[E <: Throwable] private[exceptions] (private val pckg :imports) extends AnyVal {
	@inline final def apply[T](action: => T )(errorMessage: => String)(implicit E :ClassTag[E]) :T =
		try { eval(action) } catch {
			case E(e) =>
				throw pckg.pushErrorMessage(errorMessage).applyOrElse(e, pckg.suppressErrorMessage(errorMessage))
		}
}
