package net.noresttherein.sugar.exceptions

import java.io.IOException
import java.lang.reflect.Constructor
import java.sql.SQLException
import java.util.ConcurrentModificationException

import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.arrays.ArrayLike
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.Constructors.{LazyStringArg, LazyStringThrowableArgs, LazyStringThrowableBoolArgs, LazyStringThrowableBoolBoolArgs, StringArg, StringLazyStringThrowableBoolArgs, StringLazyStringThrowableBoolBoolArgs, StringThrowableArgs, StringThrowableBoolArgs, StringThrowableBoolBoolArgs, defaultConstructor, findConstructor, lazyStringConstructor, lazyStringThrowableConstructor, stringConstructor, stringThrowableConstructor, throwableConstructor}
import net.noresttherein.sugar.exceptions.reflect.newThrowable
import net.noresttherein.sugar.extensions.{ClassExtension, castTypeParamMethods, castingMethods, classNameMethods, downcastTypeParamMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** Definitions of various methods throwing or creating exceptions. Extended by
  * [[net.noresttherein.sugar.exceptions exceptions]] package object and (indirectly,
  * through [[net.noresttherein.sugar.imports sugar.imports]] by package object [[net.noresttherein.sugar sugar]].
  * They exist not only for convenience, but also reduce the byte code size of the calling class, increasing
  * its chances of being compiled in the runtime.
  * @see [[net.noresttherein.sugar.exceptions.validate validate]]
  */
trait imports {

//	@inline final def leaveCrumbs[T](params :Any*)(block: => T) :T = StackContext(new StackContext(params))(block)
//	@inline final def trace[T](info: => String)(block: => T) :T = StackContext(new StackContext())
//	@inline final def trace[T](block: => T) :T = macro

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
	  * to this method. May be delegated to by overridden `pushErrorMessage` by more performance conscious code.
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
		(classOf[IOException],                     new IOException(_, _)), //todo: there is a shitload of thesee
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
		(classOf[SQLException],                    new SQLException(_, _)),
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



	/** Throws an exception of the class specified by the type argument. The exception class must provide
	  * at least one constructor matching the signature of some standard `Throwable` constructor:
	  *   - `()`,
	  *   - `(String)`,
	  *   - `(String, Throwable)`,
	  *   - `(String, Throwable, Boolean, Boolean)`,
	  *   - `(String, () => String, Throwable, Boolean, Boolean)`,
	  *   - `(() => String)`,
	  *   - `(() => String, Throwable)`,
	  *   - `(() => String, Throwable, Boolean, Boolean)`.
	  *
	  * Note that in the byte code `() => String` is equivalent to `=> String`. The arguments provided are the same
	  * as in the case of the default (zero argument) `Throwable` constructor.
	  *
	  * This method relies on reflection, and performs repeated searches through the constructor list.
	  * With the exception of a couple cached, most often used exception classes it is significantly slower
	  * than throwing an exception explicitly, and should not be used in performance critical code.
	  */
	final def raise[E <: Throwable :ClassTag] :Nothing = throw newThrowable[E]

	/** Throws an exception, with the given message, of the class specified by the type argument. The exception class
	  * must provide at least one constructor matching the signature of some standard `Throwable` constructor:
	  *   - `(String)`,
	  *   - `(String, Throwable)`,
	  *   - `(String, Throwable, Boolean, Boolean)`,
	  *   - `(String, () => String, Throwable, Boolean, Boolean)`,
	  *   - `(() => String)`,
	  *   - `(() => String, Throwable)`,
	  *   - `(() => String, Throwable, Boolean, Boolean)`
	  *
	  * Note that in the byte code `() => String` is equivalent to `=> String`. The arguments provided are the same
	  * as in the case of the default (zero argument) `Throwable` constructor.
	  *
	  * This method relies on reflection, and performs repeated searches through the constructor list.
	  * With the exception of a couple cached, most often used exception classes, it is significantly slower
	  * than throwing an exception explicitly, and should not be used in performance critical code.
	  * It is however more succinct and generic, which may make it useful in domain specific languages.
	  */ //todo: add macro versions
	final def raise[E <: Throwable :ClassTag](msg :String) :Nothing = throw newThrowable[E](msg)

	/** Throws an exception of the class specified by the type argument, with the argument as its cause.
	  * The exception class must provide at least one constructor
	  * matching the signature of some standard `Throwable` constructor:
	  *   - `(Throwable)`,
	  *   - `(String, Throwable)`,
	  *   - `()`,
	  *   - `(String)`
	  *   - `(String, Throwable, Boolean, Boolean)`,
	  *   - `(() => String)`,
	  *   - `(() => String, Throwable)`,
	  *   - `(() => String, Throwable, Boolean, Boolean)`,
	  *   - `(String, () => String, Throwable, Boolean, Boolean)`,
	  *
	  * This method relies on reflection, and performs repeated searches through the constructor list.
	  * With the exception of a couple cached, most often used exception classes, it is significantly slower
	  * than throwing an exception explicitly, and should not be used in performance critical code.
	  * It is however more succinct and generic, which may make it useful in domain specific languages.
	  */
	final def raise[E <: Throwable :ClassTag](cause :Throwable) :Nothing = throw newThrowable[E](cause)

	/** Throws an exception of the class specified by the type argument, with the argument as its cause.
	  * The exception class must provide at least one constructor
	  * matching the signature of some standard `Throwable` constructor:
	  *   - `(String, Throwable)`,
	  *   - `(String)`,
	  *   - `(String, Throwable, Boolean, Boolean)`,
	  *   - `(() => String, Throwable)`,
	  *   - `(() => String)`,
	  *   - `(() => String, Throwable, Boolean, Boolean)`,
	  *   - `(String, () => String, Throwable, Boolean, Boolean)`,
	  *
	  * This method relies on reflection, and performs repeated searches through the constructor list.
	  * With the exception of a couple cached, most often used exception classes, it is significantly slower
	  * than throwing an exception explicitly, and should not be used in performance critical code.
	  * It is however more succinct and generic, which may make it useful in domain specific languages.
	  */
	final def raise[E <: Throwable :ClassTag](msg :String, cause :Throwable) :Nothing =
		throw newThrowable[E](msg, cause)
//
//	/** Throws an exception with the given, lazily evaluated message, of the class specified by the type argument.
//	  * The exception class must provide at least one constructor matching the signature analogous to
//	  * one of the standard `Throwable` constructors:
//	  *   - `(() => String)`,
//	  *   - `(() => String, Throwable)`,
//	  *   - `(String, Throwable, Boolean, Boolean)`,
//	  *   - `(String, () => String, Throwable, Boolean, Boolean)`.
//	  *
//	  * Note that in the byte code `() => String` is equivalent to `=> String`. The arguments provided are the same
//	  * as in the case of the default (zero argument) `Throwable` constructor.
//	  *
//	  * This method relies on reflection, and performs repeated searches through the constructor list.
//	  * It is significantly slower than throwing an exception explicitly, and should not be used
//	  * in performance critical code. It is however more succinct and generic, which may make it useful
//	  * in domain specific languages.
//	  */ //todo: add macro versions
//	private[sugar] final def raise[E <: Throwable :ClassTag](msg :() => String) :Nothing =
//		throw newThrowable[E](msg)


	/** A 'WTF' method throwing an [[ImpossibleError ImpossibleError]].
	  * Intended for code which should, to the best of the programmer's - but not compiler's - knowledge, be unreachable.
	  * Placed after infinite loops, as the body of methods which are never called (but, for example, remain
	  * for binary compatibility), or methods of sealed classes which are overriden by subclasses and similar
	  * circumstances.
	  */
	def ??! :Nothing = throw ImpossibleError()

	/** Throws an [[net.noresttherein.sugar.exceptions.ImpossibleError ImpossibleError]].  */
	final def impossible_!(msg :String, cause :Throwable = null) :Nothing =
		throw ImpossibleError(msg)

	/** Throws an [[java.lang.AssertionError AssertionError]]. */
	def !!! :Nothing = throw new AssertionError

	/** Throws an [[java.lang.AssertionError AssertionError]] with the given message. */
	def !!!(msg :String) :Nothing = throw new AssertionError(msg)

	/** Throws an [[net.noresttherein.sugar.exceptions.Oops Oops]] with the given message,
	  * to indicate a situation which should not have happened and is due to a bug in the executed code.
	  */
	def oops(msg :String) :Nothing = throw Oops(msg)

	/** Throws an [[net.noresttherein.sugar.exceptions.Oops Oops]] with the given message,
	  * to indicate a situation which should not have happened and is due to a bug in the executed code.
	  */
	def oops(msg :String, cause :Throwable) :Nothing = throw Oops(msg, cause)

	/** Throws an [[UnsupportedOperationException]]. */
	final def unsupported_! :Nothing =
		throw SugaredUnsupportedOperationException()

	/** Throws an [[UnsupportedOperationException]]. */
	final def unsupported_!(msg :String, cause :Throwable = null) :Nothing =
		throw SugaredUnsupportedOperationException(msg, cause)

	/** Throws an [[UnsupportedOperationException]]`(s"$obj.$method")`. */
	final def unsupported_!(obj :String, method :String) :Nothing =
		throw SugaredUnsupportedOperationException(obj + '.' + method)

	/** Throws an [[UnsupportedOperationException]]`(s"${obj.className}.$method")`. */
	final def unsupported_!(obj :Any, method :String) :Nothing =
		throw SugaredUnsupportedOperationException(obj.className + '.' + method)

	/** Throws a [[NoSuchElementException]]. */
	final def noSuch_!(msg :String, cause :Throwable = null) :Nothing =
		throw SugaredNoSuchElementException(msg, cause)

	/** Throws a [[NoSuchElementException]].
	  * @param empty an empty collection whose element was accessed, and is used to enhance the error message.
	  */
	final def noSuch_!(empty :IterableOnce[_]) :Nothing =
		throw SugaredNoSuchElementException(empty.toString + " is empty.")

	/** Throws a [[NoSuchElementException]].
	  * @param obj        an object whose property was accessed.
	  * @param methodName the name of the method of `obj` which caused the exception to be thrown.
	  */
	final def noSuch_!(obj :Any, methodName :String) :Nothing =
		throw SugaredNoSuchElementException(obj.toString + "." + methodName)

	/** Throws a [[NoSuchElementException]].
	  * The argument is an empty collection whose element was accessed, and is used to enhance the error message.
	  */
	final def noSuch_!(empty :ArrayLike[_]) :Nothing =
		throw SugaredNoSuchElementException(errorString(empty) + " is empty.")

	/** Throws an [[IllegalArgumentException]]. */
	final def illegal_!(msg :String, cause :Throwable = null) :Nothing =
		throw SugaredIllegalArgumentException(msg, cause)

	/** Throws an [[IllegalArgumentException]].
	  * @param method the name of the method to which an illegal argument was passed.
	  * @param param  the name of the formal parameter for which an illegal value was passed.
	  * @param arg    the illegal argument value.
	  */
	final def illegal_!(method :String, param :String, arg :Any) :Nothing =
		throw SugaredIllegalArgumentException("Illegal " + param + " argument for " + method + ": " + arg + ".")

	/** Throws an [[IllegalArgumentException]].
	  * @param method the name of the method to which an illegal argument was passed.
	  * @param param  the name of the formal parameter for which an illegal value was passed.
	  * @param arg    the illegal argument value.
	  * @param reason a more detailed explanation why the argument is invalid.
	  */
	final def illegal_!(method :String, param :String, arg :Any, reason :String) :Nothing =
		throw SugaredIllegalArgumentException(
			"Illegal " + param + " argument for " + method + ": " + arg + " - " + reason + "."
		)

	/** Throws an [[IllegalArgumentException]].
	  * @param param  the name of the parameter for which an illegal value was passed.
	  * @param arg    the illegal argument value.
	  * @param reason a more detailed explanation why the argument is invalid.
	  */
	final def illegal_!(param :String, arg :Any, reason :String) :Nothing =
		throw SugaredIllegalArgumentException(
			"Illegal " + param + " argument : " + arg + " - " + reason + "."
		)

	/** Throws an [[IndexOutOfBoundsException]]. */
	final def outOfBounds_!(idx :Int) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString)

	/** Throws an [[IndexOutOfBoundsException]]. */
	final def outOfBounds_!(idx :Int, size :Int) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString + " out of " + size)

	/** Throws an [[IndexOutOfBoundsException]]. */
	final def outOfBounds_!(idx :Int, min :Int, max :Int) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString + " out of bounds [" + min + ", " + max + ")")

	/** Throws an [[IndexOutOfBoundsException]]. The second argument is used to enhance the error message
	  * and should provide information about the legal range.
	  * @throws SugaredIndexOutOfBoundsException with message `"idx.toString + " out of bounds for " + source`.
	  */
	final def outOfBounds_!(idx :Int, source :String) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString + " out of bounds for " + source)

	/** Throws an [[IndexOutOfBoundsException]]. The second argument is the collection which was accessed,
	  * and is used to enhance the error message.
	  */
	final def outOfBounds_!(idx :Int, items :Iterable[_]) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString + " out of bounds for " + errorString(items))

	/** Throws an [[IndexOutOfBoundsException]]. The second argument is the array for whch the index was out of bounds,
	  * and is used to enhance the error message.
	  */
	final def outOfBounds_!(idx :Int, items :ArrayLike[_]) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString + " out of bounds for " + errorString(items))

	/** Throws an [[IndexOutOfBoundsException]]. */
	final def outOfBounds_!(idx :Int, cause :Throwable) :Nothing =
		throw SugaredIndexOutOfBoundsException(idx.toString, cause)

	/** Throws an [[IndexOutOfBoundsException]]. */
	final def outOfBounds_!(msg :String, cause :Throwable = null) :Nothing =
		throw SugaredIndexOutOfBoundsException(msg, cause)

	/** Throws a [[NullPointerException]] with the given message.
	  * This method is useful for reducing calling methods bytecode size.
	  */
	final def null_!(msg :String) :Nothing = throw SugaredNullPointerException(msg)

	/** Throws a [[ConcurrentModificationException]] with the given message.
	  * This method is useful for reducing calling methods bytecode size.
	  */
	final def concurrent_!(msg :String) :Nothing = throw SugaredConcurrentModificationException(msg)
}



//todo: remove eval and use the stack frames for both rethrow and apply here. Remember this will be a synthetic method!
class RethrowGuard[E <: Throwable] private[exceptions] (private val pckg :imports) extends AnyVal {
	@inline final def apply[T](action: => T)(errorMessage: => String)(implicit E :ClassTag[E]) :T =
		try { eval(action) } catch {
			case E(e) =>
				throw pckg.pushErrorMessage(errorMessage).applyOrElse(e, pckg.suppressErrorMessage(errorMessage))
		}
}
