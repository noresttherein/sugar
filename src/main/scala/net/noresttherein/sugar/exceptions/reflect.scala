package net.noresttherein.sugar.exceptions

import java.lang.reflect.Constructor

import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.exceptions.Constructors.{LazyStringArg, LazyStringThrowableArgs, LazyStringThrowableBoolArgs, LazyStringThrowableBoolBoolArgs, StringArg, StringLazyStringThrowableArgs, StringLazyStringThrowableBoolArgs, StringLazyStringThrowableBoolBoolArgs, StringThrowableArgs, StringThrowableBoolArgs, StringThrowableBoolBoolArgs, defaultConstructor, findConstructor, lazyStringConstructor, lazyStringThrowableConstructor, stringConstructor, stringThrowableConstructor, throwableConstructor}
import net.noresttherein.sugar.exceptions.reflect.{IAE, IOOBE, NPE, NSEE, UOE}
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Opt.implicits.gotAny




/** Factory methods for exceptions which use reflection to match method arguments to one of standard
  * `Exception` constructor signature.
  */
private[sugar] trait reflect extends Any {
	/** Creates a new instance of the [[Throwable]] class specified as the type parameter by using reflection.
	  * This method attempts to find constructors `()`, `(String)`, `(String, Throwable)`,
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides the same arguments that `Throwable`'s default constructor would.
	  */
	private[sugar] final def getThrowable[E <: Throwable](implicit tag :ClassTag[E]) :Opt[E] =
		(tag.runtimeClass :Any) match {
			case IAE   => Got(new IllegalArgumentException).castParam[E]
			case IOOBE => Got(new IndexOutOfBoundsException).castParam[E]
			case NSEE  => Got(new NoSuchElementException).castParam[E]
			case UOE   => Got(new UnsupportedOperationException).castParam[E]
			case NPE   => Got(new NullPointerException).castParam[E]
			case _     => defaultConstructor[E].map(_())
		}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * by using reflection. This method attempts to find constructors `(String)`, `(String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides as an argument `msg` and same arguments that `Throwable`'s default constructor would.
	  */
	private[sugar] final def getThrowable[E <: Throwable](msg :String)(implicit tag :ClassTag[E]) :Opt[E] =
		(tag.runtimeClass :Any) match {
			case IAE   => Got(new IllegalArgumentException(msg)).castParam[E]
			case IOOBE => Got(new IndexOutOfBoundsException(msg)).castParam[E]
			case NSEE  => Got(new NoSuchElementException(msg)).castParam[E]
			case UOE   => Got(new UnsupportedOperationException(msg)).castParam[E]
			case NPE   => Got(new NullPointerException(msg)).castParam[E]
			case _     =>stringConstructor[E].map(_(msg))
		}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * by using reflection. This method attempts to find constructors `(() => String)`, `(() => String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, or `(String, () => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides as an argument `msg` and same arguments that `Throwable`'s default constructor would.
	  */
	private[sugar] final def getThrowable[E <: Throwable](msg :() => String)(implicit tag :ClassTag[E]) :Opt[E] =
		(tag.runtimeClass :Any) match {
			case IAE   => Got(new LazyIllegalArgumentException(msg())).castParam[E]
			case IOOBE => Got(new LazyIndexOutOfBoundsException(msg())).castParam[E]
			case NSEE  => Got(new LazyNoSuchElementException(msg())).castParam[E]
			case UOE   => Got(new LazyUnsupportedOperationException(msg())).castParam[E]
			case NPE   => Got(new LazyNullPointerException(msg())).castParam[E]
			case _     => lazyStringConstructor[E].map(_(msg))
		}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * and cause by using reflection. This method attempts to find constructors `(String)`, `(String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * (note that in the byte code `() => String` is equivalent to `=> String`),
	  * and provides as arguments `msg`, `cause`, and, optionally, `true` for the `Boolean` parameters
	  * presumed to be `enableSuppression` and `writeableStackTrace`.
	  */
	private[sugar] final def getThrowable[E <: Throwable]
	                                     (msg :String, cause :Throwable)(implicit tag :ClassTag[E]) :Opt[E] =
		(tag.runtimeClass :Any) match {
			case IAE   => Got(new IllegalArgumentException(msg, cause)).castParam[E]
			case IOOBE => Got(new IndexOutOfBoundsException(msg).initCause(cause)).castParam[E]
			case NSEE  => Got(new NoSuchElementException(msg, cause)).castParam[E]
			case UOE   => Got(new UnsupportedOperationException(msg, cause)).castParam[E]
			case NPE   => Got(new NullPointerException(msg).initCause(cause)).castParam[E]
			case _     => stringThrowableConstructor[E].map(_(msg, cause))
		}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given message
	  * and cause by using reflection. This method attempts to find constructors `(() => String)`,
	  * `(() => String, Throwable)` `(() => String, Throwable, Boolean, Boolean)`,
	  * or `(String, () => String, Throwable, Boolean, Boolean)` (note that in the byte code `() => String`
	  * is equivalent to `=> String`), and provides as arguments `msg`, `cause`, and, optionally,
	  * `true` for the `Boolean` parameters presumed to be `enableSuppression` and `writeableStackTrace`.
	  */
	private[sugar] final def getThrowable[E <: Throwable]
	                                     (msg :() => String, cause :Throwable)(implicit tag :ClassTag[E]) :Opt[E] =
		(tag.runtimeClass :Any) match {
			case IAE   => Got(new LazyIllegalArgumentException(msg(), cause)).castParam[E]
			case IOOBE => Got(new LazyIndexOutOfBoundsException(msg(), cause)).castParam[E]
			case NSEE  => Got(new LazyNoSuchElementException(msg(), cause)).castParam[E]
			case UOE   => Got(new LazyUnsupportedOperationException(msg(), cause)).castParam[E]
			case NPE   => Got(new LazyNullPointerException(msg(), cause)).castParam[E]
			case _     => lazyStringThrowableConstructor[E].map(_(msg, cause))
		}

	/** Creates a new instance of the [[Throwable]] class specified as the type parameter with the given cause
	  * by using reflection. This method attempts to find constructors `(Throwable)`, `()`, `(String, Throwable)`
	  * `(String, Throwable, Boolean, Boolean)`, `(String, () => String, Throwable, Boolean, Boolean)`,
	  * `(() => String)`, `(() => String, Throwable)`, or `(() => String, Throwable, Boolean, Boolean)`
	  * and provides as arguments an empty message (if required), the given cause and, optionally,
	  * `true` for the `Boolean` parameters presumed to be the standard flags `enableSuppression` and `writeableStackTrace`.
	  */
	private[sugar] final def getThrowable[E <: Throwable]
	                                     (cause :Throwable)(implicit tag :ClassTag[E]) :Opt[E] =
		(tag.runtimeClass :Any) match {
			case IAE   => Got(new IllegalArgumentException(cause)).castParam[E]
			case IOOBE => Got(new IndexOutOfBoundsException().initCause(cause)).castParam[E]
			case NSEE  => Got(new NoSuchElementException(cause)).castParam[E]
			case UOE   => Got(new UnsupportedOperationException(cause)).castParam[E]
			case NPE   => Got(new NullPointerException().initCause(cause)).castParam[E]
			case _     => throwableConstructor[E].map(_(cause))
		}


	/** Creates, using reflection, a new instance of the [[net.noresttherein.sugar.exceptions.Rethrowable]] subclass
	  * specified as the type parameter the given message and cause.
	  * This method attempts to find and use a constructor with one of the following signatures:
	  *   1. first, the class is checked for a matching `(String, Throwable)`, after which the heuristic looks for
	  *   1. `(String, Throwable, Boolean)`, applied to `(msg, cause, cause != null)` - the last parameter is assumed
	  *      to be the [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]] flag,
	  *   1. `(String, Throwable, Boolean, Boolean)` or `(() => String, Throwable, Boolean, Boolean)`,
	  *      applied to `(msg, cause, true, cause == null)` - the last parameters are presumed to be flags
	  *      `enableSuppression`  and `writableStackTrace`,
	  *   1. if none of the above is found, then the search proceeds to look for constructors
	  *      using a lazy `message` argument, starting with `(() => String, Throwable)`, followed by
	  *   1. `(() => String, Throwable, Boolean)`, interpreted like `(String, Throwable, Boolean)`,
	  *   1. `(() => String, Throwable, Boolean, Boolean)`,
	  *   1. `(String, () => String, Throwable)`,
	  *   1. `(String, () => String, Throwable, Boolean, Boolean)`, applied to `(msg, null, cause, true, true)`,
	  *      just as for `(String, Throwable, Boolean, Boolean)`,
	  *   1. `(String, () => String, Throwable)`, applied to `(msg, null, cause)`,
	  *   1. `(String, () => String, Throwable, Boolean)`, and, finally,
	  *   1. `(String, () => String, Throwable, Boolean, Boolean)`.
	  *
	  * Note that, in the byte code, `() => String` is the same as `=> String`.
	  * If the `isRethrown` property of the created exception does not equal `cause != null`, then the search proceeds
	  * to the next point regardless. While impossible to validate, rethrown instances (with non null causes0
	  * should have non-writeable stack traces; they will be initialized with the suffix of `cause`'s stack trace
	  * starting with most recent call to
	  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
	  */
	private[sugar] final def getRethrowable[E <: Rethrowable :ClassTag](msg :String, cause :E) :Opt[E] = {
		implicit val constructors :Array[Constructor[E]] =
			classTag[E].runtimeClass.getDeclaredConstructors.castParam[Constructor[E]]
		(findConstructor(StringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(StringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, cause != null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, true, cause == null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		//can't set cause because fillInStackTrace is called from the constructor
		}).orElse(findConstructor(LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(() => msg, cause))
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(() => msg, cause, cause != null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(() => msg, cause, true, cause == null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, null, cause)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, null, cause, cause != null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, null, cause, true, cause == null)
				if (e.isRethrown) Got(e) else Lack
			case _ => Lack
		//Constructors without a cause won't work because fillInStackTrace is called from the constructor.
//		}).orElse(findConstructor(StringArg) match {
//			case Got(cons) =>
//				val e = cons.newInstance(msg).initCause(cause).downcastTo[E]
//				if (e.isRethrown == (cause != null)) Got(e) else Lack
//			case _ => Lack
//		}).orElse(findConstructor(LazyStringArg) match {
//			case Got(cons) =>
//				val e = cons.newInstance(() => msg).initCause(cause).downcastTo[E]
//				if (e.isRethrown == (cause != null)) Got(e) else Lack
//			case _ => Lack
		})
	}

	/** Creates, using reflection, a new instance of the [[net.noresttherein.sugar.exceptions.Rethrowable]] subclass
	  * specified as the type parameter with the given message and cause.
	  * This method attempts to find and use a constructor with one of the following signatures:
	  *   1. first, a matching `(() => String, Throwable)`, followed by
	  *   1. `(() => String, Throwable, Boolean)`, applied to `(msg, cause, cause != null)` - the last parameter
	  *      is assumed to be the [[net.noresttherein.sugar.exceptions.Rethrowable.isRethrown isRethrown]] flag,
	  *   1. `(() => String, Throwable, Boolean, Boolean)`, applied to `(msg, cause, true, cause == null)` -
	  *      the last parameters are presumed to be flags `enableSuppression`  and `writeableStackTrace`,
	  *   1. `(String, () => String, Throwable, Boolean)`, applied to `(null, msg, cause, true)` -
	  *      similarly as `(() => String, Throwable, Boolean)`,
	  *   1. `(String, () => String, Throwable)`, and, in the same vein,
	  *   1. `(String, () => String, Throwable, Boolean)`, applied to `(msg, null, cause, cause != null)`,
	  *      just as for `(() => String, Throwable, Boolean)`, and, finally
	  *   1. `(String, () => String, Throwable, Boolean, Boolean)`, applied to `(msg, null, cause, true, cause == null)`,
	  *      just as for `(() => String, Throwable, Boolean, Boolean)`.
	  *
	  * Note that, in the byte code, `() => String` is the same as `=> String`.
	  * If the `isRethrown` property of the created exception does not equal `cause != null`, then the search proceeds
	  * to the next point regardless. While impossible to validate, rethrown instances (with non null causes0
	  * should have non-writeable stack traces; they will be initialized with the suffix of `cause`'s stack trace
	  * starting with most recent call to
	  * [[net.noresttherein.sugar.exceptions.imports imports]]`.`[[net.noresttherein.sugar.exceptions.imports.rethrow rethrow]].
	  */
	private[sugar] final def getRethrowable[E <: Rethrowable :ClassTag](msg :() => String, cause :E) :Opt[E] = {
		implicit val constructors :Array[Constructor[E]] =
			classTag[E].runtimeClass.getDeclaredConstructors.castParam[Constructor[E]]
		(findConstructor(LazyStringThrowableArgs) match {
			case Got(cons) => Got(cons.newInstance(msg, cause))
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, cause != null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(LazyStringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(msg, cause, true, cause == null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableArgs) match {
			case Got(cons) => cons.newInstance(null, msg, cause)
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(null, msg, cause, cause != null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		}).orElse(findConstructor(StringLazyStringThrowableBoolBoolArgs) match {
			case Got(cons) =>
				val e = cons.newInstance(null, msg, cause, true, cause == null)
				if (e.isRethrown == (cause != null)) Got(e) else Lack
			case _ => Lack
		//Constructors without a cause won't work because fillInStackTrace is called from the constructor.
//		}).orElse(findConstructor(LazyStringArg) match {
//			case Got(cons) =>
//				val e = cons.newInstance(msg).initCause(cause).downcastTo[E]
//				if (e.isRethrown == (cause != null)) Got(e) else Lack
//			case _ => Lack
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


	//consider: moving it to imports
	/** Throws an exception of the class specified by the type argument. The exception class must provide
	  * at least one constructor matching the signature of some standard `Throwable` constructor:
	  *   - `()`,
	  *   - `(String)`,
	  *   - `(String, Throwable)`,
	  *   - `(String, Throwable, Boolean, Boolean)`,
	  *   - `(String, () => String, Throwable, Boolean, Boolean)`,
	  *   - `(() => String)`,
	  *   - `(() => String, Throwable)`
	  *   - `(() => String, Throwable, Boolean, Boolean)`.
	  *
	  * Note that in the byte code `() => String` is equivalent to `=> String`. The arguments provided are the same
	  * as in the case of the default (zero argument) `Throwable` constructor.
	  *
	  * This method relies on reflection, and performs repeated searches through the constructor list.
	  * It is significantly slower than throwing an exception explicitly, and should not be used
	  * in performance critical code.
	  */ //todo: add macro versions
	private[sugar] final def raise[E <: Throwable :ClassTag] :Nothing =
		throw newThrowable[E]

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
	  * It is significantly slower than throwing an exception explicitly, and should not be used
	  * in performance critical code. It is however more succinct and generic, which may make it useful
	  * in domain specific languages.
	  */ //todo: add macro versions
	private[sugar] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw newThrowable[E](msg)
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

}


private[sugar] object reflect extends reflect {
	private final val IAE   = classOf[IllegalArgumentException]
	private final val IOOBE = classOf[IndexOutOfBoundsException]
	private final val NSEE  = classOf[NoSuchElementException]
	private final val UOE   = classOf[UnsupportedOperationException]
	private final val NPE   = classOf[NullPointerException]
}






/** Functions of standard signatures creating instances of exceptions of a specified type by reflection. */
private object Constructors {

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
			findConstructor(StringArg).map { cons => cons.newInstance(_ :String).initCause(_ :Throwable).asSubtype[T] }
		).orElse(
			findConstructor(StringThrowableBoolBoolArgs).map {
				cons => cons.newInstance(_ :String, _ :Throwable, true, true)
			}
		).orElse(
			findConstructor(LazyStringThrowableArgs).map {
				cons => (s :String, e :Throwable) => cons.newInstance(() => s, e) }
		).orElse(
			findConstructor(LazyStringArg).map {
				cons => (s :String, e :Throwable) => cons.newInstance(s).initCause(e).asSubtype[T]
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
				cons => cons.newInstance(_ :() => String).initCause(_ :Throwable).asSubtype[T]
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
		while (i < count && !(constructors(i).getParameterTypes sameElements paramTypes))
			i += 1
		if (i == count) Lack else Got(constructors(i))
	}


	private[exceptions] final def findConstructor[T](clazz :Class[T], paramTypes :Array[Class[_]]) :Opt[Constructor[T]] =
		findConstructor(paramTypes)(clazz.getDeclaredConstructors.castParam[Constructor[T]])

}
