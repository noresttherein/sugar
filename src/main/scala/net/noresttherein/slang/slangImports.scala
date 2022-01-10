package net.noresttherein.slang

import scala.reflect.{classTag, ClassTag}
import scala.util.Try






/** A namespace some non-implicit methods which are used by this library, but can also be useful in the client code.
  * Extracted into a trait so that users can extend their package objects from it, bringing everything into
  * the lexical scope of classes located therein. Extended by the `slang` package object.
  * @author Marcin Mościcki
  */
trait slangImports {
	//todo: i.++  ++.i macros

	type ->[+L, +R] = (L, R)

	/** A 'WTF' method throwing an [[net.noresttherein.slang.ImpossibleError ImpossibleError]].
	  * Intended for code which should, to the best of the programmer's - but not compiler's - knowledge, unreachable.
	  * Placed after infinite loops, as the body of methods which are never called (but, for example, remain
	  * for binary compatibility), or methods of sealed classes which are overriden by subclasses and similar
	  * circumstances.
	  */
	def ??! :Nothing = throw new ImpossibleError



	/** Throws an [[UnsupportedOperationException]]. */
	@inline final def unsupported_! :Nothing =
		throw new UnsupportedOperationException

	/** Throws an [[UnsupportedOperationException]]. */
	@inline final def unsupported_!(msg :String) :Nothing =
		throw new UnsupportedOperationException(msg)

	/** Throws a [[NoSuchElementException]]. */
	@inline final def noneSuch_!(msg :String) :Nothing =
		throw new NoSuchElementException(msg)

	/** Throws an [[IllegalArgumentException]]. */
	@inline final def illegal_!(msg :String) :Nothing =
		throw new IllegalArgumentException(msg)

	/** Throws a [[net.noresttherein.slang.ImpossibleError]].  */
	@inline final def impossible_!(msg :String) :Nothing = throw new ImpossibleError(msg)



	private def newThrowable[E <: Throwable :ClassTag](msg :String) :E =
		(Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[E]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null).asInstanceOf[E]
		} recover {
			case ex :Exception => throw new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass}($msg) as a result of a guard failure: no constructor (String) or (String, Throwable).",
				ex
			)
		}).get

	private[slang] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw newThrowable[E](msg)

	private[slang] final def raise[E <: Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance("").asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance("", null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of a guard failure: no constructor (), (String) or (String, Throwable).",
				ex
			)
		}).get

	/** Executes `action` expression, catching all standard scala exceptions
	  * (all exceptions with type aliases/classes in the `scala` package),
	  * with the exception of [[Exception]], [[RuntimeException]], [[Error]], [[Throwable]]
	  * and rethrowing them with an additional error message. The rethrown exception will be a new instance
	  * of the caught class, with `errorMessage` as its `message` and the original exception as its `cause`.
	  * Note that any subclasses of these exceptions will be caught together with their base class,
	  * and the rethrown exception thus can be more general than the original.
	  * Also note that, contrary to the convention, this method will catch
	  * [[AbstractMethodError]] and [[AssertionError]].
	  *
	  *
	  * This method is useful for adding context information about the executed action which, most likely,
	  * is unavailable in location where the exception is thrown, without changing the exception thrown,
	  * in case code higher up the call stack wishes to catch.
	  *
	  *   1. If the caught exception is a [[net.noresttherein.slang.StackableThrowable StackableThrowable]],
	  *      its [[net.noresttherein.slang.StackableThrowable.pushMessage pushMessage]] is used to create its copy
	  *      with the given message and the original exception as its cause.
	  *   1. Otherwise, if the exception is one of standard Java/Scala exceptions, then a new instance of that class
	  *      is created and rethrown statically (handling all subclasses of the given exception at the same time).
	  *   1. If the class of the caught exception defines either a `(String)` or `(String, Throwable)` constructor,
	  *      then a new instance is created through reflection.
	  *   1. Finally, if the previous point fails with an exception, an `IllegalArgumentException` with the
	  *      error message is added as a suppressed exception.
	  */
	final def rethrow[T](action: => T)(errorMessage: => String) :T =
		try { action } catch pushErrorMessage(errorMessage).andThen(throw _)

	def pushErrorMessage(msg: => String) :PartialFunction[Throwable, Throwable] = {
		case e :StackableThrowable              => e.pushMessage(msg)
		case e :NumberFormatException           => new NumberFormatException(msg).initCause(e)
		case e :IllegalArgumentException        => new IllegalArgumentException(msg, e)
		case e :UnsupportedOperationException   => new UnsupportedOperationException(msg, e)
		case e :NoSuchElementException          => new NoSuchElementException(msg).initCause(e)
		case e :IllegalStateException           => new IllegalStateException(msg, e)
		case e :ArrayIndexOutOfBoundsException  => new ArrayIndexOutOfBoundsException(msg).initCause(e)
		case e :StringIndexOutOfBoundsException => new StringIndexOutOfBoundsException(msg).initCause(e)
		case e :IndexOutOfBoundsException       => new IndexOutOfBoundsException(msg).initCause(e)
		case e :ClassCastException              => new ClassCastException(msg).initCause(e)
		case e :NullPointerException            => new NullPointerException(msg).initCause(e)
		case e :InterruptedException            => new InterruptedException(msg).initCause(e)
		case e :AbstractMethodError             => new AbstractMethodError(msg).initCause(e)
		case e :AssertionError                  => new AssertionError(msg, e)
		case e :Exception                       => try {
			newThrowable[Exception](msg)(ClassTag(e.getClass))
		} catch {
			case e1 :Exception => e.addSuppressed(e1); e
		}
	}
}
