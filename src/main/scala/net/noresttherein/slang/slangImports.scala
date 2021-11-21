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

	/** The 'WTF' method throwing
	  * a [[net.noresttherein.slang.ThisShouldBeImpossibleException ThisShouldBeImpossibleException]].
	  * Intended for code which should, to the best of the programmer's - but not compiler's - knowledge, unreachable.
	  * Placed after infinite loops, as the body of methods which are never called (but, for example, remain
	  * for binary compatibility), or methods of sealed classes which are overriden by subclasses and similar
	  * circumstances.
	  */
	def ??! :Nothing = throw new ThisShouldBeImpossibleException



	@inline final def unsupported_! :Nothing =
		throw new UnsupportedOperationException

	@inline final def unsupported_!(msg :String) :Nothing =
		throw new UnsupportedOperationException(msg)

	@inline final def noneSuch_!(msg :String) :Nothing =
		throw new NoSuchElementException(msg)

	@inline final def illegal_!(msg :String) :Nothing =
		throw new IllegalArgumentException(msg)

//	@inline final def override_! :Nothing =



	final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (String) or (String, Throwable).",
				ex
			)
		}).get

	final def raise[E <: Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance("").asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance("", null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of q guard failure: no constructor (), (String) or (String, Throwable).",
				ex
			)
		}).get

}
