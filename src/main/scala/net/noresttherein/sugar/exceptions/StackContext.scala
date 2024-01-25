package net.noresttherein.sugar.exceptions

import java.lang.StackWalker.{Option, StackFrame}
import java.lang.reflect.Method

import scala.collection.mutable.{ArrayBuffer, ArrayDeque}

import net.noresttherein.sugar.exceptions.StackContext.PackageName
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.reflect.CallerFrame
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{Yes, No}




/** A manually created activation frame containing information about the called object, class, method and parameters.
  */
@SerialVersionUID(Ver)
class StackContext protected (val callee :Maybe[AnyRef], val source :String, val method :String, val params :Seq[Any])
	extends Equals with Serializable
{
	def this(source :String, method :String, params :Seq[Any]) = this(No, source, method, params)
	def this(callee :AnyRef, method :String, params :Seq[Any]) =
		this(Yes(callee), callee.getClass.getName, method, params)

	def this(frame :StackFrame, params :Seq[Any]) = this(frame.getClassName, frame.getMethodName, params)
	def this(frame :StackTraceElement, params :Seq[Any]) = this(frame.getClassName, frame.getMethodName, params)
	def this(params :Seq[Any]) = this(CallerFrame.outside(PackageName), params)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :StackContext if other canEqual this =>
			callee == other.callee && source == other.source && method == other.method && params == other.params
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[StackContext]
	override def hashCode :Int =
		((callee.hashCode * 31 + source.hashCode) * 31 + method.hashCode) * 31 + params.hashCode

	def toStackTraceElement :StackTraceElement =
		new StackTraceElement(source, method, "<unknown>", -1)

	override def toString :String = params.mkString(source + "." + method + "(", ", ", ")")
}



//todo: instead of placing these methods here, create an extension class which will have access to our caller.
@SerialVersionUID(Ver)
object StackContext {
	//Consider: we could accept logger as an implicit parameter instead of the called class.
	def apply[T](callee :AnyRef, method :String, params :Any*)(block: => T) :T =
		apply(new StackContext(callee, method, params))(block)

	def apply[T](callee :AnyRef, method :Method, params :Any*)(block: => T) :T =
		apply(new StackContext(callee, method.toString, params))(block)

	def apply[T](callee :Class[_], method :String, params :Any*)(block: => T) :T =
		apply(new StackContext(callee.name, method, params))(block)

	def apply[T](callee :Class[_], method :Method, params :Any*)(block: => T) :T =
		apply(new StackContext(callee.name, method.toString, params))(block)

	def apply[T](source :String, method :String, params :Any*)(block: => T) :T =
		apply(new StackContext(source, method, params))(block)

	//signature conflict
//	def apply[T](params :Any*)(block: => T) :T = apply(new StackContext(params))(block)

	def apply[T](context :StackContext)(block: => T) :T =
		try {
			context +=: stack.get
			block
		} finally {
			stack.get.removeHead(false)
		}

	/** Current stack of activation frames, with the most recent being the first one. */
	def get :Seq[StackContext] = stack.get.toSeq

	def unapply(ctx :StackContext) :Maybe[(Maybe[AnyRef], String, String, Seq[Any])] =
		Yes(ctx.callee, ctx.source, ctx.method, ctx.params)

	def unapply(e :Throwable) :Maybe[Seq[StackContext]] = e match {
		case sugared :SugaredThrowable => Yes(sugared.context)
		case _ => No
	}

	private[this] val stack :ThreadLocal[ArrayDeque[StackContext]] =
		new ThreadLocal[ArrayDeque[StackContext]] {
			override def initialValue() = ArrayDeque.empty[StackContext]
		}

	private final val PackageName = getClass.getPackageName
//	private[this] final val walker = StackWalker.getInstance(Option.RETAIN_CLASS_REFERENCE)
}
