package net.noresttherein.sugar.exceptions

import java.lang.StackWalker.StackFrame
import java.lang.reflect.Method

import net.noresttherein.sugar.exceptions.StackContext.PackageName
import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.reflect.CallerFrame
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A manually created activation frame containing information about the called object, class, method and parameters.
  */
@SerialVersionUID(Ver)
class StackContext protected (val callee :Opt[AnyRef], val source :String, val method :String, val params :Seq[Any])
	extends Equals with Serializable
{
	def this(source :String, method :String, params :Seq[Any]) = this(Lack, source, method, params)
	def this(callee :AnyRef, method :String, params :Seq[Any]) =
		this(Got(callee), callee.getClass.getName, method, params)

	def this(frame :StackFrame, params :Seq[Any]) = this(frame.getClassName, frame.getMethodName, params)
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

	def apply[T](params :Any*)(block: => T) :T = apply(new StackContext(params))(block)

	def apply[T](context :StackContext)(block: => T) :T =
		try {
			stack set context::stack.get
			block
		} finally {
			stack set stack.get.tail
		}

	/** Current stack of activation frames, with the most recent being the first one. */
	def get :Seq[StackContext] = stack.get

	def unapply(ctx :StackContext) :Opt[(Opt[AnyRef], String, String, Seq[Any])] =
		Got(ctx.callee, ctx.source, ctx.method, ctx.params)

	def unapply(e :Throwable) :Opt[Seq[StackContext]] = e match {
		case sugared :SugaredThrowable => Got(sugared.context)
		case _ => Lack
	}

	private val stack :ThreadLocal[List[StackContext]] =
		new ThreadLocal[List[StackContext]] {
			override def initialValue() :List[StackContext] = Nil
		}

	private final val PackageName = getClass.getPackageName
}
