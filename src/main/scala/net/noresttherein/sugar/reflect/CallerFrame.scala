package net.noresttherein.sugar.reflect

import java.lang.StackWalker.StackFrame




object CallerFrame {
	private[this] val packagePrefix = classOf[CallerFrame.type].getPackageName + "."

	def apply() :StackFrame = StackWalker.getInstance.walk { frames =>
		val it = frames.iterator
		var frame :StackFrame = null
		while (it.hasNext) {
			frame = it.next
			val className = frame.getClassName
			if (!className.startsWith(packagePrefix))
				return frame
		}
		frame
	}

	def outside(packageName :String) :StackFrame = StackWalker.getInstance.walk { frames =>
		val it = frames.iterator
		var frame :StackFrame = null
		val prefix =
			if (packageName == null || packageName.length == 0) null
			else if (packageName(packageName.length - 1) == '.') packageName
			else packageName + '.'
		while (it.hasNext && {
			frame = it.next()
			val className = frame.getClassName
			className.startsWith(packagePrefix)
		}) {}
		if (prefix != null)
			while (it.hasNext && frame.getClassName.startsWith(prefix))
				frame = it.next()
		frame
	}

	def outside(packageNames :Seq[String]) :StackFrame = StackWalker.getInstance.walk { frames =>
		val it = frames.iterator
		var frame :StackFrame = null
		val prefixes = packageNames.map(pckg => if (pckg.endsWith(".")) pckg else pckg + '.')
		while (it.hasNext && {
			frame = it.next()
			val className = frame.getClassName
			className.startsWith(packagePrefix)
		}) {}
		while (it.hasNext && prefixes.exists(frame.getClassName.startsWith))
			frame = it.next()
		frame
	}

}
