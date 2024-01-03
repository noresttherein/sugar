package net.noresttherein.sugar

import scala.annotation.nowarn




package object concurrent {
	@nowarn("cat=deprecation")
	@inline private[sugar] def threadId(thread :Thread) :Long = thread.getId
	@nowarn("cat=deprecation")
	@inline private[sugar] def currentThreadId :Long = Thread.currentThread.getId
}
