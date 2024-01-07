package net.noresttherein.sugar

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import scala.annotation.nowarn




package object concurrent {
	@nowarn("cat=deprecation")
	@inline private[sugar] def threadId(thread :Thread) :Long = thread.getId

	@nowarn("cat=deprecation")
	@inline private[sugar] def currentThreadId :Long = Thread.currentThread.getId

	@inline private[sugar] def releaseFence() :Unit = Fences.releaseFence()

	@inline private[sugar] def acquireFence() :Unit = Fences.acquireFence()

}