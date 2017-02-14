package net.turambar.slang

import scala.annotation.meta.field
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}


/*

sealed trait Yarn[+T] extends Any {
	def head :T = toStream.head
	def headOption :Option[T]
	def head_? :Option[T]
	
	def tail :Yarn[T] = Yarn(toStream.tail)
//	def tail_? :Option[Yarn[T]]
	def toStream :Stream[T]
	def isEmpty :Boolean = toStream.isEmpty
	def isLazy :Boolean
	
	override def toString =
		if (isLazy) "Yarn(?)"
		else toStream.addString(new StringBuilder, "Yarn(", ",", ")").toString
	
}

object Yarn {
//	type @-[+T] = Yarn[T]
	def apply[T](stream : =>Stream[T]) :Yarn[T] = new LazyStreamYarn(stream _)
	
	def lzy[T](unevaled : =>Yarn[T]) :Yarn[T] = new LazyYarn[T](unevaled)
	
	def eager[T](stream :Stream[T]) :Yarn[T] = new StreamYarn(stream)
	
	implicit def stream[T](stream : =>Stream[T]) :Yarn[T] = new LazyStreamYarn(stream _)
	
	implicit def future[T](stream :Future[Stream[T]])(implicit executor :ExecutionContext) :Yarn[T] =
		new FutureStreamYarn(stream)
	
	def future[T](block : =>Stream[T])(implicit executor :ExecutionContext) :Yarn[T] =
		future(Future(block))
	
	
	
	
	implicit def yarnAsStream[T](yarn :Yarn[T]) :Stream[T] = yarn.toStream
	
	
	class StreamYarn[+T](val toStream :Stream[T]) extends AnyVal with Yarn[T] {
		override def head = toStream.head
		override def headOption = toStream.headOption
		override def head_? = toStream.headOption
		override def tail :StreamYarn[T] = new StreamYarn(toStream.tail)
//		override def tail_? :Some[Yarn[T]] = Some(new StreamYarn(toStream.tail))
		override def isEmpty = toStream.isEmpty
		override def isLazy = false
	}
	
	
	final class LazyStreamYarn[+T](uneval : ()=>Stream[T]) extends Yarn[T] {
		@volatile private[this] var eval = uneval
		@volatile private[this] var evaled :Stream[T] = _
		
		def toStream = {
			if (isLazy) synchronized {
				if (isLazy) {
					evaled = eval()
					eval = null
				}
			}
			evaled
		}
			
		override def head = toStream.head
		override def headOption = toStream.headOption
		override def head_? =
			if (isLazy) None
			else evaled.headOption
			
		override def tail = new LazyStreamYarn(toStream.tail _)
		override def isEmpty = toStream.isEmpty
		override def isLazy = evaled==null
		
	}

	
	final class LazyYarn[+T] private[Yarn] (eval : =>Yarn[T]) extends Yarn[T] {
		@volatile private[this] var content :Any = eval _
		
		private def eager = (content : @unchecked) match {
			case yarn :Yarn[T] => yarn
			case stream :Stream[T] => new StreamYarn(stream)
			case _ => synchronized {
				(content : @unchecked) match {
					case unevaled :(() => Yarn[T] @unchecked) =>
						val yarn = unevaled()
						content = yarn :Yarn[T]
						yarn
					case yarn :Yarn[T] => yarn
					case stream :Stream[T] => new StreamYarn(stream)
				}
			}
				
			
		}
		
		override def isLazy = (content : @unchecked) match {
			case _ :Stream[_] => false
			case yarn :Yarn[T] => yarn.isLazy
			case _ => synchronized {
				(content : @unchecked) match {
					case unevaled :(() => Yarn[T] @unchecked) =>
						val yarn = unevaled()
						content = yarn
						yarn.isLazy
					case yarn :Yarn[_] => yarn.isLazy
					case _ => false
				}
			}
		}
		
		override def isEmpty = (content : @unchecked) match {
			case stream :Stream[_] => stream.isEmpty
			case yarn :Yarn[_] => yarn.isEmpty
			case _ => synchronized {
				(content : @unchecked) match {
					case uneval :((()=>Yarn[_]) @unchecked) =>
						val yarn = uneval()
						content = yarn
						yarn.isEmpty
					case yarn :Yarn[_] => yarn.isEmpty
					case stream :Stream[_] => stream.isEmpty
				}
			}
		}
		
		override def toStream = (content : @unchecked) match {
			case stream :Stream[T] => stream
			case _ => synchronized {
				(content : @unchecked) match {
					case unevaled :(() => Yarn[T] @unchecked) =>
						val stream = unevaled().toStream
						content = stream
						stream
					case yarn :Yarn[T] =>
						val stream = yarn.toStream
						content = stream
						stream
					case stream :Stream[T] => stream
				}
			}
		}
		
		override def head = (content : @unchecked) match {
			case stream :Stream[T] => stream.head
			case yarn :Yarn[T] => yarn.head
			case _ => synchronized {
				content match {
					case uneval :(()=>Yarn[T] @unchecked) =>
						val yarn = uneval()
						content = yarn
						yarn.head
					case yarn :Yarn[T] => yarn.head
					case stream :Stream[T] => stream.head
				}
			}
		}
		
		override def headOption = (content : @unchecked) match {
			case stream :Stream[T] => stream.headOption
			case yarn :Yarn[T] => yarn.headOption
			case _ => synchronized {
				content match {
					case eval :(()=>Yarn[T] @unchecked) =>
						val yarn = eval()
						content = yarn
						yarn.headOption
					case yarn :Yarn[T] => yarn.headOption
					case stream :Stream[T] => stream.headOption
				}
			}
		}
		
		override def head_? = (content : @unchecked) match {
			case stream :Stream[T] => stream.headOption
			case yarn :Yarn[T] => yarn.head_?
			case _ => None
		}
		
		override def tail = (content : @unchecked) match {
			case stream :Stream[T] => stream.tail
			case yarn :Yarn[T] => yarn.tail
			case _ => synchronized {
				content match {
					case eval :(()=>Yarn[T] @unchecked) =>
						val yarn = eval()
						content = yarn
						yarn.tail
					case yarn :Yarn[T] => yarn.tail
					case stream :Stream[T] => stream.tail
				}
			}
		}
		
		override def toString = (content : @unchecked) match {
			case stream :Stream[_] => stream.addString(new StringBuilder, "Yarn(", ", ", ")").toString
			case yarn :Yarn[_] => yarn.toString
			case _ => "Yarn(?)"
		}
	}
	
	
	final class FutureStreamYarn[+T](private val future :Future[Stream[T]])(implicit executor :ExecutionContext) extends Yarn[T] {
		def toStream = Await.result(future, Duration.Inf)
		override def isLazy: Boolean = !future.isCompleted
		
		override def head = toStream.head
		
		override def headOption = toStream.headOption
		
		override def head_? = future.value match {
			case Some(Success(stream)) => stream.headOption
			case Some(Failure(exc)) => throw exc
			case _ => None
		}
			
		override def tail =
			if (!isLazy) new StreamYarn(toStream)
			else new FutureStreamYarn(future map { _.tail })
		
		override def isEmpty = toStream.isEmpty
		
		override def toString = future.value match {
			case Some(Success(stream)) => stream.addString(new StringBuilder, "Yarn(", ", ", ")").toString
			case Some(Failure(exc)) => s"Yarn<$exc>"
			case _ => "Yarn(?)"
		}
	}
	
	
	final class FutureYarn[+T](private val future :Future[Yarn[T]])(implicit executor :ExecutionContext) extends Yarn[T] {
		private def eager = Await.result(future, Duration.Inf)
		def toStream = eager.toStream
		
		override def isLazy = future.value match {
			case None => true
			case Some(Success(yarn)) => yarn.isLazy
			case _ => false
		}
		
		override def isEmpty = eager.isEmpty
		
		override def head = eager.head
		
		override def head_? = future.value match {
			case Some(Success(yarn)) => yarn.head_?
			case Some(Failure(exc)) => throw exc
			case _ => None
		}
		
		override def headOption = eager.headOption
		
		override def tail = eager.tail
		
		override def toString = future.value match {
			case Some(Success(yarn)) => yarn.toString
			case Some(Failure(exc)) => s"Yarn<$exc>"
			case None => "Yarn(?)"
		}
	}
	
	def empty[T] :Yarn[T] = *-
	
	
	object *- extends Yarn[Nothing] {
		override def head = throw new NoSuchElementException(s"Empty yarn")
		override def headOption = None
		override def head_? = None
		override def tail = throw new NoSuchElementException(s"Empty yarn")
		def toStream = Stream.Empty
		override def isEmpty = true
		override def isLazy = false
		override def toString = "Yarn()"
	}
	
	
}
*/
