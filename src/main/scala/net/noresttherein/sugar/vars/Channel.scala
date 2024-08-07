package net.noresttherein.sugar.vars

import scala.annotation.tailrec

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Channel.{Reader, Writer}
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.undefined




/** A synchronized channel for passing individual values of type `T` between producers and consumers.
  * Every call to [[net.noresttherein.sugar.vars.Channel.value_= value_=]] 'setting' the value of this channel
  * is paired with another thread's call to [[net.noresttherein.sugar.vars.Channel.get get]] and the value
  * provided by the writer in the setter is handed over to the reader in the getter. Read access will block
  * if no producers are currently waiting on this channel and, similarly, setter calls will also block
  * if there are no readers ready to consume the value. In that sense, this `Ref` is always empty, as it stores
  * no value until read. Both readers and writers are processed on a first come, first served basis.
  *
  * Synchronization happens under the monitor of this instance; it is therefore possible for client code
  * to perform more complex operations atomically by locking it manually.
  *
  * `Channel` redefines equality as referential equality: no instance will equal any other `Ref` instance.
  * @define Ref `Channel`
  * @define ref channel
  * @author Marcin Mościcki
  */ //not a Serializable
sealed class Channel[@specialized(SpecializedVars) T] private[vars] () extends InOut[T] {
	//invariant: readers == null || writers == null
	//member fields modified only while holding the monitor for this object
	private[this] var writers    :Writer[T] = _ //the writers queue
	private[this] var lastWriter :Writer[T] = _ //the last element in the writers queue
	private[this] var readers    :Reader[T] = _ //the readers queue
	private[this] var lastReader :Reader[T] = _ //the last element in the readers queue

	/** Returns `false`. */
	override def isFinal :Boolean = false

	/** Returns `!`[[net.noresttherein.sugar.vars.Channel.isDefinite isDefinite]]. */
	override def isEmpty   :Boolean = !isDefinite

	/** Returns `false`. */
	override def isFinalizable :Boolean = false

	/** Returns `false`. */
	override def isConst :Boolean = false

	/** Returns `true` (`get` never throws a [[NoSuchElementException]]). */
	override def isDefined :Boolean = true

	/** Checks if there are any writing threads waiting on this relay port. The following code won't block:
	  * {{{
	  *     val maybeGet = relay.synchronized {
	  *         if (relay.isDefinite) Some(relay.get)
	  *         else None
	  *     }
	  * }}}
	  */
	override def isDefinite :Boolean = synchronized { writers != null }


	/** Synchronously returns a value some other thread 'assigns' to this variable, with every call
	  * returning a different value (that is, a value set by a separate assignment operations).
	  * If writers (callers to [[net.noresttherein.sugar.vars.Channel.value_= value_=]]) are waiting,
	  * it wakes the first one and return the value of its argument. Otherwise the call blocks, putting
	  * this thread in line after any other readers waiting on this variable, awaiting the `n`-th writer to come,
	  * where `n` is the size of the reader queue (including the calling thread).
	  */
	override def get :T = {
		val actor = synchronized {
			if (writers != null) {  //writers are waiting - dequeue one
				val writer = writers
				writers = writers.next
				if (writers == null)
					lastWriter = null
				writer
			} else {  //enqueue a new Reader to wait on in the next step; writers == null so the invariant holds.
				val reader = new Reader[T]()
				if (readers == null) {
					readers = reader
					lastReader = reader
				} else {
					lastReader.next = reader
					lastReader = reader
				}
				reader
			}
		}
		actor match {
			case writer :Writer[T] => writer.synchronized {
				//signal the associated writing thread to skip waiting in case we consume the value before
				//the writer entered the writer.synchronized block
				writer.cleared = true
				writer.notify()
				writer.value
			}
			case reader :Reader[T] => reader.synchronized {
				//It's possible a writer came and paired itself with us before we reached this critical section.
				if (!reader.ready)
					reader.wait()
				reader.value
			}
		}
	}

	/** Returns the value set by the first writer in the queue for this `Channel`. This method does not block:
	  * if no writers are present, it throws a [[NoSuchElementException]].
	  * @see [[net.noresttherein.sugar.vars.Channel.get]]
	  */
	override def value :T = {
		val actor = synchronized {
			if (writers == null)
				noSuch_!("No writers queued on " + this + ".")
			//writers are waiting - dequeue one
			val writer = writers
			writers = writers.next
			if (writers == null)
				lastWriter = null
			writer
		}
		actor.synchronized {
			actor.cleared = true
			actor.notify()
			actor.value
		}
	}

	/** Synchronously passes over the value of the argument to another thread calling the corresponding
	  * [[net.noresttherein.sugar.vars.Channel.value getter]] of this property. If readers (threads reading the value
	  * of this variable) are waiting, it wakes the first one and passes over `newValue` so the latter may return it.
	  * Otherwise the call blocks, putting the thread in line after any other waiting writers, awaiting the `n`-th
	  * reader to come, where `n` is the total number of writers waiting.
	  */
	override def value_=(newValue :T) :Unit = {
		val actor = synchronized {
			if (readers != null) {
				val reader = readers
				readers = readers.next
				if (readers == null)
					lastReader = null
				reader
			} else {
				val writer = new Writer(newValue)
				if (writers == null) {
					writers = writer
					lastWriter = writer
				} else {
					lastWriter.next = writer
					lastWriter = writer
				}
				writer
			}
		}
		actor match {
			case reader :Reader[T] => reader.synchronized {
				reader.value = newValue
				reader.ready = true
				reader.notify()
			}
			case writer :Writer[T] => writer.synchronized {
				//wait only if a reader didn't race and consume our value before we reached this synchronized block
				if (!writer.cleared)
					writer.wait()
			}
		}
	}

	/** Throws an [[UnsupportedOperationException]]. */
	override def const :T = unsupported_!("Channel.const")


	/** Consumes a value if any producers are waiting on this instance. */
	override def opt :Opt[T] = synchronized {
		if (writers != null) One(value) else None
	}
	override def toOpt :Opt[T] = opt
	override def constOpt :Opt[T] = None

	/** If a writer is waiting on the other side of the channel, returns the set value,
	  * but without consuming it and freeing the writer. If no writer is waiting on this $ref, returns `None`.
	  */
	def peek :Opt[T] = synchronized {
		if (writers != null) One(writers.value) else None
	}

	/** The number of waiting writers. Note that, unless externally synchronized by the application,
	  * the value may be already stale when the method returns.
	  */
	def waitingWriters :Int = synchronized {
		if (writers == null) 0 else writers.length
	}

	/** The number of waiting readers. Note that, unless externally synchronized by the application,
	  * the value may be already stale when the method returns.
	  */
	def waitingReaders :Int = synchronized {
		if (readers == null) 0 else readers.length
	}

	private[vars] override def isSpecialized :Boolean = getClass != classOf[Channel[_]]

	override def equals(that :Any) :Boolean = this eq that.asInstanceOf[AnyRef]
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Channel[_]]
	override def hashCode :Int = System.identityHashCode(this)

	override def mkString :String = mkString("Channel")
	override def mkString(prefix :String) :String = synchronized {
		if (writers == null) prefix + "()"
		else prefix + "(" + writers.value + ")"
	}
	override def toString :String = synchronized {
		if (writers != null) String.valueOf(writers.value) else undefined.toString
//		if (writers != null) writers.writersString("Channel") + "@" + this.hashCodeString
//		else if (readers != null) "Channel"  + "(" + readers.length + " readers)@" + this.hashCodeString
//		else "Channel()@" + this.hashCodeString
	}
}




@SerialVersionUID(Ver)
case object Channel {
	/** Creates a new synchronization channel for passing values of type `T` between threads. */
	def apply[@specialized(SpecializedVars) T] :Channel[T] = new Channel[T]


	/** A waiting post for readers and writers, with two subclasses:
	  * [[net.noresttherein.sugar.vars.Channel.Reader Reader]] and [[net.noresttherein.sugar.vars.Channel.Writer Writer]].
	  */
	private sealed trait Actor[T]

	/** A queue of waiting posts on which reader threads (callers of `Channel.value`) wait until writers show up.
	  * If the `writers` queue is empty, a reader thread enqueues a new instance of `Reader` and waits on it
	  * for a writer to show up and dequeue it.
	  * Fields are modified only while holding the monitor for this instance.
	  */
	private class Reader[T] extends Actor[T] {
		/** The value passed to the associated reader thread by some writer.
		  *  Modified only when holding the monitor for this instance.
		  */
		var value :T = _
		/** A flag signaling that `value` is initialized with an argument to the setter method
		  *  called by a writer. Modified only when holding the monitor for this instance.
		  */
		var ready :Boolean = false
		/** The next reader in the queue. Modified only when holding the monitor for the owning `Channel`. */
		var next :Reader[T] = _

		def length :Int = {
			@tailrec def rec(reader :Reader[T], acc :Int) :Int = {
				val next = reader.synchronized(reader.next)
				if (next  == null) acc + 1
				else rec(next, acc + 1)
			}
			rec(this, 0)
		}

		override def toString :String = synchronized {
			if (ready) "Reader(" + value + ")@" + this.shortHashString
			else "Reader(_)@" + this.shortHashString
		}

		def readersString(prefix :String = "Readers") :String = {
			def format(reader :Reader[T], res :JStringBuilder) = {
				val suffix = if (reader.next == null) ")" else ", "
				if (reader.ready)
					res append "(" append value append ")@" append reader.shortHashString append suffix
				else
					res append "(_)@" append reader.shortHashString append suffix
			}

			@tailrec def rec(reader :Reader[T], res :JStringBuilder) :String = {
				val next = reader.synchronized {
					format(reader, res)
					reader.next
				}
				if (next != null)
					rec(next, res)
				else
					res.toString
			}
			rec(this, new JStringBuilder(prefix) append ')')
		}
	}

	/** A queue of value providers for `Channel`. On each link a writer (caller of `Channel.value_=`) is waiting.
	  * Writers are notified after a successful pairing with the reader when the relay is considered over.
	  * @param value   The value written by the writing thread which created this instance.
	  * @param cleared A flag set by a reader thread dequeueing this instance to signal that relay is over.
	  *                This is done to prevent race conditions, with writer threads waiting on this instance
	  *                only if the flag is still false. Modified only when holding the monitor for this instance.
	  * @param next    The next writer in the queue. Modified only when holding the monitor for the owning `Channel`.
	  */
	private class Writer[@specialized(SpecializedVars) T]
	                    (val value :T, var cleared :Boolean = false, var next :Writer[T] = null)
		extends Actor[T]
	{
		def length :Int = if (next == null) 1 else next.length + 1

		override def toString :String = synchronized {
			if (cleared) "Writer()@" + this.shortHashString else "Writer(" + value + ")@" + this.shortHashString
		}

		def writersString(prefix :String = "Writers") :String = {
			def format(writer :Writer[T], res :JStringBuilder) = {
				val suffix = if (writer.next == null) ")" else ", "
				if (writer.cleared)
					res append "()@" append writer.shortHashString append suffix
				else
					res append '(' append writer.value append ")@" append writer.shortHashString append suffix
			}

			@tailrec def rec(writer :Writer[T], res :JStringBuilder) :String = {
				val next = writer.synchronized {
					format(writer, res); writer.next
				}
				if (next != null)
					rec(writer, res)
				else
					res.toString
			}
			rec(this, new JStringBuilder(prefix) append '(')
		}
	}
}
