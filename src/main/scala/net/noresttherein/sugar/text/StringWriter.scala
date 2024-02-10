package net.noresttherein.sugar.text

import java.io.Writer

import net.noresttherein.sugar.JavaTypes.JStringBuilder




/** Similar to [[java.io.StringWriter]], but uses a non thread-safe [[StringBuilder]],
  * rather than synchronized [[java.lang.StringBuffer]].
  */ //Consider: Implementing it in Java. This way we wouldn't have to expose the primary constructor outside the package.
@SerialVersionUID(Ver)
sealed class StringWriter protected (private[this] var scala :StringBuilder, java :JStringBuilder)
	extends Writer with Serializable
{
	def this(builder :StringBuilder) = this(builder, null)
	def this(builder :JStringBuilder) = this(null, builder)
	def this(capacity :Int) =  this(null, new JStringBuilder(capacity))
	def this() = this(null, new JStringBuilder)

	private[this] val jbuilder = if (java != null) java else scala.underlying
	lock = jbuilder

	/** The builder used by this writer. */
	def builder :StringBuilder = lock.synchronized {
		if (scala == null)
			scala = new StringBuilder(java)
		scala
	}
	/** The `String` written so far. */
	def output :String = lock.synchronized(jbuilder.toString)

	override def write(cbuf :Array[Char], off :Int, len :Int) :Unit = jbuilder.append(cbuf, off, len)
	override def write(cbuf :Array[Char]) :Unit = jbuilder.append(cbuf)
	override def write(str :String, off :Int, len :Int) :Unit = jbuilder.append(str, off, len)
	override def write(str :String) :Unit = jbuilder.append(str)
	override def write(c :Int) :Unit = jbuilder.append(c.toChar)

	override def append(csq :CharSequence) :StringWriter = { jbuilder.append(csq); this }
	override def append(csq :CharSequence, start :Int, end :Int) :StringWriter = lock.synchronized {
		jbuilder.append(csq, start, end); this
	}
	override def append(c :Char) :StringWriter = { jbuilder.append(c); this }

	override def flush() :Unit = ()
	override def close() :Unit = ()
}


@SerialVersionUID(Ver)
final class ConcurrentStringWriter private (scala :StringBuilder, java :JStringBuilder)
	extends StringWriter(scala, java)
{
	def this(builder :StringBuilder) = this(builder, null)
	def this(builder :JStringBuilder) = this(null, builder)
	def this(capacity :Int) =  this(null, new JStringBuilder(capacity))
	def this() = this(null, new JStringBuilder)

	lock = if (java != null) java else scala.underlying

	override def write(cbuf :Array[Char], off :Int, len :Int) :Unit = lock.synchronized(super.write(cbuf, off, len))
	override def write(cbuf :Array[Char]) :Unit = lock.synchronized(super.write(cbuf))
	override def write(str :String, off :Int, len :Int) :Unit = lock.synchronized(super.write(str, off, len))
	override def write(str :String) :Unit = lock.synchronized(super.write(str))
	override def write(c :Int) :Unit = lock.synchronized(super.write(c.toChar))

	override def append(csq :CharSequence) :StringWriter = lock.synchronized(super.append(csq))
	override def append(csq :CharSequence, start :Int, end :Int) :StringWriter = lock.synchronized {
		super.append(csq, start, end)
	}
	override def append(c :Char) :StringWriter = lock.synchronized { super.append(c) }
}
