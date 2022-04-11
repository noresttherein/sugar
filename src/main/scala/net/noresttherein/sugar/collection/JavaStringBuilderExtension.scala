package net.noresttherein.sugar.collection

import net.noresttherein.sugar.JavaTypes.JStringBuilder




/** Adds Scala [[scala.collection.mutable.Growable Growable]] and [[scala.collection.mutable.Builder Builder]]
  * methods as inlined delegates to the Java [[java.lang.StringBuilder StringBuilder]].
  * While it essentially duplicates the functionality of standard Scala
  * [[scala.collection.mutable.StringBuilder StringBuilder]], the wrapper is not intended to be as an object
  * or referred to directly by the application, but rather a provider of extension methods, and returns always
  * the original `StringBuilder`. As methods are inlined, this incurs neither the penalty of creating an additional
  * object, nor of delegating individual calls, at least with default compiler optimisations on.
  */
class JavaStringBuilderExtension(private val self :JStringBuilder) extends AnyVal with IterableOnce[Char] {
	@inline def apply(index :Int) :Char = self.charAt(index)
	@inline def last :Char = self.charAt(self.length - 1)

	@inline def length_=(newLength :Int) :Unit = self.setLength(newLength)
	@inline def clear() :JStringBuilder = { self.setLength(0); self }

	@inline def +=(char :Char) :JStringBuilder = self append char
	@inline def addOne(char :Char) :JStringBuilder = self append char

	@inline def ++=(chars :String)             :JStringBuilder = self append chars
	@inline def ++=(chars :Array[Char])        :JStringBuilder = self append chars
	@inline def ++=(chars :CharSequence)       :JStringBuilder = self append chars
	@inline def ++=(chars :IterableOnce[Char]) :JStringBuilder = addAll(chars)

	@inline def addAll(chars :String)             :JStringBuilder = self append chars
	@inline def addAll(chars :Array[Char])        :JStringBuilder = self append chars
	@inline def addAll(chars :CharSequence)       :JStringBuilder = self append chars
	@inline def addAll(chars :IterableOnce[Char]) :JStringBuilder = chars match {
		case it :Iterable[_] if it.isEmpty => self
		case it :Iterable[Char]            => it foreach self.append; self
		case _                             =>
			val i = chars.iterator
			while (i.hasNext)
				self append i.next()
			self
	}


	override def iterator :Iterator[Char] = new AbstractIndexedSeqIterator[Char] {
		override def underlyingSize = self.length

		override var index :Int = 0
		override var limit :Int = self.length

		override def head = self.charAt(index)

		override def next() = { val res = self.charAt(index); index += 1; res }
	}

	@inline def result() :String = self.toString
	@inline override def toString :String = self.toString
}
