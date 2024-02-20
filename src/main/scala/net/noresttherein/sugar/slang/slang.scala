package net.noresttherein.sugar




package object slang {
	private[slang] final val Ver = 1L
}


package slang {
	/** A very simple serialization proxy, that is one intended to be returned from a `Serializable` class's
	  * `writeReplace` method. Consists of the serializable object `substitute` to be written to the stream
	  * instead of the proxy's creator, and a factory function `reconstruct` used upon deserialization,
	  * to build an object of the creator's type.
	  */
	@SerialVersionUID(Ver)
	class SerializationProxy[T <: Serializable](substitute: T, reconstruct: T => Any) extends Serializable {
		private def readResolve :Any = reconstruct(substitute)
		override def toString :String = substitute.toString
	}

	/** A serialization proxy, that is an object intended to be returned from a `Serializable` class's
	  * `writeReplace` method, for globally reachable values (presumably through a stable path).
	  * Upon deserialization, it returns the value of a lazily evaluated expression (which should not involve a closure)
	  * from its `readResolve` method, in order for the former to be returned to the application
	  * as the deserialized value. `Scala`'s singleton objects have this feature built in, although inheriting
	  * a `writeReplace` method (for example, from [[scala.collection.generic.DefaultSerializable DefaultSerializable]],
	  * will overwrite that behaviour. It may be however useful for values which are not Scala's singleton object,
	  * but arbitrary constant values - typically of an anonymous class extending an interface class, in order
	  * to expose in an API solely a value of the interface class, rather than an object, to make any future
	  * changes easier.
	  * @example {{{
	  *     object WebPage1995 {
	  *         final val VisitorCount :AtomicInteger = new AtomicInteger {
	  *             private def readResolve :AnyRef = new SingletonSerializationProxy
	  *         }
	  *     }
	  * }}}
	  * @param fetch a lazy expression returning an object which should be unique in the application.
	  */
	@SerialVersionUID(Ver)
	class SingletonSerializationProxy(fetch: => Any) extends Serializable {
		private def readResolve :Any = fetch
		override def toString :String = fetch.toString
	}
}
