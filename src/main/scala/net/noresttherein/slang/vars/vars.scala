package net.noresttherein.slang

/**
  * @author Marcin Mościcki
  */
package object vars {
	type atomic[T] = Atomic[T]
	val  atomic    = Atomic

	type export[T] = Export[T]
	val  export    = Export

	type freezer[T] = Freezer[T]
	val  freezer    = Freezer

	type synchronized[T] = SyncVar[T]
	val  synchronized    = SyncVar

	type variable[T] = Var[T]
	val  variable    = Var

//	type volatile[+T] = Volatile[T]
//	val  volatile     = Volatile

	type deferred[+T] = Lazy[T]
	val  deferred     = Lazy
}
