package net.noresttherein.sugar






/** A namespace some non-implicit methods which are used by this library, but can also be useful in the client code.
  * Extracted into a trait so that users can extend their package objects from it, bringing everything into
  * the lexical scope of classes located therein. Extended by the `sugar` package object.
  * @author Marcin Mościcki
  */
trait imports extends exceptions.imports with optional.imports {
	//todo: i.++  ++.i macros
}
