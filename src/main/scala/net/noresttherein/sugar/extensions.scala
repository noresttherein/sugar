package net.noresttherein.sugar

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import net.noresttherein.sugar.typist.casting
import net.noresttherein.sugar.typist.Rank.Rank1




/** Type aliases and forwarders for most useful types and implicit conversions in the library providing new syntax.
  * They can be imported into user code either by an explicit import of the contents of the companion object
  * [[net.noresttherein.sugar.extensions$ extensions]] (which extends this trait), or by having a package object
  * containing classes using these features extend this trait. This is of course true also for any other object or class.
  * As a mnemonic, class and method members here are (re)named after the most prominent declared method.
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait extensions
	extends arrays.extensions with casting.extensions with collections.extensions with exceptions.extensions
	   with funny.extensions with matching.extensions with numeric.extensions with optional.extensions
	   with reflect.extensions with repeat.extensions with slang.extensions with time.extensions[Rank1]
	   with tuples.extensions with typist.extensions with witness.extensions




/** (Almost) all extension methods and implicit conversions providing new syntax in this library .
  * Grouping them here not only allows a wildcard import, but also makes single imports stand out due to object name.
  *
  * As a mnemonic, class and method members here are named either
  *   - `xxxExtension`, where `Xxx` is the name of the enriched type, if it is they are not general purpose methods,
  *     but work on specific types such as [[Option]] or [[Iterable]], or
  *   - `xxxMethod`/`xxxMethods`, where `xxx` is the name of the most prominent declared method.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
object extensions extends extensions
