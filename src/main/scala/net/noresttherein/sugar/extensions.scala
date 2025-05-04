package net.noresttherein.sugar

import scala.annotation.nowarn

import net.noresttherein.sugar.extensions.boxeqMethod
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
	   with funny.extensions with io.extensions with matching.extensions with numeric.extensions
	   with optional.extensions with reflect.extensions with repeat.extensions with slang.extensions
	   with time.extensions[Rank1] with tuples.extensions with typist.extensions with vars.extensions
	   with witness.extensions
{
	/** Adds [[net.noresttherein.sugar.extensions.boxeqMethod.boxeq boxeq]] method which compares
	  * any two values for equality using either `==` or `eq`, depending on whether they are value types
	  * (or their box class instances) or other, 'normal' objects - instances of AnyRef or application value classes.
	  */
	@inline implicit final def boxeqMethod[X](self :X) :boxeqMethod[X] = new boxeqMethod(self)
}




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
object extensions extends extensions {
	class boxeqMethod[X] private[extensions] (private val self :X) extends AnyVal {
		/** Compares the two values for equality if they are built in value types or their boxes,
		  * or referential equality (`eq`) if they are true `AnyRef` types.
		  * The former comparison is made using `equals`, not `==`, so
		  * {{{ !(1.asInstanceOf[AnyRef] boxeq 1L.asInstanceOf[AnyRef]), }}} but
		  * {{{ 1 == 1L }}}
		  * If either of the values is `null`, both must be `null`.
		  *
		  * Note that custom value classes are still compared for referential equality of their object representations,
		  * not the equality of the underlying field.
		  */
		def boxeq[Y](other :Y) :Boolean = self match {
			case null => other == null
			case _ :Number => self match {
				case _ :Int | _ :Long | _ :Short | _ :Byte | _ :Double | _ :Float => self equals other : @nowarn
				case _ => self.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]
			}
			case _ :Char | _ :Boolean => self == other
			case _                    => self.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]
		}
	}
}
