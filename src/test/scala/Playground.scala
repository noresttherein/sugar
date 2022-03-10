import net.noresttherein.slang.{implicits, numeric}
import net.noresttherein.slang.time.{DateTime, Timestamp, ZoneDateTime}
import net.noresttherein.slang.time.dsl.IntTimeLapseMethods


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {

}

class ValueClass[T](val value :T) extends AnyVal

class SpecializedClass[@specialized T](val value :ValueClass[T]) {
	def feedTo(f :ValueClass[T] => ValueClass[T]) = f(value)
}



