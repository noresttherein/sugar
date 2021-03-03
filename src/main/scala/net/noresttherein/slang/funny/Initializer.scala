package net.noresttherein.slang.funny

import net.noresttherein.slang.funny.fun.specializations.ReturnVal



/**
  * @author Marcin Mościcki
  */
trait Initializer[@specialized(ReturnVal) +Y] extends (() => Y) with Serializable



object Initializer {
	@inline def apply[@specialized(ReturnVal) Y](f :Initializer[Y]) :Initializer[Y] = f

	@inline def name[@specialized(ReturnVal) Y](name :String)(f :NamedInitializer[Y]) :NamedInitializer[Y] = f

	abstract class NamedInitializer[@specialized(ReturnVal) +Y](init :String) extends Initializer[Y] {
		@volatile private[funny] var name = init
		override def toString :String = name
	}
}
