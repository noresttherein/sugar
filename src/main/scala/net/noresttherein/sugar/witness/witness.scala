package net.noresttherein.sugar

import scala.reflect.ClassTag


package object witness {
	private[witness] final val Ver = 1L

	type MaybeClassTag[T] = Maybe[ClassTag[T]]
}
