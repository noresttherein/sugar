package net.turambar.slang.prettyprint



/** Provides alternative string representations of `Boolean` values in the form of ''yes/no'' or ''y/n'' (default).
  * Makes for shorter `toString` results in classes containing several `Boolean` fields.
  */
class YesNo(val toBoolean :Boolean) extends AnyVal {
	def bit :Int = if (toBoolean) 1 else 0
	def yesno :String = if (toBoolean) "yes" else "no"
	def yn :String = if (toBoolean) "y" else "n"
	override def toString :String = yn
}


/** Patches `Boolean` values to print aa ''yes'' or ''no''. */
object YesNo {
	def apply(is :Boolean) = new YesNo(is)

	@inline final val Yes = new YesNo(true)
	@inline final val No = new YesNo(false)

	implicit def shorthandBoolean(boolean :Boolean) :YesNo = new YesNo(boolean)
	implicit def isYes(yn :YesNo) :Boolean = yn.toBoolean
}