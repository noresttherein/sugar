package net.noresttherein.sugar.prettyprint

/** Provides alternative string representations of `Boolean` values in the form of ''yes/no'' or ''y/n'' (default).
  * Makes for shorter `toString` results in classes containing several `Boolean` fields.
  */
@SerialVersionUID(ver)
class YesNo(val toBoolean :Boolean) extends AnyVal with Serializable {
	/** Converts this boolean to `0` or `1`. */
	def toBit :Int = if (toBoolean) 1 else 0

	/** Returns either `"yes"` or `"no"`, depending on whether this `Boolean` is true. */
	def yesNo :String = if (toBoolean) "yes" else "no"

	/** Returns either `"y"` or `"n"`, depending on whether this `Boolean` is true. */
	def yn :String = if (toBoolean) "y" else "n"

	/** Returns either `"Y"` or `"N"`, depending on whether this `Boolean` is true. */
	def YN :String = if (toBoolean) "Y" else "N"

	/** Returns `"t"` if true and `"f"` otherwise */
	def tf :String = if (toBoolean) "t" else "f"

	/** Returns `"T"` if true and `"F"` otherwise. */
	def TF :String = if (toBoolean) "T" else "F"

	/** Returns `"y"` if this `Boolean` is true or `"n"` otherwise. */
	override def toString :String = yn
}


/** Patches `Boolean` values to print ''yes'' or ''no''. */
@SerialVersionUID(ver)
object YesNo {
	def apply(is :Boolean) = new YesNo(is)

	@inline final val Yes = new YesNo(true)
	@inline final val No = new YesNo(false)

	/** Implicit conversion from `Boolean`  to `YesNo`, enriching the former with a `"y"/"n"` `toString` implementation. */
	implicit def shorthandBoolean(boolean :Boolean) :YesNo = new YesNo(boolean)

	/** Implicit conversoin from `YesNo` back to `Boolean` */
	implicit def isYes(yn :YesNo) :Boolean = yn.toBoolean
}
