package net.noresttherein.sugar.prettyprint

import scala.reflect.runtime.universe.TypeTag




/** Base class providing a `toString` implementation listing the values of all fields
  * of extending class `Self` with their names.
  */
class DefToString[Self <: DefToString[Self] : TypeTag] { this: Self =>
	override def toString :String = (this: Self).gettersString
}


/** Base class providing a `toString` implementation listing the values of all case class fields
  * of extending case class `Self` with their names.
  */
class CaseClass[Self <: CaseClass[Self] :TypeTag] extends Serializable { this :Self =>
	override def toString :String = (this :Self).caseFieldsString
}


