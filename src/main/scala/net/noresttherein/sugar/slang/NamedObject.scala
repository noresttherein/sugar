package net.noresttherein.sugar.slang



/** A base class which overrides `toString` with a constant of the specified value.
  * The actual constant is a private field and does not override `toString` with a `val` to allow later mixing in
  * of traits which also override `toSting` with a `def`.
  * @see [[net.noresttherein.sugar.slang.LazilyNamedObject]]
  * @see [[net.noresttherein.sugar.slang.ObjectByName]]
  */
class NamedObject(name :String) {
	override def toString :String = name
}


/** A base class which overrides `toString` with a lazy constant of the specified value.
  * The argument is executed at most once and reused on repeated calls. Override is done with a `def`
  * delegating to a private field rather than a `lazy val` in order to allow later mixing in of traits
  * which also override `toString` with a `def`.
  * @see [[net.noresttherein.sugar.slang.NamedObject]]
  * @see [[net.noresttherein.sugar.slang.ObjectByName]]
  */
class LazilyNamedObject(string: => String) {
	private[this] lazy val name = toString
	override def toString :String = name
}


/** A base class which overrides `toString` with a method executing the given ''by-name'' expression.
  * The argument is executed every time `toString` is called.
  * @see [[net.noresttherein.sugar.slang.NamedObject]]
  * @see [[net.noresttherein.sugar.slang.LazilyNamedObject]]
  */
class ObjectByName(string: => String) {
	override def toString :String = string
}
