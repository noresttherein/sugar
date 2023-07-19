package net.noresttherein.sugar.witness




/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure.
  * If a conflict exists between multiple methods, one can, in addition, use classes 
  * [[net.noresttherein.sugar.witness.Ignored1 Ignored1]], [[net.noresttherein.sugar.witness.Ignored2, Ignored2]], etc.
  * in the same way.
  * @author Marcin Mościcki
  */
final class Ignored private ()

object Ignored {
	implicit val value :Ignored = new Ignored
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored2) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored1 private ()

object Ignored1 {
	implicit val value :Ignored1 = new Ignored1
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored2) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored2 private ()

object Ignored2 {
	implicit val value :Ignored2 = new Ignored2
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored2) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored3 private ()

object Ignored3 {
	implicit val value :Ignored3 = new Ignored3
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored2) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored4 private ()

object Ignored4 {
	implicit val value :Ignored4 = new Ignored4
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored2) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored5 private ()

object Ignored5 {
	implicit val value :Ignored5 = new Ignored5
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored6) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored6 private ()

object Ignored6 {
	implicit val value :Ignored6 = new Ignored6
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored7) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored7 private ()

object Ignored7 {
	implicit val value :Ignored7 = new Ignored7
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored8) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored8 private ()

object Ignored8 {
	implicit val value :Ignored8 = new Ignored8
}


/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure. Example:
  * {{{
  *     class Nat(val toLong :Long) {
  *         def *(x: => Nat) :Nat = if (toLong == 0) this else new Not(toLong * x.toLong)
  *         def *(x: => Long)(implicit p :Ignored) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Int)(implicit p :Ignored1) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *         def *(x: => Short)(implicit p :Ignored9) :Nat = if (toLong == 0) this else new Nat(toLong * x)
  *     }
  * }}}
  */
final class Ignored9 private ()

object Ignored9 {
	implicit val value :Ignored9 = new Ignored9
}
