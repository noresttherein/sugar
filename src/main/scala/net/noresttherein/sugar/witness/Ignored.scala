package net.noresttherein.sugar.witness




/** A dummy class for which an implicit value always exists. Taking an implicit parameter of this type allows
  * to avoid conflicts between methods which would otherwise have the same erasure.
  * If a conflict exists between multiple methods, one can, in addition, use classes 
  * [[net.noresttherein.sugar.witness.Ignored1 Ignored1]], [[net.noresttherein.sugar.witness.Ignored2, Ignored2]], etc.
  * in the same way.
  * @author Marcin Mościcki
  */ //consider: other names - Irrelevant, Variant1, Variant2, etc.
final class Ignored private[witness]


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
final class Ignored1 private[witness]


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
final class Ignored2 private[witness]


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
final class Ignored3 private[witness]


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
final class Ignored4 private[witness]


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
final class Ignored5 private[witness]


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
final class Ignored6 private[witness]


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
final class Ignored7 private[witness]


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
final class Ignored8 private[witness]


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
final class Ignored9 private[witness]






final class Overload[+X] private[witness]


final class Overload1[+X] private[witness]

final class Overload2[+X] private[witness]

final class Overload3[+X] private[witness]

final class Overload4[+X] private[witness]

final class Overload5[+X] private[witness]

final class Overload6[+X] private[witness]

final class Overload7[+X] private[witness]

final class Overload8[+X] private[witness]

final class Overload9[+X] private[witness]
