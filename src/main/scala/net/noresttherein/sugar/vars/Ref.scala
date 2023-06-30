package net.noresttherein.sugar.vars

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.Got
import net.noresttherein.sugar.vars.Ref.undefined




/** The root type of a class hierarchy of various value wrappers such as
  * [[net.noresttherein.sugar.vars.InOut variable references]], [[net.noresttherein.sugar.vars.Lazy lazy values]],
  * [[net.noresttherein.sugar.vars.Unsure various]] [[net.noresttherein.sugar.vars.Opt optional]]
  * [[net.noresttherein.sugar.vars.LazyUnsure value]] implementations
  * and [[java.lang.ref.Reference]] [[net.noresttherein.sugar.vars.DisposableRef wrappers]] among others.
  *
  * A `Ref` can be mutable or immutable and possibly undefined, either permanently or temporarily.
  * More specifically, a `Ref` can be:
  *   1. [[net.noresttherein.sugar.vars.Ref.isEmpty empty]]/[[net.noresttherein.sugar.vars.nonEmpty non empty]] -
  *      currently contain a [[net.noresttherein.sugar.vars.Ref.value value]], with no guarantee about its future state,
  *   1. [[net.noresttherein.sugar.vars.Ref.isDefined defined]] -
  *      having a stored value or capable of producing one on demand,
  *   1. [[net.noresttherein.sugar.vars.Ref.isFinal final]] -
  *      either defined or undefined, but immutable from this point forward,
  *   1. [[net.noresttherein.sugar.vars.Ref.isConst constant]] - non empty and final,
  *   1. [[net.noresttherein.sugar.vars.Ref.isFinalizable finalizable]] - 'eventually immutable', that is capable
  *      of becoming [[net.noresttherein.sugar.vars.Ref.isConst constant]].
  *
  * Some implementations disallow some of these states. What constitutes the value of a `Ref`
  * depends on the implementation and usage context; it can vary from an immediately available, stored value,
  * a logical value, abstracting over the manner in which it is computed, to an expected final constant.
  * This is reflected in multiple standard ways in which the value of a `Ref` can be queried:
  *   1. [[net.noresttherein.sugar.vars.Ref.value value]] reflects the current state and never blocks;
  *   1. [[net.noresttherein.sugar.vars.Ref.get get]] returns the value best representing the character
  *      of the implementing class: for example, for [[net.noresttherein.sugar.vars.Lazy Lazy]],
  *      it is the initialized value, while for [[net.noresttherein.sugar.vars.Mutable Mutable]] variables
  *      it is the current state, same as `value`.
  *      Equality and ordering between `Ref`s is defined in terms of these values.
  *   1. [[net.noresttherein.sugar.vars.Ref.const const]] value is a final, immutable value
  *      a `Ref` can take in its lifetime, if the particular implementation has such a concept.
  *
  * In general, when speaking about a `Ref`'s value without mentioning a particular implementation
  * or specifically discerning one of these contexts, it is assumed to mean `Ref.get`.
  * Subclasses are roughly divided into 'mutable' and 'immutable', although the terms are meant loosely.
  * [[net.noresttherein.sugar.vars.Val Val]] implementations are special in that they can reach a final state
  * and thus have a [[net.noresttherein.sugar.vars.Ref.const constant]] value.
  * @tparam T the type of referenced value.
  * @define Ref `Ref`
  */
trait Ref[@specialized(SpecializedVars) +T] extends Any with Equals {

	/** The $Ref will see no further changes, regardless if empty or not. This does not imply
	  * that it was always immutable. [[net.noresttherein.sugar.vars.Lazy Lazy]] values and similar may return `false`
	  * if uninitialized. If this method returns `true`, then this instance is from this point on immutable.
	  * @see [[net.noresttherein.sugar.vars.Ref.isConst isConst]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isFinalizable isFinalizable]]
	  */
	def isFinal :Boolean //certain fixed frozen stable

	/** The $Ref has no current value.
	  * In a single threaded context, it implies that `this.`[[net.noresttherein.sugar.vars.Ref.value value]]
	  * will throw a [[NoSuchElementException]] and
	  * [[net.noresttherein.sugar.vars.Ref.option option]]/[[net.noresttherein.sugar.vars.Ref.opt opt]]/[[net.noresttherein.sugar.vars.Ref.unsure unsure]]
	  * will return `None`/[[net.noresttherein.sugar.vars.Opt.Lack Lack]]/[[net.noresttherein.sugar.vars.Missing Missing]].
	  * Note that in a multi-threaded context the result may be stale the moment the method returns.
	  * @see [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isFinalizable isFinalizable]]
	  * @return [[net.noresttherein.sugar.vars.Ref.opt opt]]`.isEmpty` (or equivalent).
	  */
	def isEmpty :Boolean = opt.isEmpty

	/** The $Ref currently holds a [[net.noresttherein.sugar.vars.Ref.value value]].
	  * Note that in a multi-threaded context the result may be incorrect the moment the method returns.
	  * In absence of additional guarantees specified by the contract of a subclass,
	  * prefer polling with [[net.noresttherein.sugar.vars.Ref.opt opt]].
	  * @see [[net.noresttherein.sugar.vars.Ref.isConst isConst]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isFinalizable isFinalizable]]
	  * @return `!`[[net.noresttherein.sugar.vars.Ref.isEmpty isEmpty]]
	  */
	def nonEmpty :Boolean = !isEmpty

	/** The $Ref is 'eventually immutable': while its current state
	  * (as returned by [[net.noresttherein.sugar.vars.Ref.value value]] and/or [[net.noresttherein.sugar.vars.Ref.get get]])
	  * is undefined, it may become [[net.noresttherein.sugar.vars.Ref.isConst constant]] at some point in the future
	  * and [[net.noresttherein.sugar.vars.Ref.const const]] will return such a value, possibly blocking until
	  * an external thread provides it. Not all values capable of becoming constant are automatically finalizable:
	  * the requirement is for `const` not to throw an exception under normal conditions
	  * (although it may wait indefinitely). Being `constant` automatically implies being finalizable,
	  * otherwise it is a property of some implementing classes (subclasses of [[net.noresttherein.sugar.vars.Val Val]],
	  * not only [[net.noresttherein.sugar.vars.Freezer Freezer]] itself).
	  * @see [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]]
	  */
	def isFinalizable: Boolean

	/** The $Ref contains an initialized value which is guaranteed not to change. Once this method returns `true`,
	  * it will remain `true` for the rest of this $Ref's lifetime, although in a multi-threaded context
	  * this is guaranteed only for thread safe implementations.
	  * Implies `this.`[[net.noresttherein.sugar.vars.Ref.isDefined isDefined]],
	  * [[net.noresttherein.sugar.vars.Ref.isDefinite isDefinite]]
	  * and [[net.noresttherein.sugar.vars.Ref.isFinalizable isFinalizable]].
	  * A constant $Ref will never throw an exception from [[net.noresttherein.sugar.vars.Ref.value value]],
	  * [[net.noresttherein.sugar.vars.Ref.get get]], [[net.noresttherein.sugar.vars.Ref.const const]],
	  * and all these methods will return the same value. Similarly, all optional value methods will return the same value.
	  * @return [[net.noresttherein.sugar.vars.Ref.isFinal isFinal]]` && `[[net.noresttherein.sugar.vars.Ref.nonEmpty nonEmpty]].
	  * @see [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isFinalizable isFinalizable]]
	  * @see [[net.noresttherein.sugar.vars.Ref.const const]]
	  */
	def isConst :Boolean //not implemented here to avoid accidentally becoming not threadsafe

	/** The $Ref contains a proper value or can compute one on demand. In a single threaded context,
	  * or with proper synchronization, this implies that [[net.noresttherein.sugar.vars.Ref.get get]]
	  * will return this value without throwing an exception, although
	  * [[net.noresttherein.sugar.vars.Ref.toOption toOption]]/[[net.noresttherein.sugar.vars.Ref.toOpt toOpt]]/[[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]]
	  * may still return `None`/[[net.noresttherein.sugar.vars.Opt.Lack Lack]]/[[net.noresttherein.sugar.vars.Missing Missing]]
	  * to avoid blocking. This implies nothing about `this.`[[net.noresttherein.sugar.vars.Ref.value value]]:
	  * it might return a different result or throw a [[NoSuchElementException]].
	  * Depending on the implementation, this state can be temporary and/or the actual value returned by `get` may change.
	  * @see [[net.noresttherein.sugar.vars.Ref.isConst isConst]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isFinalizable isFinalizable]]
	  * @see [[net.noresttherein.sugar.vars.Ref.get get]]
	  */ //consider: this is a bit misleading for some classes like Relay
	def isDefined :Boolean

	/** The $Ref currently holds a proper value: in single threaded context, or with proper synchronization,
	  * [[net.noresttherein.sugar.vars.Ref.get get]] and [[net.noresttherein.sugar.vars.Ref.toOpt toOpt]],
	  * [[net.noresttherein.sugar.vars.Ref.toOption toOption]], [[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]]
	  * will return this value without blocking. Implies [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]].
	  * This value needs not be final - the above methods are allowed to return different values in the future.
	  * An uninitialized [[net.noresttherein.sugar.vars.Lazy lazy]] val is ''defined'', but not ''definite'',
	  * while a [[net.noresttherein.sugar.vars.Var variable]] is always ''definite''.
	  * @see [[net.noresttherein.sugar.vars.Ref.isEmpty isEmpty]]
	  * @see [[net.noresttherein.sugar.vars.Ref.isDefined isDefined]]
	  */
	def isDefinite :Boolean

	/** `True` if this `Ref` is an instance of `Val[T]`.
	  * This implies having the capability of having a [[net.noresttherein.sugar.vars.Val.const constant]] value,
	  * although it might not be computed at this point and the method may even throw an exception
	  * if that value must be externally set and no provision exists for awaiting its initialization.
	  */
	@inline final def isVal :Boolean = this.isInstanceOf[Val[_]]

	/** The current value of this $Ref, if it exists. This is the lowest level (with most variable results)
	  * of all methods returning a value. In a concurrent context, the result may be stale the moment the method returns.
	  * @see [[net.noresttherein.sugar.vars.Ref.get get]]
	  * @see [[net.noresttherein.sugar.vars.Ref.const const]]
	  * @see [[net.noresttherein.sugar.vars.Ref.opt opt]]
	  * @see [[net.noresttherein.sugar.vars.Ref.option option]]
	  * @see [[net.noresttherein.sugar.vars.Ref.NonEmpty.unapply NonEmpty.unapply]]
	  */ //consider: renaming to current, and making get an alias for value
	@throws[NoSuchElementException]("if this instance doesn't contain an initialized value at the moment of this call.")
	def value: T

	/** The value of this $Ref, if available or can be computed. The exact semantics depend on the implementation:
	  *   - for mutable [[net.noresttherein.sugar.vars.InOut variables]],
	  *     it is the same as [[net.noresttherein.sugar.vars.Ref.value value]];
	  *   - for [[net.noresttherein.sugar.vars.Lazy lazy]] values, it initializes this $Ref, if needed;
	  *   - for other [[net.noresttherein.sugar.vars.Val val]]s, it is their final, constant value,
	  *     if available at this point.
	  *
	  * In a single threaded context, or with proper synchronization (if allowed by the subclass), the guarantees are:
	  *   1. if `this.`[[net.noresttherein.sugar.vars.Ref.isConst isConst]], then this method will always succeed
	  *      and return the same value;
	  *   1. if `this.`[[net.noresttherein.sugar.vars.Ref.isDefined isDefined]] and `get` returns a value
	  *      without throwing an exception, then all subsequent calls will also succeed and return the same value;
	  *   1. if `this.`[[net.noresttherein.sugar.vars.Ref.toOption toOption]]
	  *      ([[net.noresttherein.sugar.vars.Ref.toOpt toOpt]], [[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]])
	  *      returns `Some` (`Got`, `Sure`), then this method will return the same value unwrapped;
	  *   1. if this method returns `v` without an exception, then [[net.noresttherein.sugar.vars.Ref.value value]]
	  *      will also return the same value (i.e., the 'value' of a `Ref` is also always 'its current value');
	  *   1. if this method returns `v` without an exception, then, in a synchronized or single thread context,
	  *      all subsequent calls to
	  *      [[net.noresttherein.sugar.vars.Ref.toOption toOption]]/[[net.noresttherein.sugar.vars.Ref.toOpt toOpt]]/[[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]]
	  *      will return `Some(v)`/`Got(v)`/`Sure(v)`.
	  *   1. if a call to `this.`[[net.noresttherein.sugar.vars.const const]] succeeds and returns `v`,
	  *      then all subsequent calls to `get` will also return `v`.
	  *
	  * The call can block if initialization is currently in progress. Whenever there is talk about a `Ref`'s value
	  * without specifying its concrete class or additional information and without making a clear distinction
	  * between [[net.noresttherein.sugar.vars.Ref.value value]], `get` and [[net.noresttherein.sugar.vars.Ref.const const]],
	  * this is the meant value. It is the value used in equality, ordering and by other type classes of `Ref[_]`.
	  * This method is equivalent to [[net.noresttherein.sugar.vars.Ref.apply apply]]`()`.
	  * @see [[net.noresttherein.sugar.vars.Ref.value value]]
	  * @see [[net.noresttherein.sugar.vars.Ref.const const]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toOpt toOpt]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toOption toOption]]
	  * @see [[net.noresttherein.sugar.vars.Ref.unapply Ref.unapply]]
	  */
	@throws[NoSuchElementException]("if this instance contains no value and none can be computed.")
	def get: T

	/** The final value of this $Ref, if it is [[net.noresttherein.sugar.vars.Ref.isFinalizable finalizable]].
	  * This method may block while awaiting the result or change the state of this instance
	  * (in particular, making it [[net.noresttherein.sugar.vars.Ref.isFinal final]]).
	  * All calls always have the same effect: either the same value is returned, or the same exception is thrown.
	  * If this call succeeds and returns `v`, then
	  * [[net.noresttherein.sugar.vars.Ref.constOption constOption]]/[[net.noresttherein.sugar.vars.Ref.constOpt constOpt]]/[[net.noresttherein.sugar.vars.Ref.constUnsure constUnsure]]
	  * will always return `Some(v)`/`Got(v)`/`Sure(v)` and vice versa.
	  * @see [[net.noresttherein.sugar.vars.Ref.value value]]
	  * @see [[net.noresttherein.sugar.vars.Ref.get get]]
	  */
	@throws[UnsupportedOperationException]("if this class does not impose any constraints on future mutability.")
	def const :T

	/** An alias for [[net.noresttherein.sugar.vars.Ref.const const]]. */
	@inline final def frozen :T = const //exists so that `Freezer.frozen_=` parses properly

	/** The value of this $Ref, if available or can be computed. The exact semantics depend on the actual implementation,
	  * in particular on its mutability/immutability.
	  * If a class has a capability of taking a [[net.noresttherein.sugar.vars.Ref.const constant]] value,
	  * it is equivalent to `this.const`; otherwise it defaults to [[net.noresttherein.sugar.vars.Ref.get get]].
	  * While `get` on all `Val`s must always return the same value, they are allowed to throw an exception
	  * in order to avoid blocking for external input, so this method isn't necessarily always equivalent simply to `get`.
	  */
	@throws[NoSuchElementException]("if this instance contains no value and none can be computed.")
	def apply(): T//not implemented here because it confuses scalac SAM type literals

	/** The current value of this $Ref.
	  * As a rule, equivalent to `Try(`[[net.noresttherein.sugar.vars.Ref.value value]]`).toOption` and,
	  * in single threaded context - or with synchronization proper to the implementation - to
	  * {{{
	  * 	if (nonEmpty) Some(value) else None
	  * }}}
	  * @see [[net.noresttherein.sugar.vars.Ref.isEmpty isEmpty]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toOption toOption]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constOption constOption]]
	  */
	def option :Option[T] = opt.constOption

	/** The value of this $Ref, if it is available or can be computed. Lazily initialized objects
	  * (containing their initializers) will proceed with the initialization if necessary, but subclasses which require
	  * external setting of the value will return [[None]].
	  * As a rule, in a single threaded context (or when properly synchronized),
	  * if this method returns `Some(v)`, then [[net.noresttherein.sugar.vars.Ref.get get]] will return `v`
	  * without throwing an exception. Conversely, if `get` returns `v`, this method can return only `Some(v)`
	  * or `None` (in particular to avoid blocking).
	  *
	  * The method can return different values for different
	  * calls, unless this instance is immutable (a [[net.noresttherein.sugar.vars.Val Val]]).
	  * It can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  * @see [[net.noresttherein.sugar.vars.Ref.option option]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constOption constOption]]
	  */
	def toOption :Option[T] = toOpt.toOption

	/** The [[net.noresttherein.sugar.vars.Ref.const constant]] value of this $Ref, if one exists.
	  * All non-empty returned values are equal: in a single threaded context (or when properly synchronized),
	  * if `const` returns `v`, then this method will return `Some(v)` and vice versa.
	  * Also, if `this.`[[net.noresttherein.sugar.vars.Ref.isDefined isDefined]]
	  * As a rule, it is equivalent to `Try(const).toOption`.
	  * [[net.noresttherein.sugar.vars.Mutable Mutable]] implementations
	  * always return [[net.noresttherein.sugar.vars.Opt.Lack Lack]], unless they can be 'finalized'
	  * (see [[net.noresttherein.sugar.vars.Freezer Freezer]]). Immutable instances return `Some(const)`
	  * (possibly initializing the value).
	  * @see [[net.noresttherein.sugar.vars.Ref.option option]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toOption toOption]]
	  */
	def constOption :Option[T] = constOpt.toOption

	/** The current value of this $Ref. In a single threaded context - or with synchronization proper
	  * to the implementation - it is equivalent to
	  * {{{
	  * 	if (nonEmpty) Got(value) else Lack
	  * }}}
	  * @see [[net.noresttherein.sugar.vars.Ref.isEmpty isEmpty]]
	  * @see [[net.noresttherein.sugar.vars.Ref.value value]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toOpt toOpt]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constOpt constOpt]]
	  */
	def opt :Opt[T]

	/** The value of this $Ref, if it is available or can be computed. Lazily initialized objects
	  * (containing their initializers) will proceed with the initialization if necessary, but subclasses which require
	  * external setting of the value will return [[net.noresttherein.sugar.vars.Opt.Lack Lack]].
	  * As a rule, in a single threaded context (or when properly synchronized),
	  * if this method returns `Got(v)`, then [[net.noresttherein.sugar.vars.Ref.get get]] will return `v`
	  * without throwing an exception. Conversely, if `get` returns `v`, this method can return only `Got(v)`
	  * or `Lack` (in particular to avoid blocking).
	  *
	  * The method can return different values for different calls, unless this instance is immutable
	  * (a [[net.noresttherein.sugar.vars.Val Val]]).
	  * It can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  * @see [[net.noresttherein.sugar.vars.Ref.get get]]
	  * @see [[net.noresttherein.sugar.vars.Ref.opt opt]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constOpt constOpt]]
	  */
	def toOpt :Opt[T]

	/** The [[net.noresttherein.sugar.vars.Ref.const constant]] value of this $Ref, if one exists.
	  * All non-empty returned values are equal: in a single threaded context (or when properly synchronized),
	  * if `const` returns `v`, then this method will return `Got(v)` and vice versa.
	  * As a rule, it is equivalent to `Opt.fromOption(Try(const).toOption)`.
	  * [[net.noresttherein.sugar.vars.Mutable Mutable]] implementations
	  * always return [[net.noresttherein.sugar.vars.Opt.Lack Lack]], unless they can be 'finalized'
	  * (see [[net.noresttherein.sugar.vars.Freezer Freezer]]). Immutable instances return
	  * [[net.noresttherein.sugar.vars.Opt.Got Got]]`(const)` (possibly initializing the value).
	  * @see [[net.noresttherein.sugar.vars.Ref.opt opt]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toOpt toOpt]]
	  */
	def constOpt :Opt[T]

	/** The current value of this $Ref, as an option-like, specialized `Unsure`.
	  * In a single threaded context - or with synchronization proper to the implementation - it is equivalent to
	  * {{{
	  * 	if (nonEmpty) Sure(value) else Missing
	  * }}}
	  * @see [[net.noresttherein.sugar.vars.Ref.isEmpty isEmpty]]
	  * @see [[net.noresttherein.sugar.vars.Ref.value value]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constUnsure constUnsure]]
	  */
	def unsure :Unsure[T] = opt.unsure

	/** The value of this $Ref, if it is available or can be computed, as an option-like, specialized `Unsure`.
	  * Lazily initialized objects (containing their initializers) will proceed with the initialization if necessary,
	  * but subclasses which require external setting of the value
	  * will return [[net.noresttherein.sugar.vars.Missing Missing]].
	  * As a rule, in a single threaded context (or when properly synchronized),
	  * if this method returns `Sure(v)`, then [[net.noresttherein.sugar.vars.Ref.get get]] will return `v`
	  * without throwing an exception. Conversely, if `get` returns `v`, this method can return only `Sure(v)`
	  * or `Missing` (in particular to avoid blocking).
	  *
	  * The method can return different values for different calls, unless this instance is immutable
	  * (a [[net.noresttherein.sugar.vars.Val Val]]).
	  * It can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  * @see [[net.noresttherein.sugar.vars.Ref.get get]]
	  * @see [[net.noresttherein.sugar.vars.Ref.unsure unsure]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constUnsure constUnsure]]
	  */
	def toUnsure :Unsure[T] = toOpt.unsure

	/** The [[net.noresttherein.sugar.vars.Ref.const constant]] value of this $Ref, if one exists, as an option-like,
	  * specialized `Unsure`. All non-empty returned values are equal: in a single threaded context
	  * (or when properly synchronized), if `const` returns `v`, then this method will return `Sure(v)` and vice versa.
	  * As a rule, it is equivalent to `Unsure.fromOption(Try(const).toOption)`.
	  * [[net.noresttherein.sugar.vars.Mutable Mutable]] implementations
	  * always return [[net.noresttherein.sugar.vars.Missing Missing]], unless they can be 'frozen'
	  * (see [[net.noresttherein.sugar.vars.Freezer Freezer]]). Immutable instances return
	  * [[net.noresttherein.sugar.vars.Sure Sure]]`(const)` (possibly initializing the value).
	  * @see [[net.noresttherein.sugar.vars.Ref.unsure unsure]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]]
	  */
	def constUnsure :Unsure[T] = constOpt.unsure

	/** The current value of this $Ref. In a single threaded context - or with synchronization proper
	  * to the implementation - it is equivalent to
	  * {{{
	  * 	if (nonEmpty) Existent(value) else Inexistent
	  * }}}
	  * @see [[net.noresttherein.sugar.vars.Ref.isEmpty isEmpty]]
	  * @see [[net.noresttherein.sugar.vars.Ref.value value]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toPotential toPotential]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constPotential constPotential]]
	  */
	def potential :Potential[T] = opt.potential

	/** The value of this $Ref, if it is available or can be computed. Lazily initialized objects
	  * (containing their initializers) will proceed with the initialization if necessary, but subclasses which require
	  * external setting of the value will return [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]].
	  * As a rule, in a single threaded context (or when properly synchronized),
	  * if this method returns `Got(v)`, then [[net.noresttherein.sugar.vars.Ref.get get]] will return `v`
	  * without throwing an exception. Conversely, if `get` returns `v`, this method can return only `Got(v)`
	  * or `Lack` (in particular to avoid blocking).
	  *
	  * The method can return different values for different calls, unless this instance is immutable
	  * (a [[net.noresttherein.sugar.vars.Val Val]]).
	  * It can block to wait for another thread only if initialization is currently in progress/the value
	  * is currently mutated, but not if no other thread currently accesses this variable.
	  * @see [[net.noresttherein.sugar.vars.Ref.get get]]
	  * @see [[net.noresttherein.sugar.vars.Ref.potential potential]]
	  * @see [[net.noresttherein.sugar.vars.Ref.constPotential constPotential]]
	  */
	def toPotential :Potential[T] = toOpt.potential

	/** The [[net.noresttherein.sugar.vars.Ref.const constant]] value of this $Ref, if one exists.
	  * All non-empty returned values are equal: in a single threaded context (or when properly synchronized),
	  * if `const` returns `v`, then this method will return `Got(v)` and vice versa.
	  * As a rule, it is equivalent to `Opt.fromOption(Try(const).toOption)`.
	  * [[net.noresttherein.sugar.vars.Mutable Mutable]] implementations
	  * always return [[net.noresttherein.sugar.vars.Potential.Inexistent Inexistent]], unless they can be 'finalized'
	  * (see [[net.noresttherein.sugar.vars.Freezer Freezer]]). Immutable instances return
	  * [[net.noresttherein.sugar.vars.Opt.Got Got]]`(const)` (possibly initializing the value).
	  * @see [[net.noresttherein.sugar.vars.Ref.potential potential]]
	  * @see [[net.noresttherein.sugar.vars.Ref.toPotential toPotential]]
	  */
	def constPotential :Potential[T] = constOpt.potential

	/** Returns `true` if both instances are [[net.noresttherein.sugar.vars.Ref.isDefined defined]]
	  * and their  [[net.noresttherein.sugar.vars.Ref.value values]] are equal.
	  */
	def valueEquals(other :Ref[_]) :Boolean = (opt, other.opt) match {
		case (Got(a), Got(b)) => a == b
		case _ => false
	}

	/** Equivalent to
	  * [[net.noresttherein.sugar.vars.Ref.toOpt toOpt]]` `[[net.noresttherein.sugar.vars.Opt.same same]]` other.toOpt`.
	  */
	def same(other :Ref[_]) :Boolean = toOpt same other.toOpt

//	def sameOpt(other :Ref[_]) :Ref[Boolean] =

	/** True if the content type is known to be a value type and the class is specialized for it.
	  * Signifies that usage of [[net.noresttherein.sugar.vars.Unsure Unsure]] is preferable
	  * over [[net.noresttherein.sugar.vars.Opt Opt]]. Should not be relied upon for anything critical
	  * and code should work correctly if this method returns a false result, in particular
	  * a false negative (the default return value being `false`).
	  */
	private[vars] def isSpecialized :Boolean = false

	override def equals(that: Any): Boolean = that match {
		case self: AnyRef if this.asInstanceOf[AnyRef] eq self => true
		case other: Ref[_] if other canEqual this =>
			if (isSpecialized && other.isSpecialized)
				(toUnsure, other.toUnsure) match {
					case (Sure(a), Sure(b)) => a == b
					case _ => false
				}
			else
				(toOpt, other.toOpt) match {
					case (Got(a), Got(b)) => a == b
					case _ => false
				}
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Ref[_]]
	override def hashCode: Int = toOpt.hashCode

	/** Formats this instance as `s"$prefix($value)` or `s"$prefix()` if [[net.noresttherein.sugar.vars.Ref.isEmpty empty]]. */
	def mkString(prefix :String) :String =
		if (isSpecialized)
			unsure match {
				case Sure(v) => prefix + "(" + v + ")"
				case _ => prefix + "()"
			}
		else
			opt match {
				case Got(v) => prefix + "(" + v + ")"
				case _ => prefix + "()"
			}

	/** Formats this instance as if it was an `Iterable` holding its value. */
	def mkString :String = mkString(this.innerClassName)

	override def toString :String =
		if (isSpecialized)
			unsure match {
				case Sure(v) => String.valueOf(v)
				case _       => undefined.toString
			}
		else
			opt match {
				case Got(v) => String.valueOf(v)
				case _ => undefined.toString
			}

}




/** A match pattern and extractor of values of [[net.noresttherein.sugar.vars.Ref Ref]] instances,
  * as well as a scope for related type classes.
  */
@SerialVersionUID(Ver)
object Ref {
	/** Extracts the value returned by [[net.noresttherein.sugar.vars.Ref.get get]], if it is available. */
	@inline def unapply[T](ref :Ref[T]) :Opt[T] = ref.toOpt

	/** A match pattern for currently [[net.noresttherein.sugar.vars.Ref.nonEmpty non empty]]
	  * [[net.noresttherein.sugar.vars.Ref Ref]] instances.
	  */
	object NonEmpty {
		/** Returns `ref.`[[net.noresttherein.sugar.vars.Ref.opt opt]]. */
		@inline def unapply[T](ref :Ref[T]) :Opt[T] = ref.opt
	}


	implicit def RefPartialOrdering[V[X] <: Ref[X], T :PartialOrdering] :PartialOrdering[V[T]] =
		new RefPartialOrdering[V, T]

	@SerialVersionUID(Ver)
	private[vars] class RefPartialOrdering[V[X] <: Ref[X], T](implicit content :PartialOrdering[T])
		extends PartialOrdering[V[T]]
	{
		private[this] final val ComparableEquiv = Some(0)

		override def tryCompare(x :V[T], y :V[T]) :Option[Int] =
			if (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
				ComparableEquiv
			else if (x.isSpecialized && y.isSpecialized)
				(x.toUnsure, y.toUnsure) match {
					case (Sure(vx), Sure(vy)) => content.tryCompare(vx, vy)
					case _ => None
				}
			else
				(x.toOpt, y.toOpt) match {
					case (Got(vx), Got(vy)) =>  content.tryCompare(vx, vy)
					case _ => None
				}

		override def lteq(x :V[T], y :V[T]) :Boolean =
			if (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
				true
			else if (x.isSpecialized && y.isSpecialized)
				(x.toUnsure, y.toUnsure) match {
					case (Sure(vx), Sure(vy)) => content.lteq(vx, vy)
					case _ => false
				}
			else
				(x.toOpt, y.toOpt) match {
					case (Got(vx), Got(vy)) => content.lteq(vx, vy)
					case _ => false
				}
	}

	@SerialVersionUID(Ver)
	private[vars] class RefOrdering[V[X] <: Ref[X], T](implicit content :Ordering[T]) extends Ordering[V[T]] {
		override def compare(x :V[T], y :V[T]) :Int = content.compare(x.get, y.get)
	}

	private[vars] abstract class RefNumeric[V[X] <: Ref[X], T](implicit content :Numeric[T])
		extends RefOrdering[V, T] with Numeric[V[T]]
	{
		protected def value :Numeric[T] = content

		private[this] val + = content.plus _
		private[this] val - = content.minus _
		private[this] val * = content.times _
		private[this] val ~ = content.negate _
		private[this] val V = apply _

		override def plus(x :V[T], y :V[T]) :V[T] = fmap(x, y)(+)
		override def minus(x :V[T], y :V[T]) :V[T] = fmap(x, y)(-)
		override def times(x :V[T], y :V[T]) :V[T] = fmap(x, y)(*)

		override def negate(x :V[T]) :V[T] = map(x)(~)

		override def fromInt(x :Int) :V[T] = apply(content.fromInt(x))
		override def parseString(str :String) :Option[V[T]] = content.parseString(str).map(V)
		override def toInt(x :V[T]) :Int = content.toInt(x.get)
		override def toLong(x :V[T]) :Long = content.toLong(x.get)
		override def toFloat(x :V[T]) :Float = content.toFloat(x.get)
		override def toDouble(x :V[T]) :Double = content.toDouble(x.get)

		protected def fmap(x :V[T], y :V[T])(op :(T, T) => T) :V[T] = apply(op(x.get, y.get))
		protected def map(x :V[T])(f :T => T) :V[T] = apply(f(x.get))
		protected def apply(x :T) :V[T]
	}

	abstract class RefIntegral[V[X] <: Ref[X], T](implicit content :Integral[T])
		extends RefNumeric[V, T] with Integral[V[T]]
	{
		protected override def value :Integral[T] = content
		private[this] val / = content.quot _
		private[this] val % = content.rem _

		override def quot(x :V[T], y :V[T]) :V[T] = fmap(x, y)(/)
		override def rem(x :V[T], y :V[T]) :V[T] = fmap(x, y)(%)
	}

	abstract class RefFractional[V[X] <: Ref[X], T](implicit content :Fractional[T])
		extends RefNumeric[V, T] with Fractional[V[T]]
	{
		protected override def value :Fractional[T] = content
		private[this] val / = content.div _

		override def div(x :V[T], y :V[T]) :V[T] = fmap(x, y)(/)
	}


	/** Mixin trait for completely immutable, [[net.noresttherein.sugar.vars.Ref.isFinal final]] `Ref` implementations.
	  * Subclasses are not required to contain a value, in which case they must remain empty
	  * for the remainder of their lifetime.
	  */
	private[vars] trait FinalRef[+T] extends Ref[T] {
		@inline final override def isFinal       :Boolean = true
		/** Same as [[net.noresttherein.sugar.vars.Ref.nonEmpty nonEmpty]]. */
		@inline final override def isFinalizable :Boolean = !isEmpty
		/** Same as [[net.noresttherein.sugar.vars.Ref.nonEmpty nonEmpty]]. */
		@inline final override def isConst       :Boolean = !isEmpty
		override def isDefined                   :Boolean = !isEmpty
		/** Same as [[net.noresttherein.sugar.vars.Ref.nonEmpty nonEmpty]]. */
		@inline final override def isDefinite    :Boolean = !isEmpty
		/** Same as [[net.noresttherein.sugar.vars.Ref.get get]]. */
		@inline final override def value :T = get
		/** Same as [[net.noresttherein.sugar.vars.Ref.get get]]. */
		@inline final override def apply() :T = get
		/** Same as [[net.noresttherein.sugar.vars.Ref.get get]],
			* except throws an [[UnsupportedOperationException]] instead of [[NoSuchElementException]].
			*/
		@inline final override def const :T =
			if (!isEmpty) get else throw new UnsupportedOperationException(this.localClassName + ".const")
		/** Same as [[net.noresttherein.sugar.vars.Ref.option option]]. */
		@inline final override def toOption    :Option[T] = option
		/** Same as [[net.noresttherein.sugar.vars.Ref.option option]]. */
		@inline final override def constOption :Option[T] = option
		/** Same as [[net.noresttherein.sugar.vars.Ref.opt opt]]. */
		@inline final override def toOpt       :Opt[T] = opt
		/** Same as [[net.noresttherein.sugar.vars.Ref.opt opt]]. */
		@inline final override def constOpt    :Opt[T] = opt
		/** Same as [[net.noresttherein.sugar.vars.Ref.unsure unsure]]. */
		@inline final override def toUnsure    :Unsure[T] = unsure
		/** Same as [[net.noresttherein.sugar.vars.Ref.unsure unsure]]. */
		@inline final override def constUnsure :Unsure[T] = unsure
	}




	/** A marker object used by some implementations to signify that a reference has no actual value. */
	@SerialVersionUID(Ver)
	private[vars] object undefined extends Serializable {
		override def toString = "<undefined>"

		@inline final override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
	}

}
