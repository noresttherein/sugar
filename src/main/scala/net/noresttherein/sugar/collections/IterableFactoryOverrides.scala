package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder
import scala.collection.{EvidenceIterableFactory, IterableFactory, IterableOps}




/** Similar to `IterableFactory.`[[scala.collection.IterableFactory.Delegate Delegate]],
  * except the implementation collection `Impl[E]` is wrapped in a `C[E]`, instead of being returned as-is.
  */
abstract class ProxyIterableFactory[+C[_], Impl[_]](underlying :IterableFactory[Impl]) extends IterableFactory[C] {
	override def from[A](source :IterableOnce[A]) :C[A] = map(underlying.from(source))

	override def empty[A] :C[A] = map(underlying.empty)

	override def newBuilder[A] :Builder[A, C[A]] = underlying.newBuilder[A].mapResult(map)

	protected def map[X](impl :Impl[X]) :C[X]
}




/** Overrides methods of [[collection.IterableFactoryDefaults IterableFactoryDefaults]]
  * with their original implementations in that trait. Used as a mixin after classes/traits which override
  * `fromSpecific` and `newSpecificBuilder` explicitly.
  */
private[collections] trait IterableFactoryOverrides[+E, +CC[A] <: Iterable[A]]
	extends IterableOps[E, Iterable, CC[E @uncheckedVariance]]
{
	override def iterableFactory :IterableFactory[CC]

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
		iterableFactory.from(coll)

	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, CC[E @uncheckedVariance]] =
		iterableFactory.newBuilder[E]

    override def empty :CC[E @uncheckedVariance] = iterableFactory.empty
}


/** Overrides methods of [[collection.EvidenceIterableFactoryDefaults EvidenceIterableFactoryDefaults]]
  * with their original implementations in that trait. Used as a mixin after classes/traits which override
  * `fromSpecific` and `newSpecificBuilder` explicitly. Note that this trait does not actually extend
  * `EvidenceIterableFactoryDefaults[E, CC, Ev]`, because the latter extends `IterableOps[E, CC, CC[E]]` and thus
  * requires a regular [[collection.IterableFactory IterableFactory]]`[CC]`,
  * which might be unavailable if `CC[E]` cannot be created without evidence `Ev[E]`.
  */
private[collections] trait EvidenceIterableFactoryOverrides[+E, +CC[A] <: Iterable[A], Ev[_]]
	extends IterableOps[E, Iterable, CC[E @uncheckedVariance]]
{
	protected def evidenceIterableFactory :EvidenceIterableFactory[CC, Ev]
	implicit protected def iterableEvidence :Ev[E @uncheckedVariance]

	override protected def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :CC[E @uncheckedVariance] =
	  evidenceIterableFactory.from(coll)

	override protected def newSpecificBuilder :Builder[E @uncheckedVariance, CC[E @uncheckedVariance]] =
		evidenceIterableFactory.newBuilder[E]

	override def empty: CC[E @uncheckedVariance] = evidenceIterableFactory.empty
}
