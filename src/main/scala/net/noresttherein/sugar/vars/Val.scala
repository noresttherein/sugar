package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.{RefFractional, RefIntegral, RefNumeric, RefOrdering}




/** A supertype of variables which can reach an immutable, final state and have well-defined
  * [[net.noresttherein.sugar.vars.Ref.const constant]] values. The current
  * [[net.noresttherein.sugar.vars.Ref.value value]] of a `Val` can be mutable,
  * but [[net.noresttherein.sugar.vars.Ref.get get]] will not change once set,
  * and all ''successful'' reads return the same value. The chief implementation class is
  * [[net.noresttherein.sugar.vars.Delayed Delayed]], but the value can be also set explicitly at a later time as in
  * [[net.noresttherein.sugar.vars.Out Out]].
  *
  * One thing in common among all subclasses which differentiate them
  * from the contract of [[net.noresttherein.sugar.vars.Ref Ref]] is that
  * [[net.noresttherein.sugar.vars.Ref.const const]] (and [[net.noresttherein.sugar.vars.Val.apply apply]]`()`)
  * will always return a value, and always the same value for the same instance of `Val`.
  * How and when that value is computed is unspecified, and optional methods
  * [[net.noresttherein.sugar.vars.Ref.opt opt]]/[[net.noresttherein.sugar.vars.Ref.option option]]/[[net.noresttherein.sugar.vars.Ref.unsure unsure]]/[[net.noresttherein.sugar.vars.Ref.maybe maybe]]
  * and [[net.noresttherein.sugar.vars.Ref.toMaybe toMaybe]]/[[net.noresttherein.sugar.vars.Ref.toOption toOption]]/[[net.noresttherein.sugar.vars.Ref.toUnsure toUnsure]]
  * are allowed to return empty instances if the value is not available at the moment of calling.
  *
  * Equality and `hashCode` is universally defined as equality of `const` values, and thus may block.
  * If the constant value is unknown and cannot be computed at this point, a `Val` will equal no other instance
  * other than itself. Also, a `Val` can equal only other `Val`s, irrespective of their concrete implementation.
  * This trait and all its subclasses are thread safe.
  *
  * @define Ref `Val`
  * @define ref value
  * @author Marcin Mościcki
  */ //consider: caching of the T wrapper, Maybe, Option, Unsure
trait Val[@specialized(SpecializedVars) +T] extends Ref[T] { //consider: toRef for equality semantic change
	/** Returns [[net.noresttherein.sugar.vars.Val.isDefinite isDefinite]]. */
	override def isFinal :Boolean = isDefinite //or nonEmpty,

	/** The instance holds a possibly temporary [[net.noresttherein.sugar.vars.Val.value value]].
	  * Without information about the concrete class of this $Ref the method is of little use.
	  * Use [[net.noresttherein.sugar.vars.Val.isDefined isDefined]] in order to verify if the actual
	  * [[net.noresttherein.sugar.vars.Val.const constant]] value can be computed
	  * or [[net.noresttherein.sugar.vars.Val.isConst isConst]] to check if it is already available.
	  * @return `!`[[net.noresttherein.sugar.vars.Val.isEmpty isEmpty]].
	  */
	@inline final override def nonEmpty :Boolean = !isEmpty

	/** The current wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. It may not be the [[net.noresttherein.sugar.vars.Val.const constant]]
	  * value of this `Val`: some implementations extend also [[net.noresttherein.sugar.vars.InOut InOut]],
	  * splitting their life cycle into two phases: mutable initialization and immutable final phase. In that case
	  * `this.`[[net.noresttherein.sugar.vars.InOut.value value]] may return different values for different calls.
	  * In most cases, there should be no need to call this method on a `Val`: prefer using
	  * [[net.noresttherein.sugar.vars.Val.get get]] and `const`, which will always return the same value
	  * from successful calls.
	  */ //redeclaration from Ref solely for a documentation change.
	override def value :T

	/** The wrapped value, if available. Depending on the actual implementation,
	  * it can throw a [[NoSuchElementException]]. If any call completes without throwing an exception,
	  * all subsequent calls will return the same value.
	  *///redeclaration from Ref solely for a documentation change.
	override def get :T

	/** The evaluated value of this `Val`.
	  * @return `this.`[[net.noresttherein.sugar.vars.Val.const const]].
	  */
	@inline final override def apply() :T = const

	/** Creates a new `Val` the value of which is derived from the value of this instance.
	  * If `this.`[[net.noresttherein.sugar.vars.Val.isConst isConst]], then the new value is initialized immediately
	  * with `f(get)`. Otherwise, a proxy object is returned, which implements
	  * [[net.noresttherein.sugar.vars.Val.isDefined isDefined]],
	  * [[net.noresttherein.sugar.vars.Val.get get]], [[net.noresttherein.sugar.vars.Val.const const]],
	  * and their optional variants by delegating to this instance.
	  * The semantics of [[net.noresttherein.sugar.vars.Val.value value]] and other methods querying the temporary state
	  * are undefined.
	  */
	def map[O](f :T => O) :Val[O] =
		if (isConst) Delayed.eager(f(get))
		else new MappedVal(this, f)

	/** Creates a new `Val` the value of which is derived from the value of this instance.
	  * If `this.`[[net.noresttherein.sugar.vars.Val.isConst isConst]], then the function is evaluated immediately,
	  * and its result is returned. Otherwise, a proxy object is created, which implements
	  * [[net.noresttherein.sugar.vars.Val.get get]], [[net.noresttherein.sugar.vars.Val.const const]],
	  * and their optional variants as a composition of the same methods on `this`, and `f`.
	  * The semantics of [[net.noresttherein.sugar.vars.Val.value value]] and other methods querying the temporary state
	  * are undefined.
	  */
	def flatMap[O](f :T => Val[O]) :Val[O] =
		if (isConst) f(get)
		else new FlatMappedVal(this, f)

	/** Compares the evaluated [[net.noresttherein.sugar.vars.Val.const const]] values for equality,
	  * blocking if either of the instances is not yet initialized.
	  */
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Val[_] if other canEqual this => const == other.const
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Val[_]]
	override def hashCode :Int = cachedHash
	private lazy val cachedHash = const.hashCode
}




@SerialVersionUID(Ver)
object Val {
	/** A wrapper over truly immutable, eagerly initialized value. Usually not very useful in the application code,
	  * but methods which work on lazy values can in some cases return such an instance if the argument
	  * is already initialized.
	  */
	def apply[T](value :T) :Val[T] = Delayed.eager(value)

	/** Matches any `Val` subclass, returning the wrapped value. */
	def unapply[T](value :Val[T]) :Val[T] = value

	/** Matches any `Val` subclass, returning the wrapped value. */
	def unapply[T](value :Ref[T]) :Maybe[T] = value.maybe //Is opt thread safe in all implementations?


	implicit def valOrdering[V[X] <: Val[X], T: Ordering] :Ordering[V[T]] = new RefOrdering[V, T]
	implicit def valNumeric[T: Numeric]       :Numeric[Val[T]] = new RefNumeric[Val, T] with ValTypeClass[T]
	implicit def valIntegral[T: Integral]     :Integral[Val[T]] = new RefIntegral[Val, T] with ValTypeClass[T]
	implicit def valFractional[T: Fractional] :Fractional[Val[T]] = new RefFractional[Val, T] with ValTypeClass[T]

	private trait ValTypeClass[T] extends RefNumeric[Val, T] {
		protected override def apply(x: T): Val[T] = Val(x)
	}

}




/** A proxy `Val` mapping the value of another `Val`. */
@SerialVersionUID(Ver)
private class MappedVal[V, +O](source: Val[V], f: V => O) extends Ref[O] with Val[O] {
	@volatile private[this] var x: Opt[O] = None

	override def isFinalizable :Boolean = x.isDefined || source.isFinalizable
	override def isConst       :Boolean = x.isDefined || source.isConst
	override def isDefined     :Boolean = x.isDefined || source.isDefined
	override def isDefinite    :Boolean = x.isDefined || source.isDefinite

	override def value :O = maybe.orNoSuch("Val()")
	override def get   :O = x getOrElse adaptVal(source.get)
	override def const :O = x getOrElse adaptVal(source.const)

	@inline private def adaptVal(v: V) :O = {
		val res = f(v)
		x = One(res)
		res
	}

	override def opt :Opt[O] = x orElse {
		if (source.isConst) {
			val res = One(f(source.const))
			x = res
			res
		} else
			None
	}
	override def toOpt    :Opt[O] = x orElse adaptOpt(source.toOpt)
	override def constOpt :Opt[O] = x orElse adaptOpt(source.constOpt)

	private def adaptOpt(outer: Opt[V]) = {
		val local = x
		if (local.isDefined)
			local
		else if (outer.isEmpty)
			None
		else {
			val res = One(f(outer.get))
			x = res
			res
		}
	}

	override def mkString = mkString("Val")
	override def toString :String = super[Ref].toString
}




/** A proxy `Val` flat mapping the value of another `Val`. */
@SerialVersionUID(Ver)
private class FlatMappedVal[V, +O](source: Val[V], f: V => Val[O]) extends Ref[O] with Val[O] {
	@volatile private[this] var cache :Opt[Val[O]] = None

	@inline protected final def cached :Opt[Val[O]] =
		cache orElse Opt.when(source.isConst)(adaptVal(source.const))

	@inline private def adaptVal(v: V) :Val[O] = {
		val res = f(v)
		cache = One(res)
		res
	}

	override def isFinalizable :Boolean = cached.mapOrElse(_.isFinalizable, false)
	override def isConst       :Boolean = cached.mapOrElse(_.isConst, false)
	override def isDefined     :Boolean = cached.mapOrElse(_.isDefined, false)
	override def isDefinite    :Boolean = cached.mapOrElse(_.isDefinite, false)

	override def value :O = maybe.orNoSuch("Val()")
	override def get   :O = toMaybe getOrElse adaptVal(source.get).get
	override def const :O = maybeConst.orNoSuch("Val()")

	override def opt      :Opt[O] = cached.flatMap(_.opt)
	override def toOpt    :Opt[O] = (cache orElse source.toOpt.map(adaptVal)).flatMap(_.toOpt)
	override def constOpt :Opt[O] = (cache orElse source.constOpt.map(adaptVal)).flatMap(_.constOpt)

	override def mkString = mkString("Val")
	override def toString = super[Ref].toString
}




@SerialVersionUID(Ver) //todo: extend FinalRef. Currently it conflicts with Val.apply, both being final.
private[sugar] trait Const[@specialized(SpecializedVars) +V] extends Val[V] {
	override def value :V = get
	override def const :V = get
	override def opt   :Opt[V] = One(get)
	override def toOpt :Opt[V] = One(get)
	override def constOpt :Opt[V] = One(get)

	override def isFinalizable :Boolean = true
	override def isConst       :Boolean = true
	override def isDefined     :Boolean = true
	override def isDefinite    :Boolean = true
}
