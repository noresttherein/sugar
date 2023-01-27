package net.noresttherein.sugar.util

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.LinearSeq
import scala.util.matching.Regex

import net.noresttherein.sugar.util.LabelPath.{/, ~/, ConcatLabelPath, Label, LabelPathPrefix, SplitLabelPath}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A type class marking the type `P` as a ''label path'' - a sequence of one or more `Label`s (string literal types)
  * separated with a `/` (a composite `LabelPath` class). The label path types, as understood here,
  * are defined as a union of of
  *   1. All `String` literal types, referred typically
  *      through type alias [[net.noresttherein.sugar.util.LabelPath.Label Label]]s;
  *   1. All types `P `[[net.noresttherein.sugar.util.LabelPath./ /]]` L` such that
  *      `P` is a label path type and `L` is a `Label` (a `String` literal type).
  *
  * Aside from its implicit witness nature, this type class actually provides the path instance it is parameterized with.
  * It is used in places where a (mapping) identifier is needed and must be composed of label identifiers
  * of individual mappings. There are two subtypes of this type:
  *   1. [[net.noresttherein.sugar.util.LabelPath.~/ ~/]], the type class for any
  *      `String` literal type representing an atomic label;
  *   1. [[net.noresttherein.sugar.util.LabelPath./ /]], the type of compound paths composed of several labels.
  *      `A / B / C` is a type class for itself, `A / B / C`.
  *      This class/type class duality comes from the dual use cases of path types: indexing by values and indexing
  *      purely on type level, and from the desire for uniform interface of single labels and compound paths.
  *      The polymorphism provided by the type class interface is unfortunately limited to cases where the label type
  *      is specified explicitly, as the compiler will never infer a literal type in place of `String`
  *      unless a singleton type is expected, introducing the need for explicit overloads of methods accepting a `Label`
  *      and a `LabelPath`. Importing the [[net.noresttherein.sugar.util.LabelPath./ /]] type
  *      imports also an implicit conversion from any string literal to `~/`, allowing their use directly
  *      as `LabelPath` values.
  *
  * Instances can be either summoned, or created,
  * starting with object [[net.noresttherein.sugar.util.LabelPath.~/ ~/]] and continuing with
  * [[net.noresttherein.sugar.util.LabelPath.LabelPathPrefix./ /]].
  *
  * Sometimes, when recursively building a path, it is convenient to represent 'an empty path'.
  * `LabelPathPrefix` exists for this purpose, introducing minimal interface for path expansion.
  * It has as subclasses this trait and the previously mentioned object.
  * @tparam P a path type - either a `Label`, or one or more `/` instances appending new labels to the first one.
  * @see [[net.noresttherein.sugar.util.LabelPath.Label]]
  * @see [[net.noresttherein.sugar.util.LabelPath./]]
  */ //consider :renaming to KeyPath (and Label to Key) or even just Path
sealed trait LabelPath[P] extends Any with LabelPathPrefix {

	/** The last label in this path. */
	def last :Label

	/** The first label in this path. This method runs in O(n). */
	def first :Label

	/** The number of labels in this path. */
	override def length :Int

	/** The instance of the path type `P` */
	def path :P

	/** Creates a new path consisting of this path followed by the given label. */
	override def /[N <: Label](next :N) :P / N = LabelPath./(this, next)

	/** Creates a new path being an inlined concatenation of this path and the given suffix path `next`. */
	@inline final def /[A, B <: Label](next :A / B)(implicit concat :ConcatLabelPath[P, A / B]) :concat.Path =
		concat.result

	/** Creates a new path being an inlined concatenation of this path and the suffix path `S`
	  * provided as an implicit parameter. */
	@inline final def /[S](implicit suffix :LabelPath[S], concat :ConcatLabelPath[P, S]) :concat.Path = concat.result

	@inline final def split(implicit split :SplitLabelPath[P]) :(split.First, split.Suffix) =
		(split.first, split.suffix.path)

	override def toSeq :Seq[String] = toList

	override def toList :List[String] = {
		@tailrec def rec(path :LabelPath[_], acc :List[String]) :List[String] = (path : @unchecked) match {
			case prefix / last => rec(prefix, last::acc)
			case ~/(first) => first::acc
		}
		rec(this, Nil)
	}

}






object LabelPath {

	/** A type of string literals used to label mappings on the type level for ease of access.
	  * Importing a Label
	  */
	type Label = String with Singleton


	/** Pattern matching extractor for ''label paths'' consisting of a single `Label`.
	  * @see [[net.noresttherein.sugar.util.LabelPath.~/]]
	  */
	object Label {
		@inline def unapply[P](path :LabelPath[P]) :Opt[P with Label] = path match {
			case label: ~/[_] => Got(label.last.asInstanceOf[P with Label])
			case _ => Lack
		}

		@inline def unapply[P](path :P) :Opt[P with String with path.type] = path match {
			case label :String => Got(label.asInstanceOf[P with String with path.type])
			case _ => Lack
		}
	}



	/** Summons an implicit type class `LabelPath` for the path type `P`. */
	@inline def apply[P](implicit path :LabelPath[P]) :LabelPath[P] = path

	/** Retrieves an instance of the ''label path'' type `P` from the implicit type class `LabelPath`. */
	@inline def pathOf[P](implicit path :LabelPath[P]) :P = path.path

	/** Provides the type class instance for a presumed ''label path'' type `P`.
	  * @return `path` itself if it is an instance of `LabelPath`, a `~/` if it is an instance of `String`,
	  *         or `None` otherwise.
	  */
	def fromPath[P](path :P) :Opt[LabelPath[P]] = path match {
		case path :LabelPath[P @unchecked] => Got(path)
		case label :String => Got(new ~/[label.type](label).asInstanceOf[LabelPath[P]])
		case _ => Lack
	}

	/** Creates a `LabelPath` consisting of labels equal to the strings in the given collection, in the same order.
	  * @param path a non-empty collection of non-empty strings.
	  */
	@throws[IllegalArgumentException]("if path is empty or contains an empty string.")
	def fromSeq(path :Seq[String]) :LabelPath[_] = path match {
		case _ if path.isEmpty =>
			throw new IllegalArgumentException("Cannot create an empty path.")
		case list :LinearSeq[String] =>
			def rec(parent :LabelPath[_], rest :Seq[String]) :LabelPath[_] =
				if (rest.isEmpty) parent
				else {
					val hd = rest.head //stabilize path
					rec(parent / hd, rest.tail)
				}
			val hd = list.head //stabilize path
			rec(~/(hd), list.tail)
		case indexed :collection.IndexedSeq[String] =>
			def rec(parent :LabelPath[_], i :Int) :LabelPath[_] =
				if (i == indexed.length) parent
				else {
					val next = indexed(i) //stabilize path
					rec(parent / next, i + 1)
				}
			val hd = indexed.head
			rec(~/(hd), 1)
		case _ =>
			fromSeq(ArraySeq.unsafeWrapArray(path.toArray[String]))
	}

	/** Creates a `LabelPath` for a given `String` representation.
	  * If the separator character does not occur in the input `String`, the returned path will be an atomic instance
	  * ([[net.noresttherein.sugar.util.LabelPath.~/ ~/]]). Otherwise each its occurrence delimits the next
	  * label in the path.
	  * @param path a non empty `String`.
	  * @param separator any character to use as a separator (`/` if not specified).
	  */
	@throws[IllegalArgumentException]("if path is an empty String or contains two consecutive occurrences of the separator.")
	def parse(path :String, separator :Char = '/') :LabelPath[_] =
		if (path.length == 0)
			throw new IllegalArgumentException("Empty path.")
		else
			fromSeq(ArraySeq.unsafeWrapArray(path.split(separator)))

	/** Creates a `LabelPath` for a given `String` representation.
	  * If the separator character does not occur in the input `String`, the returned path will be an atomic instance
	  * ([[net.noresttherein.sugar.util.LabelPath.~/ ~/]]). Otherwise each its occurrence delimits the next
	  * label in the path.
	  * @param path a non empty `String`.
	  * @param separator any character to use as a separator (`/` if not specified).
	  */
	@throws[IllegalArgumentException]("if path is an empty String or contains two consecutive occurrences of the separator.")
	def parse(path :String, separator :String) :LabelPath[_] =
		if (path.length == 0)
			throw new IllegalArgumentException("Empty path.")
		else
			fromSeq(ArraySeq.unsafeWrapArray(path.split(Regex.quote(separator))))



	/** A common super type of [[net.noresttherein.sugar.util.LabelPath LabelPath]]
	  * and constructor object [[net.noresttherein.sugar.util.LabelPath.~/$ ~/]],
	  * serving as a 'possibly empty path'.
	  */
	sealed trait LabelPathPrefix extends Any with Serializable {
		/** The number of labels in this path. */
		def length :Int

		/** Creates a new path consisting of this path followed by the given label. */
		def /[N <: Label](next :N) :LabelPath[_]

		def toSeq :Seq[String]
		def toList :List[String]
		def reverseList :List[String]
	}


	/** The `LabelPath` type class for a single label `L`. There is an implicit conversion wrapping any
	  * `String` literal type `L` in this class.
	  */
	class ~/[L <: Label] private[LabelPath] (val last :L) extends AnyVal with LabelPath[L] {
		def path :L = last

		override def first :L = last
		override def length :Int = 1

		/** Creates a new path consisting of this label followed by the given label. */
		@inline override def /[N <: Label](next :N)  :L / N = LabelPath./(last, next)

		override def reverseList :List[String] = last::Nil
		override def toString :String = "~/" + last
	}

	/** An empty `LabelPathPrefix` and a factory/pattern for single element paths. */
	object ~/ extends LabelPathPrefix {
		def apply[N <: Label](label :N): ~/[N] =
			if (label.length == 0)
				throw new IllegalArgumentException("Cannot create a LabelPath for an empty label.")
			else
				new ~/(label)

		override def /[N <: Label](label :N) : ~/[N] = apply(label)

		def unapply[P](path :LabelPath[P]) :Opt[P with Label] = path match {
			case atom: ~/[_] => Got(atom.last.asInstanceOf[P with Label])
			case _ => Lack
		}

		override def length = 0
		override def toSeq  :Seq[Nothing]  = toList
		override def toList :List[Nothing] = Nil
		override def reverseList :List[Nothing] = Nil
	}



	/** A composite label path type consisting of a prefix label path type `P` (either a label or another composite path)
	  * and the last label `L`. It is also its own type class `LabelPath[P/L]`.
	  * @tparam P the prefix path type of this path (one for which an implicit type class `LabelPath[P]` exists).
	  * @tparam L the label being the last element of this path.
	  */
	sealed trait /[P, L <: Label] extends LabelPath[P / L] {
		def path :P / L = this
		def init :LabelPath[P]
		def last :L
		def prefix :P = init.path
		def label :L = last
		override def first :Label = init.first
		override def length :Int = init.length + 1

		@inline final override def /[N <: Label](next :N) : P / L / N = LabelPath./(this, next)

		override def reverseList :List[String] = last::init.reverseList

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case path: /[_, _] => path.label == label && path.prefix == prefix
			case _ => false
		}
		override def hashCode :Int = prefix.hashCode * 31 + last.hashCode

		def format(separator :String) :String = {
			def rec(path :Any = this, capacity :Int = 0) :StringBuilder = path match {
				case composite: /[_, _] =>
					rec(composite.prefix, capacity + separator.length + composite.label.length) ++=
						separator ++= composite.label
				case _ =>
					val string = path.toString
					new StringBuilder(capacity + string.length) ++= string
			}
			rec().toString
		}

		override def toString :String = format("/")
	}

	private class Composite[P, L <: Label](override val init :LabelPath[P], override val last :L) extends /[P, L] {
		if (last.length == 0)
			throw new IllegalArgumentException("Empty path element: " + init + "/" + "''.")
	}



	object / {
//		def apply[L <: Label](label :L): ~/[L] = ~/(label)

		def apply[P, L <: Label](first :LabelPath[P], last :L) :P / L =
			new Composite[P, L](first, last)

		def unapply[P, L <: Label](path :P / L) :Opt[(LabelPath[P], L)] = path.prefix match {
			case label :String => Got((~/[label.type](label).asInstanceOf[LabelPath[P]], path.label))
			case prefix :LabelPath[P @unchecked] => Got((prefix, path.label))
			case _ => Lack //should never happen, unless someone messes with new implementations
		}

		def unapply(path :LabelPath[_]) :Opt[(LabelPath[_], Label)] = path match {
			case split: /[p, l] => unapply[p, l](split)
			case _ => Lack
		}
	}




	/** An implicit ''conversion'' from a string singleton to a single element path. Can be also used to create
	  * instances of single label type classes `~/[L]` manually: `LabelPath / "familiar"`. The conversion allows
	  * to use [[net.noresttherein.sugar.util.LabelPath!./ LabelPath./]] as an extension method of `Label`
	  * to create composite paths: `"familiar" / "name"`.
	  */
	@inline implicit def Label[L <: Label](label :L): ~/[L] = ~/(label)

	/** An implicit type class `LabelPath` for a `Label`. */
	@inline implicit def atomicLabelPath[L <: Label :ValueOf]: ~/[L] = new ~/[L](valueOf[L])

	/** An implicit type class `LabelPath` for itself. */
	@inline implicit def compositeLabelPath[A :LabelPath, B <: Label :ValueOf] :A / B =
		/(implicitly[LabelPath[A]], valueOf[B])



	/** Implicit value concatenating to path types `A` and `B` (types for which `LabelPath` type classes exist).
	  * The result of the concatenation is another path type (an instance of `/`) specified by the `Path` member type.
	  * In order to avoid creating to stacks of objects - one of these implicit witnesses and the path being
	  * the result of this concatenation - instances of this trait are actually also instances
	  * of the concatenated `Path`.
	  */
	sealed trait ConcatLabelPath[A, B] {
		/** The result of concatenating label path types `A` and `B`. */
		type Path >: this.type
		@inline final def result :Path = this
		def typeClass :LabelPath[Path]
	}

	@inline implicit def concatWithLabel[A :LabelPath, B <: Label :ValueOf] :ConcatLabelPath[A, B] { type Path = A / B } =
		new Composite[A, B](LabelPath[A], valueOf[B]) with ConcatLabelPath[A, B] {
			override type Path = A / B
			@inline override def typeClass :A / B = this
		}

	@inline implicit def concatWithPath[A, B, C <: Label :ValueOf](implicit concat :ConcatLabelPath[A, B])
			:ConcatLabelPath[A, B / C] { type Path = concat.Path / C } =
		new Composite[concat.Path, C](concat.typeClass, valueOf[C]) with ConcatLabelPath[A, B / C] {
			override type Path = concat.Path / C
			@inline override def typeClass :concat.Path / C = this
		}


	/** Implicit value splitting a path in the form of `L0 /.../ Ln` for some `n >= 2` into its ''first'' label `L0`
	  * and a `LabelPath[L1 / ... / Ln]`.
	  */
	sealed abstract class SplitLabelPath[P] {
		type First <: Label
		type Suffix

		def first :First
		def suffix :LabelPath[Suffix]
	}


	@inline implicit def splitIntoTwoLabels[A <: Label, B <: Label](implicit a :ValueOf[A], b :ValueOf[B])
			:SplitLabelPath[A / B] { type First = A; type Suffix = B } =
		new SplitLabelPath[A/B] {
			override type First = A
			override type Suffix = B
			val first = a.value
			val suffix = ~/(b.value)
		}

	@inline implicit def splitIntoLabelAndPath[P, L <: Label](implicit split :SplitLabelPath[P], last :ValueOf[L])
			:SplitLabelPath[P / L] { type First = split.First; type Suffix = split.Suffix / L } =
		new SplitLabelPath[P / L] {
			override type First = split.First
			override type Suffix = split.Suffix / L
			val first = split.first
			val suffix = new Composite(split.suffix, last.value)
		}

}
