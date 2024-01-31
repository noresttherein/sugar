package net.noresttherein.sugar.util

import scala.annotation.{showAsInfix, tailrec}
import scala.collection.immutable.ArraySeq
import scala.collection.LinearSeq
import scala.util.matching.Regex

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.extensions.{cast2TypeParamsMethods, castTypeParamMethods, castingMethods, classNameMethods}
import net.noresttherein.sugar.{illegal_!, unsupported_!}
import net.noresttherein.sugar.util.LabelPath.{/, Concat, Label, LabelPathPrefix, Split, ~/}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** A type class marking type `P` as a ''label path'' - a sequence of one or more `Label`s (string literal types)
  * separated with a `/` (a composite `LabelPath` class). The label path types, as understood here,
  * are defined as a union of of
  *   1. All `String` literal types, referred typically
  *      through type alias [[net.noresttherein.sugar.util.LabelPath.Label Label]]s;
  *   1. All types `P `[[net.noresttherein.sugar.util.LabelPath./ /]]` L` such that
  *      `P` is a label path type and `L` is a `Label` (a `String` literal type).
  *
  * Aside from its implicit witness nature, this type class actually provides the path instance it is parameterized with.
  * It is used in places where an identifier must be composed of label identifiers of individual identifiers,
  * typically to access subcomponents of some component indexed with labels. There are two subtypes of this type:
  *   1. [[net.noresttherein.sugar.util.LabelPath.~/ ~/]], the type class for any
  *      `String` literal type representing an atomic label;
  *   1. [[net.noresttherein.sugar.util.LabelPath./ /]], the type of compound paths composed of several labels.
  *      `A / B / C` is a type class for itself, `A / B / C`.
  *      This class/type class duality comes from the dual use cases of path types: indexing by values and indexing
  *      purely on type level, and from the desire for uniform interface of single labels and compound paths.
  *      The polymorphism provided by the type class interface is unfortunately limited to cases where the label type
  *      is specified explicitly, as the compiler will never infer a literal type in place of `String`
  *      unless a singleton type is expected, introducing the need for explicit overloads of methods accepting a `Label`
  *      and a `LabelPath`. Importing type [[net.noresttherein.sugar.util.LabelPath.Label Label]] type
  *      imports also an implicit conversion from any string literal to `~/`, allowing their use directly
  *      as `LabelPath` values.
  *
  * Instances can be either summoned, or created,
  * starting with object [[net.noresttherein.sugar.util.LabelPath.~/ ~/]] and continuing with
  * [[net.noresttherein.sugar.util.LabelPath.LabelPathPrefix./ /]].
  *
  * Sometimes, when recursively building a path, it is convenient to represent 'an empty path'.
  * [[net.noresttherein.sugar.util.LabelPath.LabelPathPrefix LabelPathPrefix]] exists for this purpose, introducing
  * minimal interface for path expansion. It has as subclasses this trait and the previously mentioned object.
  * @tparam P a path type - either a `Label`, or one or more `/` instances appending new labels to the first one.
  * @see [[net.noresttherein.sugar.util.LabelPath.Label]]
  * @see [[net.noresttherein.sugar.util.LabelPath./]]
  */ //consider: making it a value class wrapping a RelayArray, like in Scala 3 tuples.
sealed trait LabelPath[P] extends Any with LabelPathPrefix {

	/** The instance of the path type `P` */
	def path :P
	//todo: make these two extension methods, so they have specific types if P is known.
	/** The last label in this path. */
	def last :Label

	/** The first label in this path. This method runs in O(n). */
	final def first :Label = {
		@tailrec def rec(path :LabelPath[_]) :Label = (path : @unchecked) match {
			case prefix / _ => rec(prefix)
			case ~/(first)  => first
		}
		rec(this)
	}

	/** This path without the last element; [[net.noresttherein.sugar.util.LabelPath.~/! ~/]]`[_]`
	  * returns object [[net.noresttherein.sugar.util.LabelPath.~/$ ~/]], while a longer path
	  * `P `[[net.noresttherein.sugar.util.LabelPath./ /]]` L` returns `LabelPath[P]`.
	  */
	def init :LabelPathPrefix

	/** Creates a new path consisting of this path followed by the given label. */
	@inline final def /[N <: Label](next :N) :P / N = new /(this, next)

	/** Creates a new path being an inlined concatenation of this path and the given suffix path `next`.
	  * The return type is calculated by the implicit `Concat` type class based on types `P` and `S`.
	  * This type class is always present, even if `P` and/or `S` are abstract types, in which case
	  * the return type is `LabelPath[_]`.
	  */
	@inline final def /[S](next :LabelPath[S])(implicit concat :Concat[P, S]) :concat.Path =
		concat.safe(this, next)

	/** Splits this path into its ''first'' label, and a path with the following labels. */
	@inline final def split(implicit split :Split[P]) :(split.First, split.Suffix) =
		split(path)
}






object LabelPath {

	/** A type of string literals used to label mappings on the type level for ease of access.
	  * Importing symbol `Label` at the  same time imports an implicit conversion
	  * `N <: Label => `[[net.noresttherein.sugar.util.LabelPath.~/ ~/]]`[N]`.
	  */
	type Label = String with Singleton


	/** Pattern matching extractor for ''label paths'' consisting of a single `Label`.
	  * @see [[net.noresttherein.sugar.util.LabelPath.~/]]
	  */
	object Label {
		@inline def unapply[P](path :LabelPath[P]) :Maybe[P with Label] = path match {
			case label: ~/[_] => Yes(label.last.asInstanceOf[P with Label])
			case _ => No
		}

		@inline def unapply[P](path :P) :Maybe[P with String with path.type] = path match {
			case label :String => Yes(label.asInstanceOf[P with String with path.type])
			case _ => No
		}
	}



	/** Summons an implicit type class `LabelPath` for the path type `P`. */
	@inline def apply[P](implicit path :LabelPath[P]) :LabelPath[P] = path

	/** Retrieves an instance of the ''label path'' type `P` from the implicit type class `LabelPath`. */
	@inline def pathOf[P](implicit path :LabelPath[P]) :P = path.path

	/** Provides the type class instance for a presumed ''label path'' type `P`.
	  * @return `path` itself if it is an instance of `LabelPath`, a `~/` if it is an instance of `String`,
	  *         or an empty `Maybe` otherwise.
	  */
	def fromPath[P](path :P) :Maybe[LabelPath[P]] = path match {
		case path :LabelPath[P @unchecked] => Yes(path)
		case label :String => Yes(new ~/[label.type](label).castFrom[~/[label.type], LabelPath[P]])
		case _ => No
	}

	/** Creates a `LabelPath` consisting of labels equal to the strings in the given collection, in the same order.
	  * @param path a non-empty collection of non-empty strings.
	  */
	@throws[IllegalArgumentException]("if path is empty or contains an empty string.")
	def fromSeq(path :Seq[String]) :LabelPath[_] = path match {
		case _ if path.isEmpty =>
			illegal_!("Cannot create an empty path.")
		case list :LinearSeq[String] =>
			@tailrec def rec(parent :LabelPath[_], rest :Seq[String]) :LabelPath[_] =
				if (rest.isEmpty)
					parent
				else {
					val hd = rest.head //stabilize path
					rec(parent / (hd :hd.type), rest.tail)
				}
			val hd = list.head //stabilize path
			rec(~/[hd.type](hd), list.tail)
		case indexed :collection.IndexedSeq[String] =>
			@tailrec def rec(parent :LabelPath[_], i :Int) :LabelPath[_] =
				if (i == indexed.length)
					parent
				else {
					val next = indexed(i) //stabilize path
					rec(parent / (next :next.type), i + 1)
				}
			val hd = indexed.head
			rec(~/(hd :hd.type), 1)
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
			illegal_!("Empty path.")
		else
			fromSeq(ArraySeq.unsafeWrapArray(path.split(separator)))

	/** Creates a `LabelPath` for a given `String` representation.
	  * If the separator character does not occur in the input `String`, the returned path will be an atomic instance
	  * ([[net.noresttherein.sugar.util.LabelPath.~/ ~/]]). Otherwise each its occurrence delimits the next
	  * label in the path.
	  * @param path a non empty `String`.
	  * @param separator any character to use as a separator.
	  */
	@throws[IllegalArgumentException]("if path is an empty String or contains two consecutive occurrences of the separator.")
	def parse(path :String, separator :String) :LabelPath[_] =
		if (path.length == 0)
			illegal_!("Empty path.")
		else
			fromSeq(ArraySeq.unsafeWrapArray(path.split(Regex.quote(separator))))


	/** A common super type of [[net.noresttherein.sugar.util.LabelPath LabelPath]]
	  * and factory object [[net.noresttherein.sugar.util.LabelPath.~/$ ~/]],
	  * serving as a 'possibly empty path'.
	  */
	sealed trait LabelPathPrefix extends Any with Serializable {
		/** True if this prefix is non empty, i.e., it is a [[net.noresttherein.sugar.util.LabelPath LabelPath]]. */
		final def nonEmpty :Boolean = this.isInstanceOf[LabelPath[_]]

		/** True if this prefix is empty, i.e., it equals [[net.noresttherein.sugar.util.LabelPath.~/ ~/]]. */
		@inline final def isEmpty :Boolean = !nonEmpty

		/** The number of labels in this path. */
		final def length :Int = this match {
			case path :LabelPath[_] =>
				@tailrec def rec(path :LabelPath[_], length :Int) :Int = (path : @unchecked) match {
					case prefix / _ => rec(prefix, length + 1)
					case ~/(_)      => 1
				}
				rec(path, 0)
			case ~/ => 0
		}


		/** A prefix of this path consisting of
		  * `this.`[[net.noresttherein.sugar.util.LabelPath.LabelPathPrefix.length length]]` - labels` labels.
		  */
		@throws[IllegalArgumentException]("if labels is negative or greater than this.length.")
		def pop(labels :Int) :LabelPathPrefix = {
			if (labels < 0)
				illegal_!("Cannot drop a " + labels + " umber of labels from " + this + ".")
			@tailrec def rec(path :LabelPathPrefix, n :Int) :LabelPathPrefix =
				(path: @unchecked) match {
					case _ if n == 0 => path
					case prefix / _ => rec(prefix, n - 1)
					case _ => throw EmptyPathException
				}
			try rec(this, labels) catch {
				case EmptyPathException =>
					illegal_!("Cannot drop " + labels + " labels from " + this + ".")
			}
		}

		/** A sequence of labels in this path/path prefix, starting with the first one (on the bottom), and ending
		  * with the last (outer) label.
		  */
		final def toSeq :Seq[Label] = this match {
			case path :LabelPath[_] =>
				val res = new Array[Label](length)
				@tailrec def rec(prefix :LabelPath[_], i :Int) :Unit = (prefix: @unchecked) match {
					case init / last => res(i) = last; rec(init, i - 1)
					case ~/(first)   => res(i) = first
				}
				rec(path, res.length - 1)
				ArraySeq.unsafeWrapArray(res)
			case _ =>
				ArraySeq.empty
		}

		/** A sequence of labels in this path/path prefix, starting with the first one (on the bottom), and ending
		  * with the last (outer) label.
		  */
		final def toList :List[Label] = this match {
			case path :LabelPath[_] =>
				@tailrec def rec(path :LabelPath[_], acc :List[Label]) :List[Label] = (path : @unchecked) match {
					case prefix / last => rec(prefix, last::acc)
					case ~/(first)     => first::acc
				}
				rec(path, Nil)
			case ~/ => Nil
		}

		/** A sequence of labels in this path/path prefix, starting with the last one (on the top), and ending
		  * with the last (most nested) label.
		  */
		final def reverseSeq :Seq[Label] = reverseList

		/** A list of labels in this path/path prefix, starting with the last one (on the top), and ending
		  * with the last (most nested) label.
		  */
		final def reverseList :List[Label] = this match {
			case path :LabelPath[_] =>
				def rec(path :LabelPath[_]) :List[Label] = (path : @unchecked) match {
					case prefix / last => last::rec(prefix)
					case ~/(first)     => first::Nil
				}
				rec(path)
			case ~/ => Nil
		}

		/** Equivalent to `toList.mkString(prefix, separator, "")`. */
		final def format(prefix :String, separator :String = "/") :String = this match {
			case path :LabelPath[_] =>
				def rec(path :LabelPath[_], length :Int) :JStringBuilder =
					(path : @unchecked) match {
						case prefix / last =>
							rec(prefix, separator.length + last.length + length) append separator append last
						case ~/(first) =>
							new JStringBuilder(prefix.length + first.length + length) append prefix append first
					}
				rec(path, 0).toString
			case ~/ => prefix
		}

		final override def toString :String = format("~/", "/")
	}


	@SerialVersionUID(Ver)
	object LabelPathPrefix {
		/** Creates a `LabelPathPrefix` consisting of labels equal to the strings in the given collection,
		  * in the same order.
		  */
		@throws[IllegalArgumentException]("if path contains an empty string.")
		def fromSeq(path :Seq[String]) :LabelPathPrefix =
			if (path.isEmpty) ~/
			else LabelPath.fromSeq(path)

		/** Creates a `LabelPrefix` for a given `String` representation.
		  * If the string is empty, or consists only of the separator, [[net.noresttherein.sugar.util.LabelPath.~/$ ~/]]
		  * is returned. Otherwise, if the separator character does not occur in the input `String`,
		  * the returned path will be an atomic instance ([[net.noresttherein.sugar.util.LabelPath.~/ ~/]]).
		  * Otherwise each its occurrence delimits the next label in the path.
		  */
		@throws[IllegalArgumentException]("if path contains two consecutive occurrences of the separator.")
		def parse(path :String, separator :Char = '/') :LabelPathPrefix =
			if (path.length == 0 || path.length == 1 && path.charAt(0) == separator)
				~/
			else
				LabelPath.parse(path, separator)

		/** Creates a `LabelPrefix` for a given `String` representation.
		  * If the string is empty, or consists only of the separator, [[net.noresttherein.sugar.util.LabelPath.~/$ ~/]]
		  * is returned. Otherwise, if the separator character does not occur in the input `String`,
		  * the returned path will be an atomic instance ([[net.noresttherein.sugar.util.LabelPath.~/ ~/]]).
		  * Otherwise each its occurrence delimits the next label in the path.
		  */
		@throws[IllegalArgumentException]("if path is an empty String or contains two consecutive occurrences of the separator.")
		def parse(path :String, separator :String) :LabelPathPrefix =
			if (path.length == 0 || path == separator)
				~/
			else
				LabelPath.parse(path, separator)


		//As extension methods, because due to simpler signatures, they confuse the compiler
		// if placed in LabelPathPrefix and left operand is a LabelPath
		/** Extension methods for [[net.noresttherein.sugar.util.LabelPath.LabelPathPrefix LabelPathPrefix]]
		  * implementing unchecked concatenation.
		  */
		implicit class LabelPathPrefixExtension(private val self :LabelPathPrefix) extends AnyVal {
			/** Creates a new path consisting of this path followed by the given label. */
			def /(next :Label) :LabelPath[_] =
				Concat.unsafeConcat[LabelPathPrefix, Label].path(self, next)

			/** Creates a new path consisting of this path followed by the given path. */
			def /(next :LabelPath[_]) :LabelPath[_] =
				Concat.unsafeConcat[LabelPathPrefix, LabelPath[_]].path(self, next)

			/** Creates a new path consisting of this path followed by the given path or an empty prefix. */
			def /(next :LabelPathPrefix) :LabelPathPrefix =
				Concat.unsafeConcat[LabelPathPrefix, LabelPathPrefix].unchecked(self, next)
		}


		@SerialVersionUID(Ver)
		implicit object ordering extends math.Ordering[LabelPathPrefix] {
			@tailrec private def dropRight(path :LabelPathPrefix, n :Int) :LabelPathPrefix =
				(path : @unchecked) match {
					case _ if n <= 0 => path
					case prefix / _ => dropRight(prefix, n - 1)
					case ~/(_) if n == 1 => ~/
				}

			@tailrec private def cmp(x :LabelPathPrefix, y :LabelPathPrefix, onEqual :Int) :Int =
				((x, y) : @unchecked) match {
					case (prefix1 / last1, prefix2 / last2) => (last1 compare last2) match {
						case 0 => cmp(prefix1, prefix2, onEqual)
						case n => cmp(prefix1, prefix2, n)
					}
					case (~/(a), ~/(b)) => (a compare b) match {
						case 0 => onEqual
						case n => n
					}
					case (LabelPath.~/, LabelPath.~/) => onEqual
				}

			override def compare(x :LabelPathPrefix, y :LabelPathPrefix) :Int = {
				x.length - y.length match {
					case 0 => cmp(x, y, 0)
					case n if n > 0 => cmp(dropRight(x, n), y, 1)
					case n => cmp(x, dropRight(y, -n), -1)
				}
			}
		}
	}


	/** The `LabelPath` type class for a single label `L`. There is an implicit conversion wrapping any
	  * `String` literal type `L` in this class. Note that appending another label
	  * with `this `[[net.noresttherein.sugar.util.LabelPath.~/./ /]]` K` creates an instance
	  * of `L `[[net.noresttherein.sugar.util.LabelPath./ /]]` K`, rather than `~/[L] / K`.
	  */
	@SerialVersionUID(Ver)
	class ~/[L <: Label] private[LabelPath] (override val path :L) extends AnyVal with LabelPath[L] {
		override def init: ~/.type = ~/
		override def last: L = path
	}

	/** An empty `LabelPathPrefix` and a factory/pattern for single element paths.
	  * In a match pattern, it can be either used as a value, to match itself, or as a pattern for a single `Label`,
	  * matching `~/[L]`. It never occurs as an actual prefix in a non empty path, it is always replaced
	  * with `~/[L]` when appending a label `L`.
	  */ //consider: a different name, because it is never an actual path prefix, just a stand alone quasi path
	@SerialVersionUID(Ver)
	object ~/ extends LabelPathPrefix {
		/** Creates a singleton `LabelPath` for the given `Label`. */
		def apply[N <: Label](label :N): ~/[N] =
			if (label.length == 0)
				illegal_!("Cannot create a LabelPath for an empty label.")
			else
				new ~/(label)

		/** Creates a singleton `LabelPath` for the given `Label`. Same as `apply`. */
		@inline def /[N <: Label](label :N) : ~/[N] = apply(label)

		def unapply[P](path :LabelPath[P]) :Maybe[P with Label] = path match {
			case atom: ~/[_] => Yes(atom.last.asInstanceOf[P with Label])
			case _ => No
		}

		def unapply(path :LabelPathPrefix) :Maybe[Label] = path match {
			case atom: ~/[_]  => Yes(atom.last)
			case _ => No
		}
	}



	/** A composite label path type consisting of a prefix label path type `P` (either a label or another composite path)
	  * and the last label `L`. It is also its own type class `LabelPath[P/L]`.
	  * @tparam P the prefix path type of this path (one for which an implicit type class `LabelPath[P]` exists).
	  * @tparam L the label being the last element of this path.
	  */
	@SerialVersionUID(Ver) @showAsInfix
	final class /[P, L <: Label] private[util] (override val init :LabelPath[P], override val last :L)
		extends LabelPath[P / L]
	{
		if (last.length == 0)
			illegal_!("Empty path element: " + init + "/" + "''.")

		def prefix :P = init.path

		override def path :P / L = this

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case path: /[_, _] => path.last == last && path.prefix == prefix
			case _ => false
		}
		override def hashCode :Int = prefix.hashCode * 31 + last.hashCode
	}



	/** Factory and matching pattern of composite [[net.noresttherein.sugar.util.LabelPath paths]],
	  * appending a single [[net.noresttherein.sugar.util.LabelPath.Label label]] to a prefix path.
	  */
	 /* Note that the extracted values differ depending on the matched type:
	  * {{{
	  *     val x :"stairway" / "heaven" = "stairway" / "heaven"
	  *     val pair :Option[(String, String)] = x match {
	  *         case first / last => Some((first, last)) //the left side of `/` is of the the left type parameter type
	  *         case _ => None
	  *     }
	  *     val pair2 :Option[(String, String)] = (x :LabelPath[_]) match {
	  *         case ~/(first) / last => Some((first, last)) //the left side of `/` is a LabelPath[_]
	  *         case _ => None
	  *     }
	  * }}}
	  */
	@SerialVersionUID(Ver)
	object / {
		def apply[P, L <: Label](first :LabelPath[P], last :L) :P / L =
			new /[P, L](first, last)

		def unapply[P, L <: Label](path :P / L) :Maybe[(LabelPath[P], L)] = path.prefix match {
			case label :String => Yes((~/[label.type](label).asInstanceOf[LabelPath[P]], path.last))
			case prefix :LabelPath[P @unchecked] => Yes((prefix, path.last))
			case _ => No //should never happen, unless someone messes with new implementations
		}

		def unapply(path :LabelPathPrefix) :Maybe[(LabelPath[_], Label)] = path match {
			case split: /[p, l] => unapply[p, l](split)
			case _ => No
		}
	}




	/** An implicit conversion from a string singleton to a single element path. Can be also used to create
	  * instances of single label type classes `~/[L]` manually: `LabelPath / "familiar"`. The conversion allows
	  * to use [[net.noresttherein.sugar.util.LabelPath!./ LabelPath./]] as an extension method of `Label`
	  * to create composite paths: `"familiar" / "name"`.
	  */
	@inline implicit def Label[L <: Label](label :L): ~/[L] = ~/(label)

	/** An implicit type class `LabelPath` for a `Label`. */
	@inline implicit def atomicLabelPath[L <: Label :ValueOf]: ~/[L] = ~/(valueOf[L])

	/** An implicit type class `LabelPath` for itself. */
	@inline implicit def compositeLabelPath[A :LabelPath, B <: Label :ValueOf] :A / B =
		/(implicitly[LabelPath[A]], valueOf[B])




	/** Implicit value concatenating to path types `A` and `B` (types for which `LabelPath` type classes exist).
	  * The result of the concatenation is another path type (an instance of `/`) specified by the `Path` member type.
	  * In order to avoid creating to stacks of objects - one of these implicit witnesses and the path being
	  * the result of this concatenation - instances of this trait are actually also instances
	  * of the concatenated `Path`.
	  */
	sealed trait Concat[A, B] extends Serializable {
		/** The result of concatenating label path types `A` and `B`. If both `A` are either a `Label` or a `LabelPath`,
		  * then `Result <:< LabelPath[Result]` and
		  * `Result =:= `[[net.noresttherein.sugar.util.LabelPath.Concat.Path Path]]. However, if either is
		  * an empty path prefix [[net.noresttherein.sugar.util.LabelPath.~/$ ~/.type]], then it might also be
		  * a `Label`, or even `~/.type`.
		  */
		type Result
		/** The type class for the concatenated path. If `A` and `B` are both either a `Label`s or a `LabelPath`,
		  * then this type equals `Result`.
		  */
		type Path <: LabelPath[Result]

		def apply(first :A, second :B) :Result

		/** The type class for the path value returned by `apply`. When concatenating proper `LabelPath` instances
		  * (including `Label`s), this is equivalent to `apply`, but the return type is a `LabelPath`
		  * even in an unchecked (abstract) context.
		  */
		@throws[UnsupportedOperationException]("if A =:= ~/.type and B =:= ~/.type.")
		def path(first :A, second :B) :Path

		/** Unchecked and safe concatenation working for any two `LabelPathPrefix` or `Label` instances. */
		def unchecked(first :A, second :B) :LabelPathPrefix

		/** Same as [[net.noresttherein.sugar.util.LabelPath.Concat.path path]]`(first.path, second.path)`,
		  * but accepting the arguments as type classes guarantees that no exception is thrown, as this excludes
		  * the possibility of providing an empty prefix as an argument.
		  */
		@inline final def safe(first :LabelPath[A], second :LabelPath[B]) :Path = path(first.path, second.path)
	}

	private[LabelPath] sealed abstract class UnsafeConcats extends Serializable {
		/** A non-typed witness implementing concatenation of paths `A` and `B`.
		  * Valid arguments are all [[net.noresttherein.sugar.util.LabelPath.LabelPathPrefix LabelPathPrefix]] subtypes
		  * and [[net.noresttherein.sugar.util.LabelPath.Label Label]] (`String` literal types).
		  * Note that, in case of concatenation of [[net.noresttherein.sugar.util.LabelPath.~/ ~/]] with itself,
		  * the returned evidence's property [[net.noresttherein.sugar.util.LabelPath.Concat.typeClass typeClass]]
		  * will throw an [[UnsupportedOperationException]].
		  */
		implicit def unsafeConcat[A, B] :Concat[A, B] = unsafeConcatPrototype.castFrom[Concat[Any, Any], Concat[A, B]]

		private object unsafeConcatPrototype extends Concat[Any, Any] {
			override type Result = Any
			override type Path = LabelPath[Any]

			override def apply(first :Any, second :Any) :Result =
				(second: @unchecked) match {
					case LabelPath.~/ => first match {
						case path  :LabelPathPrefix => path //all implementation extend a Concat instance
						case label :String => label
						case _ => illegal_!(
							"Second argument is neither a LabelPathPrefix nor a Label (String): " + second + "."
						)
					}
					case prefix: LabelPath[_] if first == ~/ => //together with the previous case covers all LabelPathPrefix subtypes
						prefix
					case label:  String if first == ~/ =>
						~/(label)
					case p: /[_, _] => //init must be a path, so a non-empty Concat with non-throwing typeClass is returned by concat
						path(first, p.init) / p.last
					case single: ~/[_]   => cat(first, single)
					case label:  String  => cat(first, ~/[label.type](label))
				}

			def cat(first: Any, second: ~/[_ <: Label]) = first match {
				case path :LabelPath[_] => path / second.path
				case LabelPath.~/ => second
				case other :String => ~/(other) / second.path
				case _ =>
					illegal_!(
						"The first argument is neither a LabelPathPrefix nor a Label (String): " + first + "."
					)
			}

			override def unchecked(first :Any, second :Any) :LabelPathPrefix =
				(apply(first, second): @unchecked) match {
					case prefix :LabelPathPrefix => prefix
					case label :String => ~/(label)
				}

			override def path(first :Any, second :Any) = apply(first, second) match {
				case path  :LabelPath[_] => path.castParam[Result]
				case label :String => ~/[label.type](label).castFrom[~/[label.type], LabelPath[Any]]
				case _ => unsupported_!(
					"No LabelPath type class for an empty path prefix ~/"
				)
			}
		}
	}

	private[LabelPath] sealed abstract class UntypedConcats extends UnsafeConcats {
		implicit def untypedConcatLabelWithPath[A <: Label, B <: LabelPath[_]]
				:Concat[A, B] { type Result = Path; type Path <: /[_, _ <: Label] } =
			untypedConcatPrototype.asInstanceOf[Concat[A, B] { type Result = Path; type Path <: /[_, _ <: Label] }]

		implicit def untypedConcatPathWithPath[A <: LabelPath[_], B <: LabelPath[_]]
				:Concat[A, B] { type Result = Path; type Path <: /[_, _ <: Label] } =
			untypedConcatPrototype.asInstanceOf[Concat[A, B] { type Result = Path; type Path <: /[_, _ <: Label] }]

		@SerialVersionUID(Ver)
		private object untypedConcatPrototype extends Concat[Any, LabelPath[_]] {
			type Result = Any / Label
			type Path = Any / Label

			override def apply(first :Any, second :LabelPath[_]) :Path = ((second : @unchecked) match {
				case init / last => apply(first, init) / last
				case single: ~/[_] => single / single
			}).asInstanceOf[Path]
			override def path(first :Any, second :LabelPath[_]) :Any / Label = apply(first, second)
			override def unchecked(first :Any, second :LabelPath[_]) :LabelPathPrefix = apply(first, second)
		}
	}

	@SerialVersionUID(Ver)
	object Concat extends UntypedConcats {
		implicit def concatEmptyWithEmpty :Concat[~/.type, ~/.type] { type Result = ~/.type; type Path = Nothing } =
			concatEmptyWithEmptyPrototype

		implicit def concatEmptyWithSingleton[A <: Label] :Concat[~/.type, ~/[A]] { type Result = A; type Path = ~/[A] } =
			concatEmptyWithPathPrototype.castFrom[
				Concat[~/.type, LabelPath[_]], Concat[~/.type, ~/[A]] { type Result = A; type Path = ~/[A] }
			]
		implicit def concatEmptyWithComposite[A <: LabelPath[A]] :Concat[~/.type, A] { type Result = A; type Path = A } =
			concatEmptyWithPathPrototype.castFrom[
				Concat[~/.type, LabelPath[_]], Concat[~/.type, A] { type Result = A; type Path = Result }
			]
		implicit def concatEmptyWithLabel[A <: Label] :Concat[~/.type, A] { type Result = A; type Path = ~/[A] } =
			concatEmptyWithLabelPrototype.castFrom[
				Concat[~/.type, Label], Concat[~/.type, A] { type Result = A; type Path = ~/[A] }
			]
		implicit def concatLabelWithLabel[A <: Label, B <: Label]
				:Concat[A, B] { type Result = A / B; type Path = Result } =
			concatLabelWithLabelPrototype.castFrom[
				Concat[Label, Label], Concat[A, B] { type Result = A / B; type Path = Result }
			]
		implicit def concatPathWithLabel[A <: LabelPath[A], B <: Label]
				:Concat[A, B] { type Result = A / B; type Path = A / B } =
			concatPathWithLabelPrototype.castFrom[
				Concat[LabelPath[_], Label], Concat[A, B] { type Result = A / B; type Path = Result }
			]
		implicit def concatSingletonWithLabel[A <: Label, B <: Label]
				:Concat[~/[A], B] { type Result = A / B; type Path = A / B } =
			concatPathWithLabelPrototype.castFrom[
				Concat[LabelPath[_], Label], Concat[~/[A], B] { type Result = A / B; type Path = Result }
			]
		//concat is refined to avoid depending on an unchecked type class which might throw an exception from path
		implicit def concatPathWithPath[A, B, C <: Label](implicit concat :Concat[A, B] { type Result <: LabelPath[_] })
				:Concat[A, B / C] { type Result = concat.Result / C; type Path = Result } =
			new Concat[A, B / C] {
				override type Result = concat.Result / C
				override type Path   = concat.Result / C
				override def apply(prefix :A, suffix :B / C) = concat.path(prefix, suffix.init.path) / suffix.last
				override def path(prefix :A, suffix :B / C) = apply(prefix, suffix)
				override def unchecked(prefix :A, suffix :B / C) = apply(prefix, suffix)
			}

		private abstract class ConcatEmpty[B] extends Concat[~/.type, B] {
			override type Result = B
			override def apply(prefix: ~/.type, suffix: B) = suffix
			override def unchecked(prefix: ~/.type, suffix: B) :LabelPathPrefix = path(prefix, suffix)
		}
		@SerialVersionUID(Ver)
		private object concatEmptyWithEmptyPrototype extends ConcatEmpty[~/.type] {
			override type Path = Nothing
			override def unchecked(prefix: ~/.type, suffix: ~/.type) = ~/
			override def path(prefix: ~/.type, suffix: ~/.type) =
				unsupported_!("No LabelPath type class for an empty path prefix ~/.")
		}
		@SerialVersionUID(Ver)
		private object concatEmptyWithPathPrototype extends ConcatEmpty[LabelPath[_]] {
			override type Path = LabelPath[Result]
			override def path(prefix: ~/.type, suffix: LabelPath[_]) = suffix.castParam[Result]
		}
		@SerialVersionUID(Ver)
		private object concatEmptyWithLabelPrototype extends ConcatEmpty[Label] {
			override type Path = ~/[Label]
			override def path(prefix: ~/.type, suffix: Label) = ~/(suffix)
		}
		private abstract class ConcatWithLabel[A, B <: Label] extends Concat[A, B] {
			override type Result <: LabelPath[Result]
			override type Path = Result
			override def path(first :A, second :B) = this(first, second)
			override def unchecked(first :A, second :B)  = this(first, second)
		}
		@SerialVersionUID(Ver)
		private object concatLabelWithLabelPrototype extends ConcatWithLabel[Label, Label] {
			override type Result = Label / Label
			override def apply(first :Label, second :Label) = ~/[Label](first) / second
		}
		@SerialVersionUID(Ver)
		private object concatPathWithLabelPrototype extends ConcatWithLabel[LabelPath[_], Label] {
			override type Result = Any / Label
			override def apply(first :LabelPath[_], second :Label) =
				(first / second).castFrom[_ / Label, Any / Label]
		}
	}




	/** Implicit type class splitting a path in the form of `L0 /.../ Ln` for some `n >= 0`
	  * into its ''first'' label `L0` and a `LabelPath[L1 / ... / Ln]`, unless `P <: Label`, in which case
	  * [[net.noresttherein.sugar.util.LabelPath.Split.Suffix Suffix]] `=:=` [[net.noresttherein.sugar.util.LabelPath.~/$.type ~/.type]].
	  */
	sealed abstract class Split[P] extends Serializable {
		type First <: Label
		type Suffix

		def first(path :P) :First
		@throws[UnsupportedOperationException]("if P is a Label (and Suffix is ~/.type).")
		def suffix(path :P) :LabelPath[Suffix]
		def apply(path :P) :(First, Suffix)
		def join(first :First, suffix :Suffix) :LabelPath[P]
	}


	private[LabelPath] sealed abstract class UntypedSplits {
		//We don't care that A may be anything, because the argument is always A / B, which enforces validity
		implicit def untypedSplitPath[A, B <: Label] :Split[A / B] =
			untypedSplitPathPrototype.asInstanceOf[Split[A / B]]

		@SerialVersionUID(Ver)
		private object untypedSplitPathPrototype extends Split[Any / Label] {
			override type First = Label
			override type Suffix = LabelPath[_]

			@tailrec override def first(path :Any / Label) :Label = (path.init: LabelPathPrefix @unchecked) match {
				case ~/(single) => single
				case path: /[Any, Label] @unchecked => first(path)
			}
			override def suffix(path :Any / Label) :LabelPath[Suffix] =
				((path.init: LabelPathPrefix @unchecked) match {
					case _: ~/[_] => ~/(path.last)
					case p: /[Any, Label] @unchecked => suffix(p) / path.last
				}).asInstanceOf[LabelPath[Suffix]]
			override def apply(path :Any / Label) :(Label, LabelPath[_]) =
				(path.init :LabelPathPrefix @unchecked) match {
					case ~/(single) => (single, ~/(path.last))
					case p: /[Any, Label] @unchecked =>
						val (first, rest) = apply(p)
						(first, rest / path.last)
				}
			override def join(first :Label, suffix :LabelPath[_]) =
				Concat.untypedConcatLabelWithPath[Label, LabelPath[_]].path(first, suffix).asInstanceOf[Any / Label]
		}
	}

	@SerialVersionUID(Ver)
	object Split extends UntypedSplits {
		implicit def splitIntoTwoLabels[A <: Label, B <: Label]
				:Split[A / B] { type First = A; type Suffix = B } =
			splitIntoTwoLabelsPrototype.castFrom[Split[Label / Label], Split[A / B] { type First = A; type Suffix = B }]

		implicit def splitIntoLabelAndPath[P, L <: Label](implicit split :Split[P], last :ValueOf[L])
				:Split[P / L] { type First = split.First; type Suffix = split.Suffix / L } =
			new Split[P / L] {
				override type First = split.First
				override type Suffix = split.Suffix / L
				override def first(path :P / L) = split.first(path.init.path)
				override def suffix(path :P / L) = split.suffix(path.init.path) / path.last

				override def apply(path :P / L) = (split(path.init.path): @unchecked) match {
					case (first :split.First @unchecked, rest :LabelPath[split.Suffix] @unchecked) =>
						(first, rest / path.last)
					case (first :split.First @unchecked, rest :String) =>
						(first, (~/(rest) / path.last).asInstanceOf[Suffix])
				}
				override def join(first :First, suffix :Suffix) = split.join(first, suffix.prefix) / last.value
				override def toString :String = split.toString + "/" + valueOf[L]
			}
		implicit def splitLabel[L <: Label] :Split[L] = splitLabelPrototype.castParam[L]

		@SerialVersionUID(Ver)
		private object splitIntoTwoLabelsPrototype extends Split[Label / Label] {
			override type First  = Label
			override type Suffix = Label
			override def first(path :Label / Label) = path.init.path
			override def suffix(path :Label / Label) = ~/(path.last)
			override def apply(path :Label / Label) = (path.init.path, path.last)
			override def join(first :First, suffix :Suffix) = (first :Label) / suffix
			override def toString = "Split[Label/Label]"
		}
		@SerialVersionUID(Ver)
		private object splitLabelPrototype extends Split[Label] {
			override type First = Label
			override type Suffix = ~/.type
			override def first(path :Label) = path
			override def suffix(path :Label) =
				unsupported_!("A label '" + path + "' has an empty suffix")
			override def apply(path :Label) = (path, ~/)
			override def join(first :Label, suffix: ~/.type) = first
			override def toString = "Split[Label]"
		}
	}


	@SerialVersionUID(Ver)
	implicit object ordering extends math.Ordering[LabelPath[_]] {
		override def compare(x :LabelPath[_], y :LabelPath[_]) :Int = LabelPathPrefix.ordering.compare(x, y)
	}



	private class EmptyPathException extends Exception("EmptyPath", null, false, false)
	private final val EmptyPathException = new EmptyPathException

}





//
//trait LabelTree
//
//object LabelTree {
//
//	case object Root {
//
//	}
//	type Root = Root.type
//
////	val \ : \ = new \
////
////	class \ private[LabelTree]
//
//	class |-:[R, +T] private[LabelTree] (val branch :R, val tree :T) extends LabelTree {
//		def sibling(implicit isTree :R <:< LabelTree) :R = branch
//		def label(isLabel :R <:< Label) :R = branch
//		def root(isRoot :R <:< Root)
//
//		def |-:[L <: Label](label :L) :L |-: R |-: T = new |-:(label, this)
//		def |-:[S <: LabelTree](sibling :S) :T |-: R |-: T = new |-:(sibling, this)
//		def |-:(root :Root)
//	}
//
////	class IsLabelTree[T] extends Serializable
//
//	class LeafExtension[V](private val leaf :V) extends AnyVal {
//		def |-:[L <: Label](label :L) :L |-: V = new |-:(label, leaf)
//		def |-:[T <: LabelTree](sibling :T) = new |-:(sibling, leaf)
//		def |-:(root :Root) :Root |-: V = new |-:(root, leaf)
//	}
//
//	final class LegalLeft[L] private [LabelTree] {
//		def apply[T](left :L, right :T) :L |-: T = new |-:(left, right)
//	}
//
///*
//
//
//	|- ("key" |- Int)
//	|- ("key2"
//	    |- ("a" |- String)
//	    |- ("b" |- String))
//    |- ("key3 |- Long)
//
//	Root
//	|-: ("key" |-: Int)
//	|-: ("key2"
//	    |-: "a" |-: String
//	    |-: "b" |-: String)
//    |-: "key3 |-: Long
//
//*/
//}


