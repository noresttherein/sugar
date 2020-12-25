package net.noresttherein.slang.tuples

import net.noresttherein.slang.tuples.Nat.{++, _0, _1, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _2, _20, _21, _22, _3, _4, _5, _6, _7, _8, _9}
import net.noresttherein.slang.tuples.Tuple.evidence.{TupleAt, TupleDrop, TupleDropRight, TupleLUB, TupleProjection, TupleTake, TupleTakeRight, UpdatedTuple}
import net.noresttherein.slang.typist.UpperBound

import scala.annotation.implicitNotFound
import scala.collection.immutable.ArraySeq
import scala.compat.Platform


//todo: use a macro to elide the implicit witnesses
//todo: zipper; maybe lenses

/** Generic base trait for tuples of varying number of elements. It is functionally very similar to ''shapeless' '' `HList`,
  * but is conceptually closer to tuples than lists. The main differences are:
  *  - they are left associative, making left-to-right writing more natural;
  *  - they are indexed starting with `1`, like scala tuples, but unlike lists and other collections;
  *  - indexing only works if the type is fully known;
  *  - they are backed by arrays instead of lists, with `O(1)` element retrieval, while retaining amortized `O(1)` append;
  *    this comes at the cost of making all updates `O(n)`.
  *  - the 'Nil' type ([[net.noresttherein.slang.tuples.Tuple.<>! <>]]) can be omitted from tuples of length &gt;=2 by
  *    using an alias: `Int&gt;:&lt;Int`;
  *  - they do not natively offer generic (polymorphic) mapping over all elements of a tuple;   
  *
  * @see [[net.noresttherein.slang.tuples.Tuple.><]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait Tuple extends Product {

	/** Number of elements in this tuple. Same as [[net.noresttherein.slang.tuples.Tuple.productArity productArity]]. */
	def length :Int

	/** True if this tuple has at least one element. */
	def isEmpty :Boolean = length == 0

	def productArity :Int = length

	def canEqual(that :Any) :Boolean = that.isInstanceOf[Tuple]
}






/** Factory object for variable-length tuples. */
object Tuple {

	implicit class TupleExtension[T <: Tuple](private val self :T) extends AnyVal {
		def toSeq[U](implicit lub :TupleLUB[T, U]) :Seq[U] = self match {
			case <> => Nil
			case t : ><[p, l] => t.toList[U](lub.asInstanceOf[TupleLUB[p >< l, U]])
		}

		def :+[X](x :X) :T >< X =
			(self match {
				case <> => <> >< x
				case t: ><[p, l] => t >< x
			}).asInstanceOf[T >< X]
	}

	/** Retrieves all elements from this tuple as a list with the type of their common upper bound. To match
	  * individual elements similarly to `List`s `::`, use [[net.noresttherein.slang.tuples.Tuple.><$ ><]]
	  * (infix notation possible).
	  * @param tuple the tuple to explode
	  * @tparam T concrete product type of this tuple containing information about all elements.
	  * @tparam U calculated least upper bound type for elements of this tuple.
	  * @return an list with all elements of this tuple, with the leftmost (index 0) being its head.
	  */
	def unapplySeq[T <: Tuple, U](tuple :T)(implicit lub :TupleLUB[T, U]) :Seq[U] = tuple match {
		case _ : <> => Nil
		case xs : ><[p, l] => xs.toList[U](lub.asInstanceOf[TupleLUB[p >< l, U]])
	}

	/** Create an empty tuple. */
	def apply(): <> = <>

	/** Create a single element tuple. */
	def apply[A](a :A) : <*>[A] = {
		val elems = new Array[Any](1)
		elems(0) = a
		new ><(elems, 1)
	}

	/** Create a two-element tuple. */
	def apply[A, B](a :A, b :B) :A >:< B = {
		val elems = new Array[Any](2)
		elems(0) = a; elems(1) = b
		new ><(elems, 2)
	}

	/** Create a three-element tuple. */
	def apply[A, B, C](a :A, b :B, c :C) :A >:< B >< C = {
		val elems = new Array[Any](3)
		elems(0) = a; elems(1) = b; elems(2) = c
		new ><(elems, 3)
	}

	/** Create a four-element tuple. */
	def apply[A, B, C, D](a :A, b :B, c :C, d :D) :A >:< B >< C >< D = {
		val elems = new Array[Any](4)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d
		new ><(elems, 4)
	}

	/** Create a five-element tuple. */
	def apply[A, B, C, D, E](a :A, b :B, c :C, d :D, e :E) :A >:< B >< C >< D >< E = {
		val elems = new Array[Any](5)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e
		new ><(elems, 5)
	}

	/** Create a six-element tuple. */
	def apply[A, B, C, D, E, F](a :A, b :B, c :C, d :D, e :E, f :F) :A >:< B >< C >< D >< E >< F = {
		val elems = new Array[Any](6)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		new ><(elems)
	}

	/** Create a seven-element tuple. */
	def apply[A, B, C, D, E, F, G](a :A, b :B, c :C, d :D, e :E, f :F, g :G) :A >:< B >< C >< D >< E >< F >< G = {
		val elems = new Array[Any](7)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g
		new ><(elems)
	}

	/** Create an eight-element tuple. */
	def apply[A, B, C, D, E, F, G, H](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H) :A >:< B >< C >< D >< E >< F >< G >< H = {
		val elems = new Array[Any](8)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h
		new ><(elems)
	}

	/** Create a nine-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I =
	{
		val elems = new Array[Any](9)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i
		new ><(elems)
	}

	/** Create a ten-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J =
	{
		val elems = new Array[Any](10)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j
		new ><(elems)
	}

	/** Create an eleven-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K =
	{
		val elems = new Array[Any](11)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k
		new ><(elems)
	}

	/** Create a twelve-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L =
	{
		val elems = new Array[Any](12)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		new ><(elems)
	}

	/** Create a thirteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M =
	{
		val elems = new Array[Any](13)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m
		new ><(elems)
	}

	/** Create a fourteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N =
	{
		val elems = new Array[Any](14)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n
		new ><(elems)
	}

	/** Create a fifteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O =
	{
		val elems = new Array[Any](15)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o
		new ><(elems)
	}

	/** Create a sixteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P =
	{
		val elems = new Array[Any](16)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p
		new ><(elems)
	}

	/** Create a seventeen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q =
	{
		val elems = new Array[Any](17)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p; elems(16) = q
		new ><(elems)
	}

	/** Create an eighteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R =
	{
		val elems = new Array[Any](18)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p; elems(16) = q; elems(17) = r
		new ><(elems)
	}

	/** Create a nineteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S =
	{
		val elems = new Array[Any](19)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p; elems(16) = q; elems(17) = r
		elems(18) = s
		new ><(elems)
	}

	/** Create a twenty-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S, t :T)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S >< T =
	{
		val elems = new Array[Any](20)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p; elems(16) = q; elems(17) = r
		elems(18) = s; elems(19) = t
		new ><(elems)
	}

	/** Create a twenty one-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S, t :T, u :U)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S >< T >< U =
	{
		val elems = new Array[Any](21)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p; elems(16) = q; elems(17) = r
		elems(18) = s; elems(19) = t; elems(20) = u
		new ><(elems)
	}

	/** Create a twenty two-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S, t :T, u :U, v :V)
	         :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S >< T >< U >< V =
	{
		val elems = new Array[Any](22)
		elems(0) = a; elems(1) = b; elems(2) = c; elems(3) = d; elems(4) = e; elems(5) = f
		elems(6) = g; elems(7) = h; elems(8) = i; elems(9) = j; elems(10) = k; elems(11) = l
		elems(12) = m; elems(13) = n; elems(14) = o; elems(15) = p; elems(16) = q; elems(17) = r
		elems(18) = s; elems(19) = t; elems(20) = u; elems(21) = v
		new ><(elems)
	}






	@inline
	implicit def fromTuple[A](t :Tuple1[A]) : <*>[A] = apply(t._1)

	@inline
	implicit def fromTuple[A, B](t :(A, B)) :A >:< B = apply(t._1, t._2)

	@inline
	implicit def fromTuple[A, B, C](t :(A, B, C)) :A >:< B >< C = apply(t._1, t._2, t._3)

	@inline
	implicit def fromTuple[A, B, C, D](t :(A, B, C, D)) :A >:< B >< C >< D = apply(t._1, t._2, t._3, t._4)

	@inline
	implicit def fromTuple[A, B, C, D, E](t :(A, B, C, D, E)) :A >:< B >< C >< D >< E =
		apply(t._1, t._2, t._3, t._4, t._5)

	@inline
	implicit def fromTuple[A, B, C, D, E, F](t :(A, B, C, D, E, F)) :A >:< B >< C >< D >< E >< F =
		apply(t._1, t._2, t._3, t._4, t._5, t._6)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G](t :(A, B, C, D, E, F, G)) :A >:< B >< C >< D >< E >< F >< G =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H](t :(A, B, C, D, E, F, G, H)) :A >:< B >< C >< D >< E >< F >< G >< H =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I](t :(A, B, C, D, E, F, G, H, I))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J](t :(A, B, C, D, E, F, G, H, I, J))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K](t :(A, B, C, D, E, F, G, H, I, J, K))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L](t :(A, B, C, D, E, F, G, H, I, J, K, L))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18
		)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19
		)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S >< T =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19, t._20
		)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S >< T >< U =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21
		)

	@inline
	implicit def fromTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	                      (t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))
	                      :A >:< B >< C >< D >< E >< F >< G >< H >< I >< J >< K >< L >< M >< N >< O >< P >< Q >< R >< S >< T >< U >< V =
		apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
		      t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22
		)






	@inline
	implicit def toTuple[A](t: <*>[A]) :Tuple1[A] =  Tuple1(t._1)

	@inline
	implicit def toTuple[A, B](t: A>:<B) :(A, B) =  (t._1, t._2)

	@inline
	implicit def toTuple[A, B, C](t: A>:<B><C) :(A, B, C) =  (t._1, t._2, t._3)

	@inline
	implicit def toTuple[A, B, C, D](t: A>:<B><C><D) :(A, B, C, D) =  (t._1, t._2, t._3, t._4)

	@inline
	implicit def toTuple[A, B, C, D, E](t: A>:<B><C><D><E) :(A, B, C, D, E) =  (t._1, t._2, t._3, t._4, t._5)

	@inline
	implicit def toTuple[A, B, C, D, E, F](t: A>:<B><C><D><E><F) :(A, B, C, D, E, F) =  (t._1, t._2, t._3, t._4, t._5, t._6)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G](t: A>:<B><C><D><E><F><G) :(A, B, C, D, E, F, G) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H](t: A>:<B><C><D><E><F><G><H) :(A, B, C, D, E, F, G, H) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I](t: A>:<B><C><D><E><F><G><H><I) :(A, B, C, D, E, F, G, H, I) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J](t: A>:<B><C><D><E><F><G><H><I><J) :(A, B, C, D, E, F, G, H, I, J) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K](t: A>:<B><C><D><E><F><G><H><I><J><K)
	                    :(A, B, C, D, E, F, G, H, I, J, K) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L](t: A>:<B><C><D><E><F><G><H><I><J><K><L)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M](t: A>:<B><C><D><E><F><G><H><I><J><K><L><M)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N](t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P><Q)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P><Q><R)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P><Q><R><S)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P><Q><R><S><T)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19, t._20)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P><Q><R><S><T><U)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)

	@inline
	implicit def toTuple[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	                    (t: A>:<B><C><D><E><F><G><H><I><J><K><L><M><N><O><P><Q><R><S><T><U><V)
	                    :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
		(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13,
			t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)






	/** An empty tuple (a product of zero arity). Useful particularly as the terminator type for variable tuples
	  * in a manner similar to list's `Nil` (and `HList`'s `HNil`).
	  */
	@SerialVersionUID(1L)
	final class <> private[Tuple]() extends Tuple {

		override def length :Int = 0

		override def isEmpty :Boolean = true

		override def productElement(n :Int) :Any = throw new IndexOutOfBoundsException(n.toString)

		/** Create a single element tuple consisting of element `head`. */
		def ><[X](head :X): <*>[X] = {
			val elems = new Array[Any](4)
			elems(0) = head
			new ><(elems, 1)
		}

		def toList[U](implicit tpe :TupleLUB[<>, U]) :List[U] = Nil

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[<>]

		override def toString = "<>"
	}

	/** An empty tuple (a product of zero arity). Prepending elements with its
	  * [[net.noresttherein.slang.tuples.Tuple.<>.>< ><]] operator results in creation of new tuples.
	  */
	final val <> : <> = new <>
	
	/** An alias for the empty tuple `&lt;&gt;`. No special characters which allows omitting spaces in composite types: `T0&gt;&lt;X&gt;&lt;Y`. */
	type T0 = <>
	/** An empty tuple, same as `&lt;&gt;`, but without special characters which allows omitting spaces on append: `T0&gt;&lt;"first"`. */
	final val T0 = <>
	

	
	/** An alias for a single element tuple `&lt;&gt; &gt;&lt; X`. New instances can be created either by the 
	  * companion object to this type, by the single-argument [[net.noresttherein.slang.tuples.Tuple.apply[A](a:A)* Tuple(x)]],
	  * or by a factory method added implicitly after importing this type: `x.&lt;&gt;`.
	  */
	type <*>[+X] = <> >< X
	
	/** Companion object for singleton tuples `&lt;*&gt;` offering a factory method and matching pattern. */
	object <*> {
		/** A single element tuple. */
		@inline final def apply[T](singleton :T): <*>[T] = Tuple(singleton)
		
		def unapply[T](t: <*>[T]) :Some[T] = Some(t.array(0).asInstanceOf[T])
	}

	/** Wraps the given value in a value class providing a zero-argument `&lt;&gt;` method for converting it into
	  * a single-element tuple.
	  */
	@inline implicit def <*>[X](value :X) :Tuple1Constructor[X] = new Tuple1Constructor(value)
	
	/** Value class providing an extension method converting the wrapped value into a single-element tuple. */
	class Tuple1Constructor[X](private val value :X) extends AnyVal {
		/** Converts this value into a single element tuple. */
		@inline def <> : <*>[X] = Tuple(value)
	}


	
	/** An alias for a two element tuple `<> >< X >< Y` omitting the terminator type `<>` from the type signature,
	  * shortening the notation to `X >:< Y`. New instances can be created by the companion object to this type
	  * [[net.noresttherein.slang.tuples.Tuple.>:<$ >:<]]: `>:<(x, y)`, `Tuple(x, y)`, or an implicitly added extension
	  * method `>:<`: `x >:< y` (made available after importing this type).
	  */
	type >:<[+X, +Y] = <> >< X >< Y
	
	/** Constructor and extractor for two-element tuples `H&gt;:&lt;T` (equivalent to `&lt;&gt; &gt; H &gt;&lt; T`) 
	  * for the use in pattern matching. 
	  */
	object >:< {
		/** Create a two-element tuple. */
		def apply[A, B](first :A, second :B) :A >:< B = {
			val elems = new Array[Any](4)
			elems(0) = second; elems(1) = first
			new ><(elems, 2)
		}

		def unapply[A, B](tuple :A >:< B) :Option[(A, B)] = Some((tuple.array(0), tuple.array(1)).asInstanceOf[(A, B)])
	}
	
	/** Implicit conversion from values of any type `X` adding a [[net.noresttherein.slang.tuples.Tuple.Tuple2Constructor.>:< >:<]]
	  * factory method for creating pairs (two-argument tuples).
	  * @param first the first element of the created tuple.
	  * @tparam X the type of the first element in the created tuple.
	  * @return a light wrapper over the `first` object, adding a pair factory method `:*`.
	  */
	@inline implicit def >:<[X](first :X): Tuple2Constructor[X] = new Tuple2Constructor(first)

	/** Patches any object implicitly adding the [[net.noresttherein.slang.tuples.Tuple.Tuple2Constructor.\:\*]] method
	  * for creating pair objects. */
	class Tuple2Constructor[X](private val first :X) extends AnyVal {
		/** Creates a two-argument tuple `X &gt;:&lt; Y` with this (left) value as the first element and the argument as the second. */
		@inline def >:<[Y](second :Y): X >:< Y = apply(first, second)

		/** Creates a two-argument tuple `X &gt;:&lt; Y` with this (left) value as the first element and the argument as the second.
		  * This works exactly like `>:<` but retains the uniformity of notation between all tuple elements.
		  */
		@inline def ><[Y](second :Y): X >:< Y = apply(first, second)
	}




	/** Cartesian product of types in the tuple `P` and type `L`, being a type constructor for tuples with variable lengths.
	  * New instances can be created either by the [[net.noresttherein.slang.tuples.Tuple$ Tuple]]'s object overloaded
//	  * `apply` methods for fixed arities, or by recursively appending elements with [[net.noresttherein.slang.tuples.Tuple!.>< &gt;&lt;]]
	  * to existing tuples. A natural start would be either the empty product [[net.noresttherein.slang.tuples.Tuple.<>$ &lt;&gt;]] 
	  * or a pair: [[net.noresttherein.slang.tuples.Tuple.>:< >:<]].
	  * Please note that, in order to retain consistency with arbitrary scala tuples, elements are indexed starting 
	  * with `1`, unlike collections (including `HList`s).
	  * @tparam P the product of the types of first `n-1` elements in this tuple (where `n` is the length of this tuple).
	  * @tparam L the type of the last element in this tuple.
	  * @see [[net.noresttherein.slang.tuples.Tuple.><$ companion object]]
	  */
	@SerialVersionUID(1L)
	final class ><[+P <: Tuple, +L] private[Tuple](elements :Array[Any], size :Int, owner :Boolean = true) extends Tuple {

		private[Tuple] def this(elements :Array[Any]) = this(elements, elements.length)

		@inline private[Tuple] def array :Array[Any] = elements

		@volatile private[this] var canAppend = owner

		/** The number of elements in this tuple. */
		def length :Int = size

		override def isEmpty :Boolean = false


		/** The first element of this tuple. */
		def _1[E](implicit tpe :TupleAt[P >< L, _1, E]) :E = elements(0).asInstanceOf[E]

		/** The second element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _2[E](implicit tpe :TupleAt[P >< L, _2, E]) :E = elements(1).asInstanceOf[E]

		/** The third element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _3[E](implicit tpe :TupleAt[P >< L, _3, E]) :E = elements(2).asInstanceOf[E]

		/** The fourth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _4[E](implicit tpe :TupleAt[P >< L, _4, E]) :E = elements(3).asInstanceOf[E]

		/** The fifth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _5[E](implicit tpe :TupleAt[P >< L, _5, E]) :E = elements(4).asInstanceOf[E]

		/** The sixth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _6[E](implicit tpe :TupleAt[P >< L, _6, E]) :E = elements(5).asInstanceOf[E]

		/** The seventh element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _7[E](implicit tpe :TupleAt[P >< L, _7, E]) :E = elements(6).asInstanceOf[E]

		/** The eighth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _8[E](implicit tpe :TupleAt[P >< L, _8, E]) :E = elements(7).asInstanceOf[E]

		/** The ninth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _9[E](implicit tpe :TupleAt[P >< L, _9, E]) :E = elements(8).asInstanceOf[E]

		/** The tenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _10[E](implicit tpe :TupleAt[P >< L, _10, E]) :E = elements(9).asInstanceOf[E]

		/** The eleventh element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _11[E](implicit tpe :TupleAt[P >< L, _11, E]) :E = elements(10).asInstanceOf[E]

		/** The twelfth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _12[E](implicit tpe :TupleAt[P >< L, _12, E]) :E = elements(11).asInstanceOf[E]

		/** The thirteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _13[E](implicit tpe :TupleAt[P >< L, _13, E]) :E = elements(12).asInstanceOf[E]

		/** The fourteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _14[E](implicit tpe :TupleAt[P >< L, _14, E]) :E = elements(13).asInstanceOf[E]

		/** The fifteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _15[E](implicit tpe :TupleAt[P >< L, _15, E]) :E = elements(14).asInstanceOf[E]

		/** The sixteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _16[E](implicit tpe :TupleAt[P >< L, _16, E]) :E = elements(15).asInstanceOf[E]

		/** The seventeenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _17[E](implicit tpe :TupleAt[P >< L, _17, E]) :E = elements(16).asInstanceOf[E]

		/** The eighteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _18[E](implicit tpe :TupleAt[P >< L, _18, E]) :E = elements(17).asInstanceOf[E]

		/** The nineteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _19[E](implicit tpe :TupleAt[P >< L, _19, E]) :E = elements(18).asInstanceOf[E]

		/** The twentieth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _20[E](implicit tpe :TupleAt[P >< L, _20, E]) :E = elements(19).asInstanceOf[E]

		/** The twenty-first element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _21[E](implicit tpe :TupleAt[P >< L, _21, E]) :E = elements(20).asInstanceOf[E]

		/** The twenty-second element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _22[E](implicit tpe :TupleAt[P >< L, _22, E]) :E = elements(21).asInstanceOf[E]



		/** Retrieves the `n`-th element of this tuple as a value of `LUB` type of all member types (that is the most
		  * specific type `U` such that all elements of this tuple conform to `U`. This is the only way to access the
		  * elements based on dynamic index values. If the index is statically known, consider instead
		  * [[net.noresttherein.slang.tuples.Tuple.><.apply[N](n:N)(implicit\ tpe:TupleAt[P><L,N,X]* Tuple.><(n)]]
		  * @param n the index of the element to retrieve (starting with `1` as the first element).
		  * @param lub an implicit witness providing the least upper bound for member types of this tuple.
		  * @tparam U the least upper bound for types of all elements in this tuple.
		  */
		def apply[U](n :Int)(implicit lub :TupleLUB[P >< L, U]) :U = {
			if (n <= 0 || n > size)
				throw new IndexOutOfBoundsException(s"$this($n)")
			elements(n - 1).asInstanceOf[U]
		}

		/** Retrieves the `n`-th element of this tuple. The value of `n` must be statically known and encoded as a natural
		  * number type `Nat`. See [[net.noresttherein.slang.tuples.Nat$ Nat]] for constants such as
		  * [[net.noresttherein.slang.tuples.Nat$._1 _1]], [[net.noresttherein.tuples.Nat$._2]], ''etc.''.
		  * @param n the index of the element to retrieve (starting with `_1`(1) for the first element),
		  *          statically encoded as a recursively nested type.
		  * @param tpe an implicit witness providing the type of the `n`-th element in this tuple.
		  * @tparam N a recursively constructed type encoding the index of the retrieved element.
		  * @tparam X the exact type of the retrieved element provided by the implicit parameter.
		  */
		def apply[N <: Nat, X](n :N)(implicit tpe :TupleAt[P >< L, N, X]) :X =
			elements(n.number - 1).asInstanceOf[X]

		/** Create a new tuple of the same arity resulting from replacing the `N`-th element of this tuple with
		  * the given value.
 		  * @param n a type-encoded natural number indicating the index of the replaced element.
		  * @param value a new value for the `n`-th position.
		  * @param tpe implicit witness defining the result type after replacement.
		  * @tparam N Recursively constructed type encoding the index of the retrieved element.
		  * @tparam X the type of the new element.
		  * @tparam R result type provided by an implicit parameter.
		  */
		def set[N <: Nat, X, R <: Tuple](n :N, value :X)(implicit tpe :UpdatedTuple[P >< L, N, X, R]) :R = {
			var count = size
			val copy = new Array[Any](count)
			do {
				count -= 1; copy(count) = elements(count)
			} while (count > 0)
			copy(n.number - 1) = value
			new ><[Tuple, Any](copy).asInstanceOf[R]
		}

		/** This tuple with the last element replaced with `x`. */
		def setLast[X](x :X) :P >< X = {
			var count = size - 1
			val copy = new Array[Any](size)
			copy(count) = x
			while(count > 0) {
				count -= 1
				copy(count) = elements(count)
			}
			new ><[P, X](copy)
		}

		/** Maps the `n`-th element of this tuple.
		  * @param n the index of the element to update, starting with `1` and encoded on type level.
		  * @param f the mapping function to apply to the `n`-th element.
		  * @tparam N the index to map encoded on type level.
		  * @tparam X the type of `n`-th element in this tuple, provided by implicit parameter `at`.
		  * @tparam Y the mapped type for the `n`-th element.
		  * @tparam R the tuple type resulting from replacing `X` (at n-th position) with `Y`,
		  *           as provided by the implicit parameter `res`.
		  * @return a tuple with the mapped element at `n`-th position and equal to this one on all other indices.
		  */
		def mapAt[N <: Nat, X, Y, R <: Tuple](n :N)(f :X => Y)
                 (implicit at :TupleAt[P >< L, N, X], res :UpdatedTuple[P >< L, N, Y, R]) :R = {
			val x = elements(n.number).asInstanceOf[X]
			val y = f(x)
			set(n, y)
		}

		/** Maps the last element of this tuple. */
		def mapLast[Y](f :L => Y) :P >< Y = setLast(f(elements(length).asInstanceOf[L]))


		/** First (leftmost) element of this tuple - same as `this._1`. */
		def head[E](implicit tpe :TupleAt[P >< L, _1, E]) :E = elements(0).asInstanceOf[E]

		/** A tuple consisting of all elements from this tuple except the first. */
		def tail[T <: Tuple](implicit tpe :TupleDrop[P >< L, _1, T]) :T = {
			val count = size - 1
			if (count == 0) <>
			else {
				val copy = new Array[Any](count)
				System.arraycopy(elements, 1, copy, 0, count)
				new ><(copy, count)
			}
		}.asInstanceOf[T]

		/** A tuple with the first `length-1` elements of this tuple. */
		def init: P = {
			if (size == 1) <>
			else new ><(elements, size - 1, false)
		}.asInstanceOf[P]

		/** The last (rightmost) element of this tuple. */
		def last: L = elements(size - 1).asInstanceOf[L]


		/** A copy of this tuple without the first `N` elements, where `N` is a type-level encoding of a natural number. */
		def drop[N <: Nat, R <: Tuple](n :N)(implicit restType :TupleDrop[P >< L, N, R]) :R = {
			if (n.number >= size) <>.asInstanceOf[R]
			else {
				val newsize = size - n.number
				val copy = new Array[Any](newsize)
				System.arraycopy(elements, n.number, copy, 0, newsize)
				new ><[Tuple, Any](copy, newsize).asInstanceOf[R]
			}
		}


		/** The first `N` elements of this tuple as a new tuple, where `N` is a type-level encoding of a natural number. */
		def take[N <: Nat, R <: Tuple](n :N)(implicit initType :TupleTake[P >< L, N, R]) :R =
			if (n.number <= 0) <>.asInstanceOf[R]
			else {
				if (n.number >= size) this.asInstanceOf[R]
				else new ><[Tuple, Any](elements, n.number, false).asInstanceOf[R]
			}


		/** A copy of this tuple without the last `N` elements, where `N` is a type-level encoding of a natural number. */
		def dropRight[N <: Nat, R <: Tuple](n :N)(implicit resType :TupleDropRight[P >< L, N, R]) :R = {
			val num = n.number
			if (num >= size) <>.asInstanceOf[R]
			else new ><[Tuple, Any](elements, size - n, false).asInstanceOf[R]
		}


		/** The last `N` elements of this tuple, where `N` is a type-level encoding of a natural number. */
		def takeRight[N <: Nat, R <: Tuple](n :N)(implicit resType :TupleTakeRight[P >< L, N, R]) :R =
			if (n.number <= 0) <>.asInstanceOf[R]
			else {
				val num = n.number
				if (num >= size) this.asInstanceOf[R]
				else {
					val copy = new Array[Any](num)
					System.arraycopy(elements, size - num, copy, 0, num)
					new ><[Tuple, Any](copy, num).asInstanceOf[R]
				}
			}


		/** A tuple containing elements from `F+1` to (including) `U`. `F` and `U` are type-level encodings of natural numbers.
		  * If `F >= U` or `F >= length`, an empty tuple is returned.
		  * @param from a value with encoded on the type level index of the first element of the new tuple (starting with 1).
		  * @param until a value with encoded on the type level index immediately after the last taken element (starting with 1).             
		  */
		def slice[F <: Nat, U <: Nat, R0 <: Tuple, R <:Tuple](from: ++[F], until: ++[U])
                 (implicit takeType :TupleTake[P >< L, U, R0], dropType :TupleDrop[R0, F, R]) :R = {
			val f = from.number - 1; val u = until.number - 1
			if (u <= 0 || f >= size || f >= u) <>.asInstanceOf[R]
			else if (f <= 0)
                if (u >= size) this.asInstanceOf[R]
                else new ><[Tuple, Any](elements, u, false).asInstanceOf[R]
			else new ><[Tuple, Any](elements.slice(f, u), u - f).asInstanceOf[R]
		}


		/** Splits this tuple into two tuples at the given index.
		  * @param n the first index of the second returned tuple, starting with 1 and with its value encoded on type level.
		  * @param left implicit provider of the type of the first returned tuple.
		  * @param right implicit provider of the type of the second returned tuple.
		  */
		def splitAt[N <: Nat, A <: Tuple, B <: Tuple](n :N)(implicit left :TupleTake[P >< L, N, A], right :TupleDrop[P >< L, N, B]) :(A, B) = {
			val i = n.number - 1
			if (i <= 0) (<>, this)
			else if (i >= size) (this, <>)
			else (new ><[Tuple, Any](elements, i, false), new ><[Tuple, Any](elements.slice(i, size), size - i))
		}.asInstanceOf[(A, B)]


		/** Makes a projection of this tuple, selecting the elements at the given indices.
		  * @param indices a tuple with [[net.noresttherein.slang.tuples.Nat Nat]] values,
		  *                where `n`-th element specifies the index in this tuple of the `n`-th element in the result.
		  *                An index can be present more than once on the list.
		  * @param proj implicit provider of the result type.
		  * @tparam I a tuple with the indices of selected elements; must be fully known.
		  * @tparam R the type resulting from projecting this tuple to the given indices.
		  */
		def select[I <: Tuple, R <: Tuple](indices :I)(implicit proj :TupleProjection[P >< L, I, R]) :R =
			if (indices.isEmpty)
				<>.asInstanceOf[R]
			else {
				val a = new Array[Any](indices.length)
				var i = 0
				indices.toSeq foreach { n => a(i) = n.asInstanceOf[Nat].toInt; i += 1 }
				new ><[Tuple, Any](a, i).asInstanceOf[R]
			}

		/** Returns a list containing all elements of this tuple from left to right. The element type is calculated
		  * as the least upper bound of all member types in this tuple.
		  * @param elemType implicit witness providing the most specific type to which all elements of this tuple conform.
		  * @tparam U a type to which all elements of this tuple conform.
		  */
		def toList[U](implicit elemType :TupleLUB[P >< L, U]) :List[U] = {
			var res :List[U] = Nil; var i = size
			while (i > 0) {
				i -= 1
				res = elements(i).asInstanceOf[U]::res
			}
			res
		}

		/** Returns a sequence containing all elements of this tuple from left to right. The element type is calculated
		  * as the least upper bound of all member types in this tuple.
		  * @param elemType implicit witness providing the most specific type to which all elements of this tuple conform.
		  * @tparam U a type to which all elements of this tuple conform.
		  */
		def toSeq[U](implicit elemType :TupleLUB[P >< L, U]) :Seq[U] =
			ArraySeq.unsafeWrapArray(elements).take(size).asInstanceOf[Seq[U]]



		/** A copy of this tuple with the `last` element appended (as the right-most element).
		  * The first call to this method on any tuple requires amortized constant time. More formally,
		  * recursive append operation (`x >:< y >< z >< ...)` happens in amortized `O(1)` as the elements are
		  * inserted to the shared array. Subsequent append operations on any instance always copy the whole contents.
		  */
		def ><[X](last :X): P >< L >< X = {
			if (elements.length < size && canAppend) synchronized {
				if (canAppend) {
					canAppend = false
					elements(size) = last
					return new ><(elements, size + 1)
				}
			}
			var newlen = elements.length * 2
			if (elements.length == size)
				newlen = size + 1
			val copy = new Array[Any](newlen)
			System.arraycopy(elements, 0, copy, 0, size)
			copy(size) = last
			new ><(copy, size + 1)
		}

		
		
		/** A copy of this tuple with the `first` element prepended as the first element.
		  * This method, unlike append `><`, will always result in copying the whole contents, requiring `O(n)` time.
		  * @see [[net.noresttherein.slang.tuples.Tuple.><.>< ><]]
		  */
		def ><:[X, U >: P >< L <: Tuple, R <: Tuple](first :X)(implicit tpe :TupleTakeRight[R, _1, U]) :R = {
			val copy = new Array[Any](size + 1)
			System.arraycopy(elements, 0, copy, 1, size)
			copy(0) = first
			new ><[Tuple, Any](copy, size + 1).asInstanceOf[R]
		}



		override def productElement(n :Int) :Any = elements(length - n)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[><[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case t : ><[_, _] =>
				(t eq this) || (t canEqual this) && t.length == size && {
					var i = size; val others = t.array
					do { i -= 1 } while (i >= 0 && elements(i) == others(i))
					i < 0
				}
			case _ => false
		}

		override def toString :String = {
			val res = new StringBuilder
			res append '(' append elements(0)
			var i = 1
			while (i < size) {
				res append " >< "
				res append elements(i)
				i += 1
			}
			res append ')'
			res.toString
		}

	}



	/** Extractor separating the first element of the tuple from the rest in pattern matching. Can be used in the recursive
	  * infix notation:
	  * {{{
	  * tuple match {
	  *     case 1 >:< 2 >< 3 >< 4 => ...
	  * }
	  * }}}.
	  * @see [[net.noresttherein.slang.tuples.Tuple.>:<$ >:<]]
	  */
	object >< {
		def unapply[P <: Tuple, L](tuple :P >< L) :Option[(P, L)] = Some(tuple.init, tuple.last)
	}






	/** Types and values for implicit parameters providing necessary types for the tuple methods. */
	object evidence {

		/** Implicit witness providing the upper type bound `U` (if unrestricted, the ''least upper bound'' for all elements
		  * of tuple `T`).
		  * @tparam T a tuple type
		  * @tparam U a type such that tuple(''_i'') :U for all `0 &lt;= i &lt; tuple.length`.
		  */
		@implicitNotFound("Cannot determine the upper bound type for elements of tuple ${T} (or it is not a subtype of ${U}).")
		final class TupleLUB[-T <: Tuple, +U] private()

		sealed abstract class AbsoluteUpperBound {
			implicit def any[T <: Tuple] :TupleLUB[T, Any] = TupleLUB.nothing.asInstanceOf[TupleLUB[T, Any]]
		}

		object TupleLUB extends AbsoluteUpperBound {
			implicit val nothing :TupleLUB[<>, Nothing] = new TupleLUB[<>, Nothing]

//			implicit def lub[T <: Tuple, L <: U, U](implicit init :TupleLUB[T, U]) :TupleLUB[T >< L, U] =
//				nothing.asInstanceOf[TupleLUB[T >< L, U]]
			implicit def lub[T <: Tuple, L, X, U](implicit ev :TupleLUB[T, X], lub :UpperBound[L, X, U]) :TupleLUB[T >< L, U] =
				nothing.asInstanceOf[TupleLUB[T >< L, U]]
		}



		/** Implicit evidence attesting that `N` is the type-encoded length of tuple type `T`. */
		@implicitNotFound("Cannot determine the length of tuple ${T} or it is not ${N}. Both types need to be fully instantiated for this to work.")
		final class TupleLength[-T <: Tuple, N <: Nat] private()

		object TupleLength {
			implicit val zeroLength :TupleLength[<>, _0] = new TupleLength[<>, _0]

			@inline implicit def longer[N <: Nat, T <: Tuple](implicit tailLength :TupleLength[T, N])
			                           :TupleLength[T >< Any, ++[N]] =
				zeroLength.asInstanceOf[TupleLength[T >< Any, ++[N]]]
		}



		/** An implicit witness providing the type `X` of `N`-th element in the tuple `T`.
		  * @tparam T a tuple type.
		  * @tparam N a zero-based index of an element in the tuple encoded as a type.
		  * @tparam X type of `n`-th element in `T` starting from the left.
		  * @see [[net.noresttherein.slang.tuples.Nat]]
		  */
		@implicitNotFound("The ${N]-th element of tuple ${T} does not conform to ${X} or the tuple does not have that many elements. Indexing requires both the tuple and index types to be fully instantiated.")
		final class TupleAt[-T, N <: Nat, +X] private[Tuple]()

		object TupleAt {
			private[this] val instance = new TupleAt[Tuple, Nat, Any]

			implicit def last_type[T <: Tuple, N <: Nat, X](implicit length :TupleLength[T, N]) :TupleAt[T >< X, ++[N], X] =
				instance.asInstanceOf[TupleAt[T >< X, ++[N], X]]

			implicit def type_n[T <: Tuple, N <: Nat, X](implicit tpe :TupleAt[T, N, X]) :TupleAt[T >< Any, N, X] =
				instance.asInstanceOf[TupleAt[T >< Any, N, X]]
		}



		/** An implicit witness providing the type `R` of tuple `T` after its `N`-th element has been set to `X`. */
		@implicitNotFound("Can't replace the ${N}-th element of tuple ${T} with ${X}: the tuple is too short, or the result does not conform to ${R}. Indexing requires both the tuple and index types to be fully instantiated.")
		final class UpdatedTuple[-T <: Tuple, N <: Nat, X, R <: Tuple] private()

		object UpdatedTuple {
			private[this] val instance = new UpdatedTuple[Tuple, Nat, Any, Tuple]

			implicit def updateLast[T <: Tuple, N <: Nat, X](implicit length :TupleLength[T, N])
			                       :UpdatedTuple[T >< Any, ++[N], X, T >< X] =
				instance.asInstanceOf[UpdatedTuple[T >< Any, ++[N], X, T >< X]]

			implicit def update_n[T <: Tuple, L, N <: Nat, X, P <: Tuple](implicit init :UpdatedTuple[T, N, X, P])
			                     :UpdatedTuple[T >< L, N, X, P >< L] =
				instance.asInstanceOf[UpdatedTuple[T >< L, N, X, P >< L]]
		}



		/** Implicit evidence attesting that `R` is the type resulting from dropping `N` first elements from tuple type `T`. */
		@implicitNotFound("Can't drop ${N} first elements from tuple ${T}: the tuple is too short or the result does not conform to ${R}. In order to compute the result type both the input tuple and index types must be fully instantiated.")
		final class TupleDrop[-T <: Tuple, N <: Nat, R <: Tuple] private()

		object TupleDrop {
			private[this] val instance = new TupleDrop[Tuple, _0, Tuple]

			@inline implicit def dropAll[T <: Tuple, N <: Nat](implicit length :TupleLength[T, N]) :TupleDrop[T, N, <>] =
				instance.asInstanceOf[TupleDrop[T, N, <>]]

			@inline implicit def dropNoMore[T <: Tuple, L, N <: Nat, R <: Tuple](implicit init :TupleDrop[T, N, R])
			                               :TupleDrop[T >< L, N, R >< L] =
				instance.asInstanceOf[TupleDrop[T >< L, N, R >< L]]
		}



		/** Implicit witness attesting that `R` is the type resulting from taking `N` first elements from tuple type `T`. */
		@implicitNotFound("Can't take ${N} first elements of tuple ${T}: the tuple is too short or the result does not conform to ${R}. In order to take, both the input tuple and index types must be fully instantiated")
		final class TupleTake[-T <: Tuple, N <: Nat, R <: Tuple] private()

		object TupleTake {
			private[this] val instance = new TupleTake[<>, _0, <>]

			@inline implicit def takeAll[T <: Tuple, N <: Nat](implicit length :TupleLength[T, N]) :TupleTake[T, N, T] =
				instance.asInstanceOf[TupleTake[T, N, T]]

			@inline implicit def takeNoMore[T <: Tuple, N <: Nat, R <: Tuple](implicit init :TupleTake[T, N, R])
			                               :TupleTake[T >< Any, N, R] =
				instance.asInstanceOf[TupleTake[T >< Any, N, R]]
		}



		/** Implicit evidence attesting that `R` is the type resulting from dropping `N` last (right) elements from tuple type `T`. */
		@implicitNotFound("Can't drop ${N} last elements from tuple ${T}: the tuple is too short, too few element types are known, index type is not fully instantiated or the result does not conform to ${R}.")
		final class TupleDropRight[-T <: Tuple, N <: Nat, R <: Tuple] private()

		object TupleDropRight {
			private[this] val instance = new TupleDropRight[Tuple, _0, Tuple]

			@inline implicit def drop0[T <: Tuple] :TupleDropRight[T, _0, T] =
				instance.asInstanceOf[TupleDropRight[T, _0, T]]

			@inline implicit def dropMore[T <: Tuple, N <: Nat, R <: Tuple](implicit dropped :TupleDropRight[T, N, R])
			                             :TupleDropRight[T >< Any, ++[N], R] =
				instance.asInstanceOf[TupleDropRight[T >< Any, ++[N], R]]
		}



		/** Implicit witness attesting that `R` is the type resulting from taking `N` last elements from tuple type `T`. */
		@implicitNotFound("Can't take ${N} last elements of tuple ${T}: the tuple is too short, too few element types are known, index type is not fully instantiated or the result does not conform to ${R}")
		final class TupleTakeRight[-T <: Tuple, N <: Nat, R <: Tuple] private()

		object TupleTakeRight {
			private[this] val instance = new TupleTakeRight[<>, _0, <>]

			@inline implicit def take0[T <: Tuple] :TupleTakeRight[T, _0, <>] =
				instance.asInstanceOf[TupleTakeRight[T, _0, <>]]

			@inline implicit def takeMore[T <: Tuple, L, N <: Nat, R <: Tuple](implicit tail :TupleTakeRight[T, N, R])
			                             :TupleTakeRight[T >< L, ++[N], R >< L] =
				instance.asInstanceOf[TupleTakeRight[T >< L, ++[N], R >< L]]
		}


		/** Implicit evidence witnessing that `R` is the projection of tuple `T` on the indices of `I` (encoded as `Nat` types). */
		@implicitNotFound("Can't calculate the projection of ${T} to ${I} or it does not conform to ${R}. Both the input and indices tuple types must be fully instantiated for this to work.")
		final class TupleProjection[-T <: Tuple, I <: Tuple, R <: Tuple] private ()

		object TupleProjection {
			implicit val empty :TupleProjection[Tuple, <>, <>] = new TupleProjection[Tuple, <>, <>]

			implicit def projectNext[T <: Tuple, N <: Nat, X, I <: Tuple, R <:Tuple](implicit init :TupleProjection[T, I, R], nth :TupleAt[T, N, X])
			                        :TupleProjection[T, I >< N, R >< X] =
				empty.asInstanceOf[TupleProjection[T, I >< N, R >< X]]
		}
	}

}
