package net.noresttherein.slang.tuples

import net.noresttherein.slang.tuples.Nat.{++, _0, _1, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _2, _20, _21, _22, _3, _4, _5, _6, _7, _8, _9}
import net.noresttherein.slang.typist.UpperBound


//todo: projections and mapping, update


/** Generic base trait for tuples of varying number of elements. It is functionally very similar to ''shapeless' '' `HList`s.
  * The main difference is that they are backed by arrays instead of linked lists, offering constant time element retrieval,
  * assuming the compiler optimizer of HotSpot-like VM will do away with unused implicit arguments existing solely as
  * witnesses of type safety. The added expense is the need for copying the whole structure at modification (with exclusion of
  * first prepend operation).
  *
  * @see [[Tuple.*:]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait Tuple extends Product {
	/** Number of elements in this tuple. Same as [[Tuple#productArity]]. */
	def length :Int

	def productArity :Int = length

}






/** Factory object for variable-length tuples. */
object Tuple {

	/** Retrieves all elements from this tuple as a list with the type of their common upper bound. To match
	  * individual elements similarly to `List`s `::`, use [[net.noresttherein.slang.Tuple.*:]] (infix notation possible).
	  * @param tuple the tuple to explode
	  * @tparam T concrete product type of this tuple containing information about all elements.
	  * @tparam U calculated least upper bound type for elements of this tuple.
	  * @return an list with all elements of this tuple, with the leftmost (index 0) being its head.
	  */
	def unapplySeq[T <: Tuple, U](tuple :T)(implicit lub :TupleLUB[T, U]) :Seq[U] = tuple match {
		case _ : <*> => Nil
		case xs : *:[h, t] => xs.toSeq[U](lub.asInstanceOf[TupleLUB[h *: t, U]])
	}

	/** Create an empty tuple. */
	def apply(): <*> = <*>

	/** Create a single element tuple. */
	def apply[A](a :A) :A *: <*> = {
		val elems = new Array[Any](1)
		elems(0) = a
		new *:[A, <*>](elems, 1)
	}

	/** Create a two-element tuple. */
	def apply[A, B](a :A, b :B) :A **: B = {
		val elems = new Array[Any](2)
		elems(0) = b; elems(1) = a
		new *:(elems, 2)
	}

	/** Create a three-element tuple. */
	def apply[A, B, C](a :A, b :B, c :C) :A *: B **: C = {
		val elems = new Array[Any](3)
		elems(0) = c; elems(1) = b; elems(2) = a
		new *:(elems, 3)
	}

	/** Create a four-element tuple. */
	def apply[A, B, C, D](a :A, b :B, c :C, d :D) :A *: B *: C **: D = {
		val elems = new Array[Any](4)
		elems(0) = d; elems(1) = c; elems(2) = b; elems(3) = a
		new *:(elems, 4)
	}

	/** Create a five-element tuple. */
	def apply[A, B, C, D, E](a :A, b :B, c :C, d :D, e :E) :A *: B *: C *: D **: E = {
		val elems = new Array[Any](5)
		elems(0) = e; elems(1) = d; elems(2) = c; elems(3) = b; elems(4) = a
		new *:(elems, 5)
	}

	/** Create a six-element tuple. */
	def apply[A, B, C, D, E, F](a :A, b :B, c :C, d :D, e :E, f :F) :A *: B *: C *: D *: E **: F = {
		val elems = new Array[Any](6)
		elems(0) = f; elems(1) = e; elems(2) = d; elems(3) = c; elems(4) = b; elems(5) = a
		new *:(elems)
	}

	/** Create a seven-element tuple. */
	def apply[A, B, C, D, E, F, G](a :A, b :B, c :C, d :D, e :E, f :F, g :G) :A *: B *: C *: D *: E *: F **: G = {
		val elems = new Array[Any](7)
		elems(0) = g; elems(1) = f; elems(2) = e; elems(3) = d; elems(4) = c; elems(5) = b
		elems(6) = a
		new *:(elems)
	}

	/** Create an eight-element tuple. */
	def apply[A, B, C, D, E, F, G, H](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H) :A *: B *: C *: D *: E *: F *: G **: H = {
		val elems = new Array[Any](8)
		elems(0) = h; elems(1) = g; elems(2) = f; elems(3) = e; elems(4) = d; elems(5) = c
		elems(6) = b; elems(7) = a
		new *:(elems)
	}

	/** Create a nine-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I)
			:A *: B *: C *: D *: E *: F *: G *: H **: I =
	{
		val elems = new Array[Any](9)
		elems(0) = i; elems(1) = h; elems(2) = g; elems(3) = f; elems(4) = e; elems(5) = d
		elems(6) = c; elems(7) = b; elems(8) = a
		new *:(elems)
	}

	/** Create a ten-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J)
			:A *: B *: C *: D *: E *: F *: G *: H *: I **: J =
	{
		val elems = new Array[Any](10)
		elems(0) = j; elems(1) = i; elems(2) = h; elems(3) = g; elems(4) = f; elems(5) = e
		elems(6) = d; elems(7) = c; elems(8) = b; elems(9) = a
		new *:(elems)
	}

	/** Create an eleven-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J **: K =
	{
		val elems = new Array[Any](11)
		elems(0) = k; elems(1) = j; elems(2) = i; elems(3) = h; elems(4) = g; elems(5) = f
		elems(6) = e; elems(7) = d; elems(8) = c; elems(9) = b; elems(10) = a
		new *:(elems)
	}

	/** Create a twelve-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L](a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K **: L =
	{
		val elems = new Array[Any](12)
		elems(0) = l; elems(1) = k; elems(2) = j; elems(3) = i; elems(4) = h; elems(5) = g
		elems(6) = f; elems(7) = e; elems(8) = d; elems(9) = c; elems(10) = b; elems(11) = a
		new *:(elems)
	}

	/** Create a thirteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L **: M =
	{
		val elems = new Array[Any](13)
		elems(0) = m; elems(1) = l; elems(2) = k; elems(3) = j; elems(4) = i; elems(5) = h
		elems(6) = g; elems(7) = f; elems(8) = e; elems(9) = d; elems(10) = c; elems(11) = b
		elems(12) = a
		new *:(elems)
	}

	/** Create a fourteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M **: N =
	{
		val elems = new Array[Any](14)
		elems(0) = n; elems(1) = m; elems(2) = l; elems(3) = k; elems(4) = j; elems(5) = i
		elems(6) = h; elems(7) = g; elems(8) = f; elems(9) = e; elems(10) = d; elems(11) = c
		elems(12) = b; elems(13) = a
		new *:(elems)
	}

	/** Create a fifteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N **: O =
	{
		val elems = new Array[Any](15)
		elems(0) = o; elems(1) = n; elems(2) = m; elems(3) = l; elems(4) = k; elems(5) = j
		elems(6) = i; elems(7) = h; elems(8) = g; elems(9) = f; elems(10) = e; elems(11) = d
		elems(12) = c; elems(13) = b; elems(14) = a
		new *:(elems)
	}

	/** Create a sixteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O **: P =
	{
		val elems = new Array[Any](16)
		elems(0) = p; elems(1) = o; elems(2) = n; elems(3) = m; elems(4) = l; elems(5) = k
		elems(6) = j; elems(7) = i; elems(8) = h; elems(9) = g; elems(10) = f; elems(11) = e
		elems(12) = d; elems(13) = c; elems(14) = b; elems(15) = a
		new *:(elems)
	}

	/** Create a seventeen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O *: P **: Q =
	{
		val elems = new Array[Any](17)
		elems(0) = q; elems(1) = p; elems(2) = o; elems(3) = n; elems(4) = m; elems(5) = l
		elems(6) = k; elems(7) = j; elems(8) = i; elems(9) = h; elems(10) = g; elems(11) = f
		elems(12) = e; elems(13) = d; elems(14) = c; elems(15) = b; elems(16) = a
		new *:(elems)
	}

	/** Create an eighteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O *: P *: Q **: R =
	{
		val elems = new Array[Any](18)
		elems(0) = r; elems(1) = q; elems(2) = p; elems(3) = o; elems(4) = n; elems(5) = m
		elems(6) = l; elems(7) = k; elems(8) = j; elems(9) = i; elems(10) = h; elems(11) = g
		elems(12) = f; elems(13) = e; elems(14) = d; elems(15) = c; elems(16) = b; elems(17) = a
		new *:(elems)
	}

	/** Create a nineteen-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O *: P *: Q *: R **: S =
	{
		val elems = new Array[Any](19)
		elems(0) = s; elems(1) = r; elems(2) = q; elems(3) = p; elems(4) = o; elems(5) = n
		elems(6) = m; elems(7) = l; elems(8) = k; elems(9) = j; elems(10) = i; elems(11) = h
		elems(12) = g; elems(13) = f; elems(14) = e; elems(15) = d; elems(16) = c; elems(17) = b
		elems(18) = a
		new *:(elems)
	}

	/** Create a twenty-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S, t :T)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O *: P *: Q *: R *: S **: T =
	{
		val elems = new Array[Any](20)
		elems(0) = t; elems(1) = s; elems(2) = r; elems(3) = q; elems(4) = p; elems(5) = o
		elems(6) = n; elems(7) = m; elems(8) = l; elems(9) = k; elems(10) = j; elems(11) = i
		elems(12) = h; elems(13) = g; elems(14) = f; elems(15) = e; elems(16) = d; elems(17) = c
		elems(18) = b; elems(19) = a
		new *:(elems)
	}

	/** Create a twenty one-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S, t :T, u :U)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O *: P *: Q *: R *: S *: T **: U =
	{
		val elems = new Array[Any](21)
		elems(0) = u; elems(1) = t; elems(2) = s; elems(3) = r; elems(4) = q; elems(5) = p
		elems(6) = o; elems(7) = n; elems(8) = m; elems(9) = l; elems(10) = k; elems(11) = j
		elems(12) = i; elems(13) = h; elems(14) = g; elems(15) = f; elems(16) = e; elems(17) = d
		elems(18) = c; elems(19) = b; elems(20) = a
		new *:(elems)
	}

	/** Create a twenty two-element tuple. */
	def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	         (a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J, k :K, l :L, m :M, n :N, o :O, p :P, q :Q, r :R, s :S, t :T, u :U, v :V)
			:A *: B *: C *: D *: E *: F *: G *: H *: I *: J *: K *: L *: M *: N *: O *: P *: Q *: R *: S *: T *: U **: V =
	{
		val elems = new Array[Any](22)
		elems(0) = v; elems(1) = u; elems(2) = t; elems(3) = s; elems(4) = r; elems(5) = q
		elems(6) = p; elems(7) = o; elems(8) = n; elems(9) = m; elems(10) = l; elems(11) = k
		elems(12) = j; elems(13) = i; elems(14) = h; elems(15) = g; elems(16) = f; elems(17) = e
		elems(18) = d; elems(19) = c; elems(20) = b; elems(21) = a
		new *:(elems)
	}










	/** An empty tuple (a product of zero arity). Useful particularly as terminator type for variable tuples
	  * in a manner similar to list's `Nil` (and `HList`'s `HNil`).
	  */
	final class <*> private[Tuple]() extends Tuple {
		override def length :Int = 0

		override def productElement(n :Int) :Any = throw new IndexOutOfBoundsException(n.toString)

		/** Create a single element tuple consisting of element `head`. */
		def *:[X](head :X): X *: <*> = {
			val elems = new Array[Any](4)
			elems(0) = head
			new *:(elems, 1)
		}

		def toSeq :Seq[Nothing] = Nil

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[<*>]

		override def toString = "<*>"
	}

	/** An empty tuple (a product of zero arity). Prepending elements with its [[Tuple.<*>.*:]] operator results in creation of new tuples. */
	final val <*> : <*> = new <*>




	/** An alias for `H *: T *: <*>` omitting the terminator type `<*>` from the type signature, shortening the notation a bit.
	  * New instances can be created by the companion object to this type [[Tuple*::]]: `**:(x, y)`.
	  */
	type **:[+H, +T] = H *: T *: <*>

	/** Implicit conversion from values of any type `X` adding a right-associative [[PairConstructor.**: **:]] method
	  * for creating pairs (two-argument tuples).
	  * @param second the second element of the created tuple.
	  * @tparam Y the type of the second element in the created tuple.
	  * @return a light wrapper over the `second` object, adding a pair factory method `**:`.
	  */
	@inline implicit def **:[Y](second :Y): PairConstructor[Y] = new PairConstructor(second)


	/** Constructor and extractor for two-element tuples `H**:T` (equivalent to `H *: T *: <*>`) for use in pattern matching. */
	object **: {
		/** Create a two-element tuple. */
		def apply[A, B](first :A, second :B) :A **: B = {
			val elems = new Array[Any](2)
			elems(0) = second; elems(1) = first
			new **:(elems, 2)
		}

		def unapply[A, B](tuple :A **: B) :Option[(A, B)] = Some((tuple._0, tuple._1))

	}


	/** Patches any object implicitly adding the [[PairConstructor#**:]] method for creating pair objects. */
	class PairConstructor[Y](private val second :Y) extends AnyVal {
		@inline def **:[X](first :X): X **: Y = apply(first, second)
		@inline def *:[X](first :X) : X **: Y = apply(first, second)
	}




	/** Cartesian product of type `H` and types listed in `T`, being a type constructor for tuples with variable lengths.
	  * New instances can be created either by the [[Tuple]]'s object overloaded `apply` methods for fixed arities,
	  * or by recursively prepending elements with [[Tuple!.*:]] to existing tuples. A natural start would be
	  * either the empty product [[Tuple.<*>]] or a pair: [[Tuple.**:]].
	  * @tparam H type of the first element in the tuple
	  * @tparam T a tuple type consisting of the 'tail'of this tuple, that is all element types after the first.
	  * @see [[**:]]
	  */
	final class *:[+H, +T <: Tuple] private[Tuple](elements :Array[Any], size :Int) extends Tuple {

		private[Tuple] def this(elements :Array[Any]) = this(elements, elements.length)

		@inline private[Tuple] def array :Array[Any] = elements

		/** Absolute value is equal to the arity of this tuple, i.e. the number of its elements. Signum is used to
		  * mark ownership of the `elements` array - if `len &gt; 0` then this instance is the only one which
		  * can write to the array. Negative value signifies that prepending elements to the tuple requires copying
		  * of the array. Singleton tuples always start with a positive value, while each time an element is prepended
		  * to a tuple, the value of field of the new tuple is copied from the original, while the latter's field
		  * is negated by setting it to `len = -math.abs(len)`. What follows is that the first prepend operation
		  * passes the ownership of the array to the newly created tuple without creating a new array. As the result,
		  * creating a tuple by prepending individual elements has `O(n)` complexity rather than `O(n*n)` in a naive
		  * implementation.
		  */
		@volatile private[this] var len = size

		def length :Int = { val count = len; if (count > 0) count else -count }



		/** The first element of this tuple. */
		def _0 :H = elements(length-1).asInstanceOf[H]

		/** The second element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _1[E](implicit tpe :TypeAt[H *: T, _1, E]) :E = elements(length - 2).asInstanceOf[E]

		/** The third element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _2[E](implicit tpe :TypeAt[H *: T, _2, E]) :E = elements(length-3).asInstanceOf[E]

		/** The fourth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _3[E](implicit tpe :TypeAt[H *: T, _3, E]) :E = elements(length-4).asInstanceOf[E]

		/** The fith element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _4[E](implicit tpe :TypeAt[H *: T, _4, E]) :E = elements(length-5).asInstanceOf[E]

		/** The sixth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _5[E](implicit tpe :TypeAt[H *: T, _5, E]) :E = elements(length-6).asInstanceOf[E]

		/** The seventh element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _6[E](implicit tpe :TypeAt[H *: T, _6, E]) :E = elements(length-7).asInstanceOf[E]

		/** The eighth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _7[E](implicit tpe :TypeAt[H *: T, _7, E]) :E = elements(length-8).asInstanceOf[E]

		/** The ninth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _8[E](implicit tpe :TypeAt[H *: T, _8, E]) :E = elements(length-9).asInstanceOf[E]

		/** The tenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _9[E](implicit tpe :TypeAt[H *: T, _9, E]) :E = elements(length-10).asInstanceOf[E]

		/** The eleventh element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _10[E](implicit tpe :TypeAt[H *: T, _10, E]) :E = elements(length-11).asInstanceOf[E]

		/** The twelfth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _11[E](implicit tpe :TypeAt[H *: T, _11, E]) :E = elements(length-12).asInstanceOf[E]

		/** The thirteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _12[E](implicit tpe :TypeAt[H *: T, _12, E]) :E = elements(length-13).asInstanceOf[E]

		/** The fourteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _13[E](implicit tpe :TypeAt[H *: T, _13, E]) :E = elements(length-14).asInstanceOf[E]

		/** The fifteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _14[E](implicit tpe :TypeAt[H *: T, _14, E]) :E = elements(length-15).asInstanceOf[E]

		/** The sixteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _15[E](implicit tpe :TypeAt[H *: T, _15, E]) :E = elements(length-16).asInstanceOf[E]

		/** The seventeenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _16[E](implicit tpe :TypeAt[H *: T, _16, E]) :E = elements(length-17).asInstanceOf[E]

		/** The eighteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _17[E](implicit tpe :TypeAt[H *: T, _17, E]) :E = elements(length-18).asInstanceOf[E]

		/** The nineteenth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _18[E](implicit tpe :TypeAt[H *: T, _18, E]) :E = elements(length-19).asInstanceOf[E]

		/** The twentieth element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _19[E](implicit tpe :TypeAt[H *: T, _19, E]) :E = elements(length-20).asInstanceOf[E]

		/** The twenty-first element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _20[E](implicit tpe :TypeAt[H *: T, _20, E]) :E = elements(length-21).asInstanceOf[E]

		/** The twenty-second element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _21[E](implicit tpe :TypeAt[H *: T, _21, E]) :E = elements(length-22).asInstanceOf[E]

		/** The twenty-third element of this tuple. If the tuple has fewer elements, no implicit argument will be available making the call impossible. */
		def _22[E](implicit tpe :TypeAt[H *: T, _22, E]) :E = elements(length-23).asInstanceOf[E]



		/** Retrieves the `n`-th element of this tuple as a value of `LUB` type of all member types (that is the least
		  * generic type `U` such that all elements of this tuple conform to `U`. This is the only way to access the
		  * elements based on dynamic index values. If the index is statically known, consider instead
		  * [[Tuple.*:.apply[N](n:N)(implicit tpe:TypeAt[H*:T,N,X]]]
		  * @param n zero based index of the element to retrieve.
		  * @param lub implicit witness providing the least upper bound for member types of this tuple
		  * @tparam E the least upper bound for types of all elements in this tuple
		  */
		def apply[E](n :Int)(implicit lub :TupleLUB[H *: T, E]) :E = {
			val len = length
			if (n < 0 || n >= length)
				throw new IndexOutOfBoundsException(s"$this($n)")
			elements(len - 1 - n).asInstanceOf[E]
		}

		/** Retrieves the `n`-th element of this tuple. The value of `n` must be statically known and encoded as a natural
		  * number type `Nat`. See [[Nat]] for constants such as [[Nat._0]], [[Nat._1]], ''etc.''.
		  * @param n zero-based index of the element to retrieve statically encoded as a recursively nested type.
		  * @param tpe implicit witness providing the type of the `n`-th element in this tuple.
		  * @tparam N Recursively constructed type encoding the index of the retrieved element.
		  * @tparam X exact type of the retrieved element provided by an implicit parameter.
		  */
		def apply[N <: Nat, X](n :N)(implicit tpe :TypeAt[H *: T, N, X]) :X =
			elements(length - 1 - n.number).asInstanceOf[X]


		/** First (leftmost) element of this tuple - same as `this._0`. */
		def head :H = elements(length-1).asInstanceOf[H]

		/** A tuple consisting of all elements from this tuple except the first. */
		def tail :T = {
			val count = length
			if (count == 1) <*>
			else new *:(elements, -count + 1)
		}.asInstanceOf[T]



		def drop[N <: Nat, R <: Tuple](n :N)(implicit restType :TupleDrop[N, H *: T, R]) :R = {
			val count = length
			if (n.number >= count) <*>.asInstanceOf[R]
			else new *:[Any, Tuple](elements, n.number - count).asInstanceOf[R]
		}


		def take[N <: Nat, R <: Tuple](n :N)(implicit initType :TupleTake[N, H *: T, R]) :R =
			if (n.number <= 0) <*>.asInstanceOf[R]
			else {
				val count = length
				if (n.number >= count) this.asInstanceOf[R]
				else new *:[Any, <*>](elements.slice(count-n.number, count), n.number).asInstanceOf[R]
			}


		def dropRight[N <: Nat, R <: Tuple](n :N)(implicit resType :TupleDropRight[N, H *: T, R]) :R = {
			val count = length; val num = n.number
			if (num >= count) <*>.asInstanceOf[R]
			else new *:[Any, Tuple](elements.slice(num, count), count - num).asInstanceOf[R]
		}


		def takeRight[N <: Nat, R <: Tuple](n :N)(implicit resType :TupleTakeRight[N, H *: T, R]) :R =
			if (n.number <= 0) <*>.asInstanceOf[R]
			else {
				val count = length; val num = n.number
				if (num >= count) this.asInstanceOf[R]
				else new *:[Any, Tuple](elements, num).asInstanceOf[R]
			}

		def slice[F <: Nat, U <: Nat, R0 <: Tuple, R <:Tuple](from :F, until :U)(implicit takeType :TupleTake[U, H *: T, R0], dropType :TupleDrop[F, R0, R]) :R = {
			val count = length; val f = from.number; val u = until.number
			if (u <= 0 || f >= count) <*>.asInstanceOf[R]
			else if (f <= 0 && u >= count) this.asInstanceOf[R]
			else new *:[Any, Tuple](elements.slice(count - u, count - f), u - f).asInstanceOf[R]
		}



		/** Returns a sequence containing all elements of this tuple from left to right. The element type is calculated
		  * as the least upper bound of all member types in this tuple.
		  * @param elemType implicit witness providing the most specific type to which all elements of this tuple conform.
		  * @tparam E a type to which all elements of this tuple conform.
		  */
		def toSeq[E](implicit elemType :TupleLUB[H *: T, E]) :Seq[E] = {
			var res :List[E] = Nil; var i = 0; val count = length
			while (i < count) {
				res = elements(i).asInstanceOf[E]::res
				i += 1
			}
			res
		}


		/** Prepends a new element to the left of this tuple.
		  * Returned tuple will have the element `head` at index `0`, followed by all elements of this tuple in the same order.
		  */
		def *:[X](head :X): X *: H *: T = {
			var count = len //store in a variable as signum of len might change concurrently
			if (elements.length < count) synchronized { //count >0 and there is space for a new  element in the array
				count = len
				if (count > 0) {
					len = -count
					elements(count) = head
					return new *:(elements, count+1)
				}
			}
			if (count < 0) count = -count
			val copy = new Array[Any](elements.length + 1)
			Array.copy(elements, 0, copy, 0, count)
			copy(count) = head
			new *:(copy, count + 1)
		}



		override def productElement(n :Int) :Any = elements(length - n)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[*:[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case t : *:[_, _] =>
				(t eq this) || (t canEqual this) && t.length == length && {
					var i = length; val others = t.array
					do { i -=1 } while (i >= 0 && elements(i) == others(i))
					i < 0
				}
			case _ => false
		}

		override def toString :String = {
			val res = new StringBuilder
			var i = length - 1
			res append '('
			res append elements(i)
			i -= 1
			while (i >= 0) {
				res append " *: "
				res append elements(i)
				i -= 1
			}
			res append ')'
			res.toString
		}

	}




/*
	/** Implicit conversion from any type into a singleton tuple which can be extended by prepending new elements
	  * with the [[*:.*:]] method
	  */
	implicit def *:[X](last :X): X *: <*> = {
		val array = new Array[Any](4)
		array(0) = last
		new *:[X, <*>](array, 1)
	}
*/
	/** Implicit conversion extending any type with a `*:` method to create two element tuples.
	  * `x *: y` is equivalent to `x *: y *: <*>`, both creating a pair of type `X *: Y *: <*>`.
	  * If you wish to have an empty element tupe as the last element of another tuple, use `x **: <*>` or one of
	  * direct `apply` factory methods of this object.
	  */
	implicit def *:[X](last :X) :PairConstructor[X] = new PairConstructor(last)


	/** Extractor separating the first element of the tuple from the rest in pattern matching. Can be used in the recursive
	  * infix notation.
	  */
	object *: {
		def unapply[H, T <: Tuple](tuple :H *: T) :Option[(H, T)] = Some(tuple.head, tuple.tail)
	}



	/** An implicit witness providing the type `X` of `N`-th element in the tuple `T`.
	  * @tparam T a tuple type.
	  * @tparam N a zero-based index of an element in the tuple encoded as a type.
	  * @tparam X type of `n`-th element in `T` starting from the left.
	  * @see [[Nat]]
	  */
	final class TypeAt[-T <: Tuple, N <: Nat, +X] private()

	object TypeAt {
		private[this] val instance = new TypeAt[Tuple, Nat, Any]

		implicit def type_0[H, T <: Tuple] :TypeAt[H *: T, _0, H] = instance.asInstanceOf[TypeAt[H *: T, _0, H]]

		implicit def type_n[T <: Tuple, N <: Nat, X](implicit tpe :TypeAt[T, N, X]) :TypeAt[Any *: T, ++[N], X] =
			instance.asInstanceOf[TypeAt[Any *: T, ++[N], X]]
	}


	/** Implicit witness providing the upper type bound `U` (if unrestricted, the ''least upper bound'' for all elements
	  * of tuple `T`).
	  * @tparam T a tuple type
	  * @tparam U a type such that tuple(''_i'') :U for all `0 &lt;= i &lt; tuple.length`.
	  */
	final class TupleLUB[-T <: Tuple, +U] private()


	object TupleLUB {
		implicit val nothing :TupleLUB[<*>, Nothing] = new TupleLUB[<*>, Nothing]

		implicit def lub[H, T <: Tuple, X, U](implicit ev :TupleLUB[T, X], lub :UpperBound[H, X, U]) :TupleLUB[H *: T, U] =
			nothing.asInstanceOf[TupleLUB[H *: T, U]]
	}


	/** Implicit evidence atdebug that `N` is the type-encoded length of tuple type `T`. */
	final class TupleLength[N <: Nat, -T <: Tuple] private ()

	object TupleLength {
		implicit val zeroLength :TupleLength[_0, <*>] = new TupleLength[_0, <*>]

		@inline implicit def longer[N <: Nat, T <: Tuple](implicit tailLength :TupleLength[N, T]) :TupleLength[++[N], Any *: T] =
			zeroLength.asInstanceOf[TupleLength[++[N], Any *: T]]
	}


	/** Implicit evidence atdebug that `R` is the type resulting from dropping `N` first elements from tuple type `T`. */
	final class TupleDrop[N <: Nat, -T <: Tuple, R <: Tuple] private ()

	object TupleDrop {
		private[this] val instance = new TupleDrop[_0, Tuple, Tuple]

		@inline implicit def drop0[T <: Tuple] :TupleDrop[_0, T, T] = instance.asInstanceOf[TupleDrop[_0, T, T]]

		@inline implicit def dropMore[N <: Nat, T <: Tuple, R <: Tuple](implicit init :TupleDrop[N, T, R]) :TupleDrop[++[N], Any *: T, R] =
			instance.asInstanceOf[TupleDrop[++[N], Any *: T, R]]
	}



	/** Implicit witness atdebug that `R` is the type resulting from taking `N` first elements from tuple type `T`. */
	final class TupleTake[N <: Nat, -T <: Tuple, R <: Tuple] private ()

	object TupleTake {
		private[this] val instance = new TupleTake[_0, <*>, <*>]

		@inline implicit def take0[T <: Tuple] :TupleTake[_0, T, <*>] = instance.asInstanceOf[TupleTake[_0, T, <*>]]

		@inline implicit def takeMore[N <: Nat, H, T <: Tuple, R <:Tuple](implicit tail :TupleTake[N, T, R]) :TupleTake[++[N], H *: T, H *: R] =
			instance.asInstanceOf[TupleTake[++[N], H *: T, H *: R]]
	}



	/** Implicit evidence atdebug that `R` is the type resulting from dropping `N` last (right) elements from tuple type `T`. */
	final class TupleDropRight[N <: Nat, -T <: Tuple, R <: Tuple] private ()

	object TupleDropRight {
		private[this] val instance = new TupleDropRight[_0, Tuple, Tuple]

		@inline implicit def dropAll[N <: Nat, T <:Tuple](implicit length :TupleLength[N, T]) :TupleDropRight[N, T, <*>] =
			instance.asInstanceOf[TupleDropRight[N, T, <*>]]

		@inline implicit def dropNoMore[N <: Nat, H, T <: Tuple, R <: Tuple](implicit init :TupleDropRight[N, T, R]) :TupleDropRight[N, H *: T, H *: R] =
			instance.asInstanceOf[TupleDropRight[N, H *: T, H *: R]]
	}



	/** Implicit witness atdebug that `R` is the type resulting from taking `N` first elements from tuple type `T`. */
	final class TupleTakeRight[N <: Nat, -T <: Tuple, R <: Tuple] private ()

	object TupleTakeRight {
		private[this] val instance = new TupleTakeRight[_0, <*>, <*>]

		@inline implicit def takeAll[N <: Nat, T <: Tuple](implicit length :TupleLength[N, T]) :TupleTakeRight[N, T, T] =
			instance.asInstanceOf[TupleTakeRight[N, T, T]]

		@inline implicit def takeNoMore[N <: Nat, T <: Tuple, R <:Tuple](implicit tail :TupleTakeRight[N, T, R]) :TupleTakeRight[N, Any *: T, R] =
			instance.asInstanceOf[TupleTakeRight[N, Any *: T, R]]
	}


}





/** Type level implementation of natural numbers used to index variable length tuples. In order to identify the type
  * of the element at the given index, the latter has to be statically encoded. A special subtype [[Nat._0]] is used
  * to denote zero, while every positive value is represented by recursively applying successor subclass [[Nat.++]]
  * to the zero type. For example, `++[++[_0]]` is the representation of the natural number `2`.
  * @param number numerical value of this number
  */
sealed abstract class Nat protected (val number :Int) {
	override def toString :String = number.toString

	override def hashCode :Int = number

	override def equals(that :Any) :Boolean = that match {
		case n :Nat => n.number == number
		case _ => false
	}
}





/** Low priority implicit [[Nat]] values for every natural number by recursively applying it to itself. */
sealed abstract class NatImplicitInduction {
	/** Given an implicit encoding of natural number `n` provide implicit value representing `n+1`. */
	implicit def ++[N <: Nat](implicit n :N) : ++[N] = new ++(n)
}



/** Implicit values and type aliases for encoding of the first 23 natural numbers as types. */
object Nat extends NatImplicitInduction {
	/** Type encoding of natural number `0`. */
	final class _0 private[Nat]() extends Nat(0)

	/** Implicitly available constant representing the natural number `0` at type level. */
	final implicit val _0 : _0 = new _0

	/** Type constructor implementing successor operation for natural numbers. Given a type-level encoding of number `N`,
	  * creates a type representing `N+1`.
	  * @param pred preceeding number
	  * @tparam N a natural number.
	  */
	final class ++[N <: Nat] private[tuples](pred :N) extends Nat(pred.number+1)

	/** Implicit extension of `Nat` number types providing a successor operator returning the value for the next natural number. */
	implicit class N_++[N <: Nat](private val n :N) extends AnyVal {
		def ++ : ++[N] = new ++[N](n)
	}

	/** Type encoding of natural number `1`. */
	type _1 = ++[_0]

	/** Implicitly available constant representing the natural number `1` at type level. */
	implicit val _1 : _1 = new ++(_0)


	/** Type encoding of natural number `2`. */
	type _2 = ++[_1]

	/** Implicitly available constant representing the natural number `2` at type level. */
	implicit val _2 : _2 = new ++(_1)


	/** Type encoding of natural number `3`. */
	type _3 = ++[_2]

	/** Implicitly available constant representing the natural number `3` at type level. */
	implicit val _3 : _3 = new ++(_2)


	/** Type encoding of natural number `4`. */
	type _4 = ++[_3]

	/** Implicitly available constant representing the natural number `4` at type level. */
	implicit val _4 : _4 = new ++(_3)


	/** Type encoding of natural number `5`. */
	type _5 = ++[_4]

	/** Implicitly available constant representing the natural number `5` at type level. */
	implicit val _5 : _5 = new ++(_4)


	/** Type encoding of natural number `6`. */
	type _6 = ++[_5]

	/** Implicitly available constant representing the natural number `6` at type level. */
	implicit val _6 : _6 = new ++(_5)


	/** Type encoding of natural number `7`. */
	type _7 = ++[_6]

	/** Implicitly available constant representing the natural number `7` at type level. */
	implicit val _7 : _7 = new ++(_6)


	/** Type encoding of natural number `8`. */
	type _8 = ++[_7]

	/** Implicitly available constant representing the natural number `8` at type level. */
	implicit val _8 : _8 = new ++(_7)


	/** Type encoding of natural number `9`. */
	type _9 = ++[_8]

	/** Implicitly available constant representing the natural number `9` at type level. */
	implicit val _9 : _9 = new ++(_8)


	/** Type encoding of natural number `10`. */
	type _10 = ++[_9]

	/** Implicitly available constant representing the natural number `10` at type level. */
	implicit val _10 : _10 = new ++(_9)


	/** Type encoding of natural number `11`. */
	type _11 = ++[_10]

	/** Implicitly available constant representing the natural number `11` at type level. */
	implicit val _11 : _11 = new ++(_10)


	/** Type encoding of natural number `12`. */
	type _12 = ++[_11]

	/** Implicitly available constant representing the natural number `12` at type level. */
	implicit val _12 : _12 = new ++(_11)


	/** Type encoding of natural number `13`. */
	type _13 = ++[_12]

	/** Implicitly available constant representing the natural number `13` at type level. */
	implicit val _13 : _13 = new ++(_12)


	/** Type encoding of natural number `14`. */
	type _14 = ++[_13]

	/** Implicitly available constant representing the natural number `14` at type level. */
	implicit val _14 : _14 = new ++(_13)


	/** Type encoding of natural number `15`. */
	type _15 = ++[_14]

	/** Implicitly available constant representing the natural number `15` at type level. */
	implicit val _15 : _15 = new ++(_14)


	/** Type encoding of natural number `16`. */
	type _16 = ++[_15]

	/** Implicitly available constant representing the natural number `16` at type level. */
	implicit val _16 : _16 = new ++(_15)


	/** Type encoding of natural number `17`. */
	type _17 = ++[_16]

	/** Implicitly available constant representing the natural number `17` at type level. */
	implicit val _17 : _17 = new ++(_16)


	/** Type encoding of natural number `18`. */
	type _18 = ++[_17]

	/** Implicitly available constant representing the natural number `18` at type level. */
	implicit val _18 : _18 = new ++(_17)


	/** Type encoding of natural number `19`. */
	type _19 = ++[_18]

	/** Implicitly available constant representing the natural number `19` at type level. */
	implicit val _19 : _19 = new ++(_18)


	/** Type encoding of natural number `20`. */
	type _20 = ++[_19]

	/** Implicitly available constant representing the natural number `20` at type level. */
	implicit val _20 : _20 = new ++(_19)


	/** Type encoding of natural number `21`. */
	type _21 = ++[_20]

	/** Implicitly available constant representing the natural number `21` at type level. */
	implicit val _21 : _21 = new ++(_20)


	/** Type encoding of natural number `22`. */
	type _22 = ++[_21]

	/** Implicitly available constant representing the natural number `22` at type level. */
	implicit val _22 : _22 = new ++(_21)

}
