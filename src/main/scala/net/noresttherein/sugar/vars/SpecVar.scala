package net.noresttherein.sugar.vars




/** A proof of concept of @specialized Var implementation which caches the box value.
  * @author Marcin Mościcki
  */
private trait SpecVar[@specialized(Int) X] {
	def value :X
	def value_=(value :X) :Unit
}




private object SpecVar {
	@inline def apply[X](implicit valueType :VarType[X]) :SpecVar[X] = valueType()

	abstract class VarType[@specialized(Int) X] {
		def apply() :SpecVar[X]
		def apply(value :X) :SpecVar[X] = { val res = apply(); res.value = value; res }
	}
	implicit object IntVar extends VarType[Int] {
		override def apply() :SpecVar[Int] = new IntVar with Generic[Int]
	}
	implicit def refVar[X] :VarType[X] = new VarType[X] {
		override def apply() :SpecVar[X] = new RefVar[X]
	}
	private abstract class VarCache[X] {
		var box :X = _
		def get :X
		def set(value :X) :Unit
	}
	private class IntVar extends VarCache[Int] with SpecVar[Int] {
		private[this] var x :Int = _
		override def value :Int = x
		override def value_=(value :Int) :Unit = x = value
		final override def get :Int = x
		final override def set(value :Int) :Unit = x = value
	}

	private class RefVar[X] extends SpecVar[X] {
		override var value :X = _
	}

	private trait Generic[X] extends VarCache[X] with SpecVar[X] {
		override def value :X = {
			var res = box
			if (res == null) {
				res = get
				box = res
			}
			res
		}
		override def value_=(value :X) :Unit = {
			box = value
			set(value)
		}
	}


	var int = SpecVar[Int]
	var x = int.value
	int.value = 100000000
	x = int.value

	generic(int, 300000000)

	x = int.value

	def generic[T](spec :SpecVar[T], value :T) :Unit = {
		var x = spec.value
		spec.value = value
		x = spec.value
	}
}
