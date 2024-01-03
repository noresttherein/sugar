package net.noresttherein.sugar.optional




@SerialVersionUID(Ver)
object implicits {
	@inline implicit def anythingToSome[X](x :X) :Some[X] = Some(x)
}
