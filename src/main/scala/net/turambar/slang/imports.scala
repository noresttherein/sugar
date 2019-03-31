package net.turambar.slang

import net.turambar.slang



/** Type aliases for most useful types and implicit conversions in the library removing the need to search for a declaration
  * among  subpackages and allowing wholesale import of (almost) everything by `import net.turambar.slang._` for the brave.
  * It also enforces the convention that the identifier of an import for an implicit conversion is the same as the name
  * of the most prominent method of the type extension. This in particular means starting with lower case names of
  * both conversion methods and target type extensions.
  *
  * @author Marcin Mościcki marcin@moscicki.net
  */
object imports {

	final val && = matching.&&



	type RefOpt[+T <: AnyRef] = optional.RefOpt[T]
	final val RefOpt = optional.RefOpt


	type ifTrue = optional.IfTrue
	@inline implicit final def ifTrue(condition :Boolean) :ifTrue = new ifTrue(condition)

	type satisfying[T] = optional.Satisfying[T]
	@inline implicit final def satisfying[T](subject :T) :satisfying[T] = new satisfying(subject)

	type providing[T] = optional.Providing[T]
	@inline implicit final def providing[T](subject :T) :providing[T] = new providing[T](subject)

	type customEnsuring[T] = optional.CustomEnsuring[T]
	@inline implicit final def customEnsuring[T](subject :T) :customEnsuring[T] = new customEnsuring(subject)

	type Var[@specialized(slang.Var.SpecializedTypes) T] = slang.Var[T]
	final val Var = slang.Var


	type foldWhile[T] = repeatedly.foldWhile[T]
	@inline implicit final def foldWhile[T](col :Iterable[T]) :foldWhile[T] = new foldWhile(col)

	type repeatTimes = repeatedly.repeatTimes
	@inline implicit final def repeatTimes(iterations :Int) :repeatTimes = new repeatTimes(iterations)


}
