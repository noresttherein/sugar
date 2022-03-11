package net.noresttherein.slang.numeric

import net.noresttherein.slang.numeric.Ratio.RatioFactory


trait extensions extends Any {
	@inline implicit final def method_%/(self :Int) = new RatioFactory(self)
	@inline implicit final def method_%/(self :Long) = new RatioFactory(self)
}
