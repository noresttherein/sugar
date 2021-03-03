import net.noresttherein.slang.vars.Out


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
	val out = new Out[Int] //Out[Int]()
	println(out)
	out.value = 1
	println(out)
	println(out.value)
}
