import scala.collection.immutable.{ArraySeq, BitSet}

import net.noresttherein.slang.exceptions.{rethrow, RethrowableException}
import net.noresttherein.slang.numeric.Ratio


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {

	val set = Set.from(BitSet.empty + 42 :Iterable[Any])
	set.contains("oops.")
/*
	class Test(msg :String, cause :Throwable = null, rethrown :Boolean = false)
		extends RethrowableException(msg, cause, rethrown)

	def test(): Unit = {
		rethrow(rabbitHole()) { "rethrown exception" }
	}
	def rabbitHole(i :Int = 10) :Nothing =
		if (i == 0) well()
		else rabbitHole(i - 1)
	def well(i :Int = 10) :Nothing =
		if (i == 0)
			throw new Test("deep exception")
		else
			well(i - 1)
	try {
		test()
	} catch {
		case e :Test =>
			System.err.println("Normal stack trace: ")
			e.printStackTrace(System.err)
//			System.err.println("stack trace: ")
//			System.err.println(e.getStackTrace.mkString("\n"))
			System.err.println()
			System.err.println()
			System.err.println("suffix stack trace: ")
			System.err.println(e.stackTrace.mkString("\n"))
			System.err.println()
			System.err.println()
//			System.err.println("stack trace: ")
//			System.err.println(e.stackTrace.mkString("\n"))
			System.err.println("equal? " + (ArraySeq.unsafeWrapArray(e.getStackTrace) == e.stackTrace))
	}
*/

}



