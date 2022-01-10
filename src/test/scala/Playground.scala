import net.noresttherein.slang.{implicits, numeric}
import net.noresttherein.slang.time.{DateTime, Timestamp, ZoneDateTime}
import net.noresttherein.slang.time.dsl.IntTimeLapseMethods
import net.noresttherein.slang.vars.Out


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
	def array(size :Int) = Array.iterate(0, size)(_ + 1)

	val WarmUpMillis = 5*60*1000
	val TestMillis   = 5*60*1000
	val StepSize     = 10*1000*1000
	val TestSize     = 100
	val TestedSizes  = Seq(0, 8, 16, 32, 64, 128)

	val store = Array.ofDim[Array[Int]](StepSize)

	def test(size :Int) = {
		val template = array(size)
		var end = ZoneDateTime.after(WarmUpMillis.millis)
		System.err.println("Warm up until " + end + ", array size: "+ size + ".")
		while (Timestamp.now < end) {
			var iterations = StepSize
			while (iterations > 0) {
				iterations -= 1
				store(iterations) = Array.copyOf(template, size)
			}
			System.err.print(".")
		}
		System.err.println()
//		end = ZoneDateTime.after(TestMillis.millis)
		val start = ZoneDateTime.now
		System.err.println(start + ": test Array(" + size + ")")
		var i = 0
		while (i < TestSize) {
			var iterations = StepSize
			while (iterations > 0) {
				iterations -= 1
				store(iterations) = Array.copyOf(template, size)
			}
			i += 1
		}
		end = ZoneDateTime.now
		System.err.println("Created " + TestSize.toLong * StepSize + " objects in " + (end - start) + ".")
		System.err.println("Useless info: " + (0 /: store) {_ ^ _.sum })
		System.gc()
	}


	TestedSizes foreach test
}
