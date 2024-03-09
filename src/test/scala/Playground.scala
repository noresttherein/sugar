
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, View}
import scala.collection.immutable.{AbstractSeq, ArraySeq, HashSet, SortedSet}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, IArray, IRefArray}
import net.noresttherein.sugar.collections.{ArraySliceBuffer, Cat, Cuboid2Iterator, Fingers, RelayArray, StrictView, StringSet, UnorderedItems, ValIterator}
import net.noresttherein.sugar.extensions.{ClassExtension, IterableExtension, IterableOnceExtension, SeqExtension, classNameMethods}
import net.noresttherein.sugar.slang.SerializationProxy



private object Playground extends App {
	import Input._

	val fingers = inserted
	//.take(10)//Fingers from input
	println(fingers.length)
	val vec = Vector.from(fingers)
	println(fingers == vec)
	//	val res = concat concat concatArg
//	val expect2 = vec.take(insertedAllIdx) :++ insertedElems :++ vec.drop(insertedAllIdx)
//		explodedFirst :+ explodedSecond :++ explodedRest :++
//		vec.drop(math.max(0, explodedIdx) + 2 + explodedRest.length)
//	println(expect2.length)
//		insertedElem :++
//		vec.drop(insertedIdx)
	val expect = vec.inserted(insertedIdx, insertedElem)
	val res    = fingers.inserted(insertedIdx, insertedElem)
//	println(res.length.toString + " vs " + expect.length)
//	println(res.length.toString + " vs " + expect2.length)
	println(res == expect)
//	println(res == expect2)
//	println(expect == expect2)
//	println(res.zip(expect).zipWithIndex.collectFirst { case ((a, b), i) if a != b => "#" + i + ": " + a + " vs " + b })

/*
	val res = fingers.flatten
	val expect = vec.flatten
	println(res.length + "/" + expect.length)

	println(flatten.toList.map(_.length).scanLeft(0)(_ + _).tail.zipWithIndex.map { case (len, i) => "#" + i + ": " + len })
	println(fingers == vec)
	println(res == expect)
*/
	//	val i = fingers.flatMap((i :Int) => Fingers.iterate(i.toLong, 10)(_ * i))
	//	val j = input.indexOf(-1, Int.MinValue)

	//	println(i.toString + " vs " + j)


	/*
	var ok = true
	var i = 10000
	var jack = Fingers.from(0 until i)
	var template = Vector.from(0 until i)
	val removed = new ArrayBuffer[Int](i)
	while (i > 0 && ok) {
		val j = Random.nextInt(i)
		val correct = template.take(j) :++ template.drop(j + 1)
		removed += j
		try {
			val oneFewer = jack.removed(j)
			ok = oneFewer == correct
			if (ok) {
				jack = oneFewer
				template = correct
			} else {
				println(template.zipWithIndex.map(pair => pair._2.toString + ": " + pair._1))
				val oneFewer = jack.removed(j)
				println(oneFewer)
				println("removed: " + removed)
				println("removed(" + j + ")")
			}
		} catch {
			case e :Throwable =>
				println(template.zipWithIndex.map(pair => pair._2.toString + ": " + pair._1))
				val oneFewer = jack.removed(j)
				println(oneFewer)
				println("removed: " + removed)
				println("removed(" + j + ")")
		}
		i -= 1
	}
*/
	/*	while (i < 250 && ok) {
		try {
			template = Vector.from(0 until i)
			jack = (Fingers.newBuilder[Int] ++= (0 until i)).result()
			for { m <- 0 until i; n <- m until i } try {
				val slice = template.slice(m, n)
				ok = jack.slice(m, n) == slice
				if (!ok) {
					val wrong = jack.slice(m, n)
					wrong == slice
					println(jack.slice(m, n))
					println(i.toString + ": slice(" + m + ", " + n + ")")
				}
			} catch {
				case e :Throwable =>
					println(i.toString + ": slice(" + m + ", " + n + ")")
					val wrong = jack.slice(m, n)
					wrong == template.slice(m, n)
					throw e
			}
//			jack = jack :+ i :++ jack
//			template = template :+ i :++ template
//			jack = jack.inserted(i >> 1, i)
//			template = template.take(i >> 1) :+ i :++ template.drop(i >> 1)
//			jack = jack :+ i
//			template = template :+ i
//			jack = i +: jack
//			template = i +: template
//			ok = jack == template
			if (!ok) {
				println(i.toString + ":")
				println(jack)
				println(template)
				val iteratorDiff = jack.iterator.zipWithIndex.indexWhere { case (elem, i) => elem != template(i) }
				val applyDiff = template.indices.indexWhere(i => jack(i) != template(i))
				println(i.toString + ": iterator diff at " + iteratorDiff + "; apply diff at " + applyDiff + " /" + jack.length)
			}
		} catch {
			case e :Throwable =>
				System.err.println(i.toString + ": " + e)
				throw e
		}
		i += 1
	}
	println(jack)
*/
}



object Input {
	val insertedAll = Fingers(1, 2147483647, 1859636613, 1, 0, -729211159, 1, 1512417519, 1, 667429256, -1865318013, 2147483647, 0, -1639504250, 497856372, -2147483648, 1472097512, -1025449505, -1457002740, -268746065, -1, 125767510, -241029080, 0, 2033558783, -553447937, 553592161, -2147483648, 0, 1, 1324236394, 2147483647, -1, 81586242, 1, 472993624, 1, -1863794998, 1970115158, 0, -2147483648, 2111715083, -570305128, -1, 262623278, -211268409, -1, 22552673, -1, -2147483648, 1, 1728674345, 384409883, 2147483647, 95497736, -1, -1, -1, -1607584239, -2147483648, 715968305, 2017558676, 0, -2147483648, 0, -1087178169, 0, -2147483648, 1, 1, -2147483648, 1, -712493338, 505738392, -198822738, -1253078946, 0, 1, 2147483647, -556350540, 2090736717, 1555101400, -1, 1358028702, 2147483647, -1, -1939407947, 1361858573, -1565972791, -2147483648, 1534618789, -1354309672, -1383641157, 0, -2147483648, 1395789305, 530711397, 2018768087, -1867266469, -245062885, -1883998105, -2147483648, 1, -1, 1966207481, 2075465576, 2041594670, 471083250, -1, -1, -1, -2147483648, 1384686346, 0, 1662308474, -1, -431927133, -2147483648, -2147483648, -2147483648, 0, 1877708751, -995906533, -2030675093, -235489177, 0, 516416172, -179055647, 0, -1696558177, -2147483648, -2147483648, 536016141, 22943296, -527059444, 2090977208, 1, 1306672454, -541952008, -2147483648, 2147483647, 432877973, 1, 1, 0, 2110983651, 934244500, 0, 1972005379, -727758037, -1, 1614390997, 0, -908432569, 2147483647, -1416614886, -1, 1328108622, -831718923, -1, 0, -700378378, 2147483647, 1360752721, 1, 1091877132, 2147483647, 1, -1, -1623101484, 665560738, -1, 2147483647, 1, 0, -1, -2147483648, 1409793791, -338352477, -1072649353, -353277281, 435594481, 1, 1, 2147483647, -2147483648, -2078498186, -2147483648, 1, -581013160, 2147483647, -1, -954691339, -1, 483033325, -1, -2147483648, 2147483647, 2147483647, -2147483648, -1, 1882520012, -1613418468, 0, -2147483648, 2045933657, 0, -729798854, -1238666303, 0, -2147483648, -1, 2147483647, 1398425264, -2147483648, 0, 0, 704280639, -2147483648, -958716281, -2147483648, -1, 2147483647, -573113599, -2147483648, -2147483648, 0, 1, -879669772, -425950283, -799380730, 0, 1, 537022654, -1, -2147483648, -3039917, 1620710807, 0, -1585848788, 1, -797211279, -65741515, 1, -1158979163, 1, -1, 2147483647, -2147483648, 2098376769, 0, -36090854, -2147483648, -1, 1, -1339459431, 2147483647, -1327674992, 0, -2053333349, -1528931852, 75560907, -1973062114, 478360816, -1067043005, -650286284, 1, -1, 0, -2147483648, 2147483647, 1, 1, 2147483647, 0, 2147483647, 0, 0, 0, -2147483648, -1097620422, -2147483648, 0, 2147483647, -1359676598, 2147483647, -2147483648, 338619575, 1, -2147483648, -1048400212, -738193621, 2147483647, 1, -179964733, -2147483648, -1169847559, 689237667, -2147483648, 1178829064, 1, 1, -2147483648, 1467218990, -113371146, -2147483648, -886218974, 795981787, -2147483648, 1, -1853190888, 860593271, -1394300178, 1720984001, -1, -1, 1493261443, -2147483648, 1, 0, 50066574, 0, 1966061788, 1, 1, 1, 1171282811, 0, -2147483648, -1, -436097855, 557128873, 1495355415, 1, -1645527862, -1749318405, 973817969, -943211837, 0, 2147483647, 1814890199, 0, 1, 2147483647, 2147483647, -2147483648, 179509714, 852923959, 1, 0, -493486807, 1, -410654494, -1, 1652070524, 66175342, 655223553, 2147483647, -1231335574, -524582672, -2147483648, -2147483648, -2147483648, -1815072957, 0, 2041280617, -1, -1252935858, -1184576745, 2147483647, -2147483648, 0, 1569268727, 2147483647)
	val insertedAllIdx = 1
	val insertedElems = Fingers(807231205, -1, 1096978210, 2077280542)

	val inserted = Fingers(-2147483648, 0, 1, -2040211601, 1, 2147483647, 0, 1)
	val insertedIdx = -1
	val insertedElem = Int.MinValue


}