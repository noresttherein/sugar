val TLD = "net"

val domain = "noresttherein"

organization := TLD + "." + domain

name := "sugar"

version := "moonshine"

scalaVersion := "2.13.16"


Compile / fork := true

Compile / javaOptions ++= Seq("-Xmx12G")

Test / sourceGenerators += Def.task {
	//Create a copy of class MatrixBuffer which uses a much lower max length for a single dimension array
	// in order to comfortably test all border cases on small data sets.
	val collectionsSourceDir = (Compile / sourceDirectory).value / "scala" / TLD / domain / name.value / "collections"
	val collectionsTestManagedDir = (Test / sourceManaged).value / TLD / domain / name.value / "collections"

	val MatrixBuffer     = collectionsSourceDir / "MatrixBuffer.scala"
	val TestMatrixBuffer = collectionsTestManagedDir / "TestMatrixBuffer.scala"
	PatchFile(MatrixBuffer, TestMatrixBuffer).patchAll(
		("MatrixBuffer", "TestMatrixBuffer"),
		("final val Dim1Bits = \\d*", "final val Dim1Bits = 4"),
		("final val Dim2Bits = \\d*", "final val Dim2Bits = 27"),
		("final val MinSize1 = \\d*", "final val MinSize1 = 4"),
		("final val NewSize1 = \\d*", "final val NewSize1 = 4"),
		("final val MinSize2 = \\d*", "final val MinSize2 = 4"),
		("final val NewSize2 = \\d*", "final val NewSize2 = 4"),
		("//\\w*override def toString", "\toverride def toString")
	)

	IO.reader(MatrixBuffer) { reader =>
		val lines = reader.lines.toList.asScala.map { line =>
			line.replaceAllLiterally("MatrixBuffer", "TestMatrixBuffer")
			    .replaceAll("final val Dim1Bits = \\d*", "final val Dim1Bits = 4")
			    .replaceAll("final val Dim2Bits = \\d*", "final val Dim2Bits = 27")
			    .replaceAll("final val MinSize1 = \\d*", "final val MinSize1 = 4")
			    .replaceAll("final val NewSize1 = \\d*", "final val NewSize1 = 4")
			    .replaceAll("final val MinSize2 = \\d*", "final val MinSize2 = 4")
			    .replaceAll("final val NewSize2 = \\d*", "final val NewSize2 = 4")
			    .replaceAll("//\\w*override def toString", "\toverride def toString")
		}
		IO.writeLines(output, lines)
	}

	//Create a copy of TreeSeq sequence using a much lower tree rank/order, so that relatively small sequences
	// are represented by deep trees, in order to comfortably test the implementations for higher levels.
	val TreeSeq     = collectionsSourceDir / "TreeSeq.scala"
	val TestTreeSeq = collectionsTestManagedDir / "TestTreeSeq.scala"
	PatchFile(TreeSeq, TestTreeSeq).patchAll(
		("TreeSeq", "TestTreeSeq"),
		("//assert", "assert"),
		("//\\w*override def toString", "\toverride def toString"),
		("final val Rank = \\d*", "final val Rank = 4")
	)

	Seq(TestTreeSeq, TestMatrixBuffer)
}.taskValue


Test / testOptions ++= Seq(Tests.Filter { s =>
	try {
		val testClass = Test.getClass.getClassLoader.loadClass(s + "$")
		!(testClass.isInterface || java.lang.reflect.Modifier.isAbstract(testClass.getModifiers))
	} catch {
		case e :ClassNotFoundException =>
			System.err.println(e.toString)
			false
	}
})



libraryDependencies ++= Seq( //todo: make shapeless optional
	"org.scala-lang" % "scala-compiler" % scalaVersion.value,
	"org.scala-lang" % "scala-library" % scalaVersion.value,
	"org.scala-lang" % "scala-reflect" % scalaVersion.value,
	"com.chuusai" %% "shapeless" % "2.3.13",
	"net.bytebuddy" % "byte-buddy" % "1.17.2",

	"junit" % "junit" % "4.13.2" % "test",
	"org.scalatest" %% "scalatest" % "3.2.19" % Test,
	"org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
//	"org.openjdk.jmh" % "jmh-core" % "1.36" % Test,
//	"org.openjdk.jmh" % "jmh-generator-annprocess" % "1.36" % Test,
)


scalacOptions ++= Seq(
//	"-J-Xss8m",
//	"-verbose",
//	"-Ydebug",
//	"-Ylog:0-100",
//	"-Ytyper-debug",
//	"-Ylog:extmethods",
//	"-Ycheck:extmethods",
//	"-Ylog-classpath",
	"-Vimplicits",
//	"-Vimplicits-verbose-tree",
//	"-Vtype-diffs",
//	"-Vprint-types",
//	"-Xdev",
//	"-Xprint:0-100",
//	"-Xprint:extmethods",
	"-Xlog-reflective-calls",
	"-Xlint:delayedinit-select,implicit-not-found,option-implicit,poly-implicit-overload,nullary-unit",
//	"-W",
	"-Wconf:cat=deprecation:w,cat=deprecation&msg=foldLeft:silent,cat=deprecation&msg=foldRight:silent,cat=feature:w",
	"-Wunused:patvars,privates,locals",
	"-feature",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)
