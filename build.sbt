val TLD = "net"

val domain = "noresttherein"

organization := TLD + "." + domain

name := "sugar"

version := "moonshine"

scalaVersion := "2.13.11"


Compile / fork := true

Compile / javaOptions ++= Seq("-Xmx4G")

Test / sourceGenerators += Def.task {
	import scala.jdk.CollectionConverters.asScalaBufferConverter

	//Create a copy of class MatrixBuffer which uses a much lower max length for a single dimension array
	// in order to comfortably test all border cases on small data sets.
	val input = (Compile / sourceDirectory).value / "scala" / TLD / domain / name.value / "collections" / "MatrixBuffer.scala"
	val output = (Test / sourceManaged).value / TLD / domain / name.value / "collections" / "TestMatrixBuffer.scala"

	IO.reader(input) { reader =>
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
	Seq(output)
}.taskValue

/*
Test / testOptions ++= Seq(Tests.Filter { s =>
//	try {
		println(getClass.getClassLoader.)
		val testClass = Class.forName(s + "$")
		!(testClass.isInterface || java.lang.reflect.Modifier.isAbstract(testClass.getModifiers))
//	} catch {
//		case _ :ClassNotFoundException => false
//	}
})
*/




libraryDependencies ++= Seq( //todo: make shapeless optional
	"org.scala-lang" % "scala-compiler" % scalaVersion.value,
	"org.scala-lang" % "scala-library" % scalaVersion.value,
	"org.scala-lang" % "scala-reflect" % scalaVersion.value,
	"com.chuusai" %% "shapeless" % "2.3.10",
	"net.bytebuddy" % "byte-buddy" % "1.14.2",

	"junit" % "junit" % "4.13.2" % "test",
	"org.scalatest" %% "scalatest" % "3.2.15" % "test",
	"org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
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
	"-Wconf:cat=deprecation&msg=foldLeft:silent,cat=deprecation&msg=foldRight:silent,cat=deprecation:w,cat=feature:w",
	"-Wunused:patvars,privates,locals",
	"-feature",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



