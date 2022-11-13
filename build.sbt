organization := "net.noresttherein"

name := "sugar"

version := "moonshine"

scalaVersion := "2.13.10"


Compile / fork := true

Compile / javaOptions ++= Seq("-Xmx2G")


Test / testOptions ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


libraryDependencies ++= Seq( //todo: make shapeless optional
	"com.chuusai" %% "shapeless" % "2.3.10",
	"net.bytebuddy" % "byte-buddy" % "1.12.18",
	"org.scala-lang" % "scala-library" % "2.13.10",
	"org.scala-lang" % "scala-reflect" % "2.13.10",
	"org.scalatest" %% "scalatest" % "3.2.14" % "test",
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
//	"-Vimplicits",
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



