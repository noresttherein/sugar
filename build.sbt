organization := "net.noresttherein"

name := "sugar"

version := "moonshine"

scalaVersion := "2.13.8"


fork in Compile := true


javaOptions in Compile ++= Seq("-Xmx2G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


libraryDependencies ++= Seq( //todo: make shapeless optional
	"com.chuusai" %% "shapeless" % "2.3.7",
	"org.scala-lang" % "scala-library" % "2.13.8",
	"org.scala-lang" % "scala-reflect" % "2.13.8",
	"org.scalatest" %% "scalatest" % "3.2.11" % "test",
	"org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
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
//	"-Wconf:cat=deprecation&msg=foldLeft:silent,cat=deprecation&msg=foldRight:silent,cat=deprecation:w,cat=feature:w",
	"-Wunused:patvars,privates,locals",
	"-feature",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



