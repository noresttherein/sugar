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
	"org.scalatest" %% "scalatest" % "3.2.9" % "test",
	"org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
)


scalacOptions ++= Seq(
	"-Vimplicits", "-Vimplicits-verbose-tree",
	"-Wconf:cat=deprecation&msg=foldLeft instead|foldRight instead:silent,cat=deprecation:w,cat=feature:w",
	"-Xlint:delayedinit-select,implicit-not-found,option-implicit,poly-implicit-overload,missing-interpolator,nullary-unit",
//	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



