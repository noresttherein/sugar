organization := "net.noresttherein"

name := "slang"

version := "moonshine"

scalaVersion := "2.13.3"

fork in Compile := true

javaOptions in Compile ++= Seq("-Xmx2G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


libraryDependencies ++= Seq(
	"com.chuusai" %% "shapeless" % "2.3.3",
	"org.scala-lang" % "scala-library" % "2.13.3",
	"org.scala-lang" % "scala-reflect" % "2.13.3",
	"org.scalatest" %% "scalatest" % "3.1.1" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.3" % "test"
)


scalacOptions ++= Seq(
//	"-Ylog-classpath",
//	"-Xlog-implicits",
	"-Wconf:cat=deprecation&msg=foldLeft instead|foldRight instead:silent,cat=deprecation:w,cat=feature:w",
//	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



