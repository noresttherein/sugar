organization := "net.noresttherein"

name := "sugar"

version := "moonshine"

scalaVersion := "2.13.10"


Compile / fork := true

Compile / javaOptions ++= Seq("-Xmx4G")


Test / testOptions ++= Seq(Tests.Filter {
	s => !(Class.forName(s).isInterface || java.lang.reflect.Modifier.isAbstract(Class.forName(s).getModifiers))
})


libraryDependencies ++= Seq( //todo: make shapeless optional
	"org.scala-lang" % "scala-compiler" % "2.13.10",
	"org.scala-lang" % "scala-library" % "2.13.10",
	"org.scala-lang" % "scala-reflect" % "2.13.10",
	"com.chuusai" %% "shapeless" % "2.3.10",
	"net.bytebuddy" % "byte-buddy" % "1.12.21",

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



