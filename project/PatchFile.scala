import java.io.File

import scala.collection.immutable.Seq
import scala.jdk.CollectionConverters.asScalaBufferConverter

import sbt.Keys.{sourceDirectory, sourceManaged}
import sbt.{Def, IO, Project, Task, ThisProject}
import sbt.librarymanagement.syntax.{Compile, Test}


case class PatchFile(src :File, target :File, patches :Seq[(String, String)]) {
	def replace(pattern :String, replacement :String) :PatchFile =
		PatchFile(src, target, patches :+ (pattern, replacement))

	def replaceAll(replacements :(String, String)*) :PatchFile =
		PatchFile(src, target, patches ++ replacements)

	def patch(pattern :String, replacement :String) :Unit = replace(pattern, replacement).generate()

	def patchAll(replacements :(String, String)*) :Unit = replaceAll(replacements *).generate()

	def generate() :Unit =
		IO.reader(src) { reader =>
			val lines = reader.lines.toList.asScala.map { line =>
				patches.foldLeft(line) { case (line, (pat, str)) => line.replaceAll(pat, str) }
			}
			IO.writeLines(target, lines)
		}

}

object PatchFile {
	def apply(src :File, target :File) :PatchFile = PatchFile(src, target, Vector.empty)

	def apply(src :File => File, target :String)(patch :PatchFile => Unit) :Def.Initialize[Task[Unit]] = Def.task {
		patch(
			PatchFile(
				src(new File((ThisProject / Compile / sourceDirectory).value, "scala")),
				new File(src((ThisProject / Test / sourceManaged).value).getParentFile, target)
			)
		)
	}
}
