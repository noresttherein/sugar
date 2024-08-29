package net.noresttherein.sugar.io

import java.io.File

import net.noresttherein.sugar.io.extensions.FileExtension


/** Extension methods for classes in `java.io` and `java.nio`. */
trait extensions extends Any {
	@inline implicit final def FileExtension(file :File) :FileExtension = new FileExtension(file)
}


@SerialVersionUID(Ver)
object extensions {
	class FileExtension(private val dir :File) extends AnyVal {
		@inline def / (name :String) :File = new File(dir, name)
		@inline def member(name :String) :File = new File(dir, name)
	}
}
