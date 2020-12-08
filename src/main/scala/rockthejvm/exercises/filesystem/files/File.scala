package rockthejvm.exercises.filesystem.files

import rockthejvm.exercises.filesystem.files.EntryType.EntryType
import rockthejvm.exercises.filesystem.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String) extends DirEntry(parentPath, name) {
  override def asDirectory: Directory = throw new FileSystemException("A file cannot converted to directory")

  override def asFile: File = this

  override def getType: EntryType = EntryType.File
}

object File {
  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}