package rockthejvm.exercises.filesystem.files

import rockthejvm.exercises.filesystem.files.EntryType.EntryType
import rockthejvm.exercises.filesystem.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String) extends DirEntry(parentPath, name) {

  override def asDirectory: Directory = throw new FileSystemException("A file cannot converted to directory")

  override def asFile: File = this

  override def getType: EntryType = EntryType.File

  override def isDirectory: Boolean = false

  override def isFile: Boolean = true

  def setContents(newContents: String): File = new File(parentPath, name, newContents)
  def appendContents(newContents: String): File = new File(parentPath, name, contents + "\n" + newContents )
}

object File {
  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}