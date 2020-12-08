package rockthejvm.exercises.filesystem.files

import rockthejvm.exercises.filesystem.files.EntryType.EntryType

abstract class DirEntry(val parentPath: String, val name: String) {

  def path: String = s"${parentPath}${Directory.SEPARATOR}${name}"

  def asDirectory: Directory
  def asFile: File

  def getType: EntryType
}

object EntryType extends Enumeration {
  type EntryType = Value

  val Directory, File = Value

}