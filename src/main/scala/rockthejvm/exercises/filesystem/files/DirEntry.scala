package rockthejvm.exercises.filesystem.files

import rockthejvm.exercises.filesystem.files.EntryType.EntryType

abstract class DirEntry(val parentPath: String, val name: String) {

  def path: String = {

    val separtorIfNecessary =
      if (parentPath.equals(Directory.ROOT_PATH)) "" else Directory.SEPARATOR

    s"${parentPath}${separtorIfNecessary}${name}"
  }

  def asDirectory: Directory
  def asFile: File

  def isDirectory: Boolean
  def isFile: Boolean
  def getType: EntryType
}

object EntryType extends Enumeration {
  type EntryType = Value

  val Directory, File = Value

}