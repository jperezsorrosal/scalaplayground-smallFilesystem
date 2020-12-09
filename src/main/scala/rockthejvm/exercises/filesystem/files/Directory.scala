package rockthejvm.exercises.filesystem.files

import rockthejvm.exercises.filesystem.files.EntryType.EntryType
import rockthejvm.exercises.filesystem.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry]) extends DirEntry(parentPath, name) {

  def isRoot: Boolean = parentPath.isEmpty

  override def isDirectory: Boolean = true

  override def isFile: Boolean = false

  def hasEntry(dirName: String): Boolean = !findEntry(dirName).isEmpty

  def getAllFoldersInPath: List[String] =
    path.substring(1).split(Directory.SEPARATOR).toList.filter(!_.isEmpty)


  def findDescendant(path: List[String]): Option[Directory] = {
    if (path.isEmpty) Some(this)
    else findEntry(path.head).flatMap(_.asDirectory.findDescendant(path.tail))
  }

  def addEntry(newEntry: DirEntry): Directory = new Directory(parentPath, name, contents :+ newEntry)

  def findEntry(entryName: String): Option[DirEntry] = {

    @tailrec
    def findEntryLoop(name: String, contentList: List[DirEntry]): Option[DirEntry] = {
      if (contentList.isEmpty) None
      else if (contentList.head.name.equals(name)) Some(contentList.head)
      else findEntryLoop(name, contentList.tail)
    }

    findEntryLoop(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: Directory): Directory =
    new Directory(parentPath, name, contents.filter(!_.name.equals(entryName)) :+ newEntry)

  override def asDirectory: Directory = this

  override def asFile: File = throw new FileSystemException("A directory cannot be converted to a file")

  override def getType: EntryType = EntryType.Directory
}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = empty("", "")

  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List.empty)
}