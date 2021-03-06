package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.{DirEntry, Directory, EntryType}

import scala.annotation.tailrec

class CD(dirName: String) extends Command {


  override def apply(state: State): State = {
    /*
      support:
        cd /some/absolute/path
        cd relative/to/current/working/directory
     */

    // find root
    val root = state.root
    val wd = state.wd

    // find absolute path of the directory I want to CD to
    val absolutePath =
      if (dirName.startsWith(Directory.SEPARATOR)) dirName
      else if (wd.isRoot) s"${wd.path}$dirName"
      else s"${wd.path}${Directory.SEPARATOR}$dirName"

    // find the directory to CD to, given the path
    val destinationDirectory = doFindEntry(root, absolutePath)

    // change the state given the new directory
      val newState = for {
        destination <- destinationDirectory if destination.isDirectory
      } yield State(root, destination.asDirectory)

    newState.getOrElse(state.setMessage(s"No such directory: $dirName"))
  }


  def doFindEntry(root: Directory, path: String): Option[DirEntry] = {

    @tailrec
    def findEntryHelper(currentDirectory: Directory, pathTokens: List[String]): Option[DirEntry] = {
      if (pathTokens.isEmpty || pathTokens.head.isEmpty) Some(currentDirectory)
      else if (pathTokens.tail.isEmpty) currentDirectory.findEntry(pathTokens.head)
      else {
        val nextDirr = currentDirectory.findEntry(pathTokens.head).filter(_.isDirectory)

        if (nextDirr.isEmpty) None
        else
          findEntryHelper(nextDirr.get.asDirectory, pathTokens.tail)

      }
    }

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]) : Option[List[String]] = path match {
      case List() =>  Some(result)
      case "." :: t => collapseRelativeTokens(t, result)
      case ".." :: t if result.isEmpty => None
      case ".." :: t  => collapseRelativeTokens(t, result.init) // init = all elements of list but last
      case h::t => collapseRelativeTokens(t, result :+ h)
    }

    // is an absolute path
    assert(path.startsWith(Directory.SEPARATOR))

    // tokens
    val tokens = path.substring(1).split(Directory.SEPARATOR).toList


    /*
      support relative paths . .. ./../..
      need to eliminate/collapse relative tokens

      /a/.  => [a, .] => [a]
      /a/./. => [a, ., .] => [a]

      /a/.. => [a, ..] => []
      /a/b/.. => [a, b, ..] => [a]
     */

    val newTokens = collapseRelativeTokens(tokens, List.empty)

    // navigate to correct entry
    for {
      tokens <- newTokens
      entry <- findEntryHelper(root, tokens)
    } yield entry
  }
}
