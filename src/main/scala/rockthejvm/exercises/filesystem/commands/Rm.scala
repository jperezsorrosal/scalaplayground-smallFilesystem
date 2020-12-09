package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.Directory

class Rm(name: String) extends Command {


  override def apply(state: State): State = {
    // 1. get working dir
    val wd = state.wd

    // 2. get absolute path
    val absolutePath = {
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name
    }

    // 3. do checks
    // absolute path not slash
    if (absolutePath.equals(Directory.ROOT_PATH)) state.setMessage("Cannot remove root directory")
    else
      // 4. find the entry to remove
      // 5. update structure
      doRM(state, absolutePath)
  }


  def doRM(state: State, path: String): State = {

    def rmHelper(currentDirectory: Directory, path: List[String]): Option[Directory] = path match {
      case List() => Option(currentDirectory)
      case  h :: Nil => currentDirectory.removeEntry(path.head)
      case h :: t =>
        for {
          nextDirectory <- currentDirectory.findEntry(h) if nextDirectory.isDirectory
          newNextDirectory <- rmHelper(nextDirectory.asDirectory, t)
        } yield currentDirectory.replaceEntry(h, newNextDirectory)
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot = rmHelper(state.root, tokens)

    val newState = for {
      root <- newRoot
      descendant <- root.findDescendant(state.wd.path.substring(1))
    } yield State(root, descendant)

    newState.getOrElse(
        state.setMessage(s"$path: no such file or directory")
    )


    // 4. find the entry to remove
    // 5. update structure
  }
}
