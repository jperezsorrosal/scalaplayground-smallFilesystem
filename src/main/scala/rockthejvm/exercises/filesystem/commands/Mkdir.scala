package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.{DirEntry, Directory}

class Mkdir(dirName: String) extends Command {

  def checkIllegal(dirName: String): Boolean = dirName.contains(".")


  def doMkdir(state: State, dirName: String): State = {

    def updateStructure(currentDirectory: Directory, path: List[String], newDirectory: DirEntry): Option[Directory] = {
      if (path.isEmpty) Some(currentDirectory.addEntry(newDirectory))
      else {
        val oldEntry = currentDirectory.findEntry(path.head).map(_.asDirectory) // TODO: could old Entry be a file??

        for {
          old <- oldEntry
          updatedEntry <- updateStructure(old, path.tail, newDirectory)
        } yield currentDirectory.replaceEntry(old.name, updatedEntry)
      }
    }

    val wd = state.wd

    val allDirsInPath = wd.getAllFoldersInPath
    val newDirectory = Directory.empty(wd.path, dirName)
    val newRoot = updateStructure(state.root, allDirsInPath, newDirectory)
    val newWorkingDir = newRoot.flatMap(_.findDescendant(allDirsInPath))

    val newState = for {
      root <- newRoot
      newWD <- newWorkingDir
    } yield State(root, newWD)

    newState.getOrElse(state.setMessage(s"ERROR: Cannot create directory ${dirName}"))
  }

  override def apply(state: State): State = {

    val wd = state.wd

    if (wd.hasEntry(dirName))
      state.setMessage(s"Directory ${dirName}, already exists.")

    else if (dirName.contains(Directory.SEPARATOR))
      state.setMessage(s"${dirName} must not contain separators: ${Directory.SEPARATOR}")

    else if (checkIllegal(dirName))
      state.setMessage(s"${dirName} is an illegal directory name.")

    else doMkdir(state, dirName)
  }
}
