package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.{DirEntry, Directory}

abstract class CreateEntry(entryName: String) extends Command {

  override def apply(state: State): State = {

    val wd = state.wd

    if (wd.hasEntry(entryName))
      state.setMessage(s"Directory ${entryName}, already exists.")

    else if (entryName.contains(Directory.SEPARATOR))
      state.setMessage(s"${entryName} must not contain separators: ${Directory.SEPARATOR}")

    else if (checkIllegal(entryName))
      state.setMessage(s"${entryName} is an illegal directory name.")

    else doCreateEntry(state, entryName)
  }

  def checkIllegal(entryName: String): Boolean = entryName.contains(".")

  def doCreateEntry(state: State, entryName: String): State = {

    def updateStructure(currentDirectory: Directory, path: List[String], newDirectory: DirEntry): Option[Directory] = {
      if (path.isEmpty) Some(currentDirectory.addEntry(newDirectory))
      else {
        for {
          oldEntry <- currentDirectory.findEntry(path.head)
          updatedEntry <- updateStructure(oldEntry.asDirectory, path.tail, newDirectory)
        } yield currentDirectory.replaceEntry(oldEntry.name, updatedEntry).asDirectory
      }
    }

    val wd = state.wd

    val allDirsInPath = wd.getAllFoldersInPath
    //val newDirectory = Directory.empty(wd.path, entryName)
    val newEntry: DirEntry = createSpecificEntry(state)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    val newWorkingDir = newRoot.flatMap(_.findDescendant(allDirsInPath))

    val newState = for {
      root <- newRoot
      newWD <- newWorkingDir
    } yield State(root, newWD)

    newState.getOrElse(state.setMessage(s"ERROR: Cannot create directory ${entryName}"))
  }

  def createSpecificEntry(state: State): DirEntry
}
