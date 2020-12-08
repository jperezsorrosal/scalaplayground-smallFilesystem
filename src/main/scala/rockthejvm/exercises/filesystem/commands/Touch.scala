package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.{DirEntry, File}

class Touch(fileName: String) extends CreateEntry(fileName) {
  override def createSpecificEntry(state: State): DirEntry = File.empty(state.wd.path , fileName)
}
