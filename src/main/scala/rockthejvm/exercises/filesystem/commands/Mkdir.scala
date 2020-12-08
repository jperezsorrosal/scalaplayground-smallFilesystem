package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.{DirEntry, Directory}

class Mkdir(dirName: String) extends CreateEntry(dirName) {
  override def createSpecificEntry(state: State): DirEntry =
    Directory.empty(state.wd.path, dirName)
}
