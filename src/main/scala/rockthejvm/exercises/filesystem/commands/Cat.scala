package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State

class Cat(filename: String) extends Command {
  override def apply(state: State): State = {

    val wd = state.wd

    {
      for {
        dirEntry <- wd.findEntry(filename) if dirEntry.isFile
      } yield state.setMessage(dirEntry.asFile.contents)
    }
      .getOrElse(state.setMessage(s"No such file: $filename"))
  }
}
