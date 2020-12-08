package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State

class PWD extends Command {
  override def apply(state: State): State = {
    state.setMessage(state.wd.path)
  }
}
