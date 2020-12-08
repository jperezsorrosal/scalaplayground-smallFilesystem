package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State

class Unknown extends Command {
  override def apply(state: State): State = state.setMessage("Unknown command")
}
