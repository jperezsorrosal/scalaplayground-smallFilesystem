package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State

class Quit extends Command {

  override def apply(state: State): State = state.setMessage(Command.QUIT)
}
