package rockthejvm.exercises.filesystem

import rockthejvm.exercises.filesystem.State.SHELL_TOKEN
import rockthejvm.exercises.filesystem.files.Directory

class State(val root: Directory, val wd: Directory, val output: String) {

  def show: Unit = {
    if (!output.isEmpty)  println(output)
    print(SHELL_TOKEN)
  }

  def setMessage(msg: String): State = State(root, wd, msg)
}

object State {

  val SHELL_TOKEN = "$ "

  def apply(root: Directory, wd: Directory, output: String = "") = new State(root, wd, output)
}
