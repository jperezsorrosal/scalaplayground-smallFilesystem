package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State

trait Command {

  def apply(state: State): State
}

object Command {
  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"

  def incompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State = {
      val msg = name match {
        case MKDIR =>
          s"${name} is an icomplete command\n" +
          s"Usage: ${name} <dir name>"

        case _ => s"${name} is an icomplete command, unknown command."
      }

      state.setMessage(msg)
    }
  }

  def emptyCommand(): Command = new Command {
    override def apply(state: State): State = state
  }

  def from(input: String): Command = {
    val tokens = input.split(" ").filter(_ != "")

    tokens match {
      case Array() | Array("") => emptyCommand()
      case Array(c @ MKDIR) => incompleteCommand(c)
      case Array(MKDIR, dirName) => new Mkdir(dirName)
      case Array(c @ MKDIR, _, _*) => incompleteCommand(c)

      case Array(c @ LS) => new LS
      case Array(c @ LS, _*) => incompleteCommand(c)

      case Array(PWD) => new PWD
      case Array(c @ PWD, _*) => incompleteCommand(c)

      case _ => new Unknown
    }
  }
}
