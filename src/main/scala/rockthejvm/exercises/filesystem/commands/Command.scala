package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State

trait Command extends (State => State)

object Command {
  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"
  val RM = "rm"
  val QUIT = "quit"
  val ECHO = "echo"
  val CAT = "cat"

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

      case Array(c @ TOUCH) => incompleteCommand(c)
      case Array(TOUCH, dirName) => new Touch(dirName)
      case Array(c @ TOUCH, _, _*) => incompleteCommand(c)

      case Array(c @ CD) => incompleteCommand(c)
      case Array(CD, dirName) => new CD(dirName)
      case Array(c @ CD, _, _*) => incompleteCommand(c)

      case Array(c @ RM) => incompleteCommand(c)
      case Array(RM, dirName) => new Rm(dirName)
      case Array(c @ RM, _, _*) => incompleteCommand(c)

      case Array(c @ ECHO) => incompleteCommand(c)
      case Array(ECHO, args @  _*) => new Echo(args:_*)

      case Array(c @ CAT) => incompleteCommand(c)
      case Array(CAT, filename) => new Cat(filename)
      case Array(c @ CAT, _, _*) => incompleteCommand(c)



      case Array(QUIT) => new Quit

      case _ => new Unknown
    }
  }
}
