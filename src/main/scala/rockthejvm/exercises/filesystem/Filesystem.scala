package rockthejvm.exercises.filesystem

import rockthejvm.exercises.filesystem.commands.Command
import rockthejvm.exercises.filesystem.files.Directory

import java.util.Scanner
import scala.annotation.tailrec
import scala.io.StdIn

object Filesystem extends App {


  val scanner = new Scanner(System.in)

  val root = Directory.ROOT
  val initState = State(root, root)


  @tailrec
  def mainLoop(state: State): State =  {
    state.show

    val input = StdIn.readLine

    val command = Command.from(input)
    val newState = command(state)

    mainLoop(newState)
  }

  mainLoop(initState)
}
