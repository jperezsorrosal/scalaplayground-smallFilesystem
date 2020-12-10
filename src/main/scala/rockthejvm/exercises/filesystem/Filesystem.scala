package rockthejvm.exercises.filesystem

import rockthejvm.exercises.filesystem.commands.Command
import rockthejvm.exercises.filesystem.files.Directory

import java.util.Scanner
import scala.annotation.tailrec
import scala.io.Source.stdin
import scala.io.StdIn

object Filesystem extends App {

  val root = Directory.ROOT
  val initState = State(root, root)

  initState.show

  stdin.getLines().foldLeft(initState){ (currentState, line) =>

    val newState = Command.from(line)(currentState)
    newState.show

    newState
  }

//  @tailrec
//  def mainLoop(state: State): State =  {
//    state.show
//
//    if (state.output.equals(Command.QUIT)) return initState // return so quit
//
//    val input = StdIn.readLine
//
//    val command = Command.from(input)
//    val newState = command(state)
//
//    mainLoop(newState)
//  }
//
//  mainLoop(initState)
}
