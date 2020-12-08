package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.DirEntry

import scala.annotation.tailrec

class LS extends Command {

  override def apply(state: State): State = {
    val contents = state.wd.contents
    val niceOutput = createNiceOutput(contents)

    state.setMessage(niceOutput)
  }

  def createNiceOutput(contents: List[DirEntry]): String = {

    @tailrec
    def loop(contents: List[DirEntry], acc: String): String = {
      if (contents.isEmpty) acc
      else {
        val entryOutput = s"\n<${contents.head.getType}>\t${contents.head.name}"
        loop(contents.tail, acc + entryOutput)
      }
    }

    loop(contents, "")
  }
}
