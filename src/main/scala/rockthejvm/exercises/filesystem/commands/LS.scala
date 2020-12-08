package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.files.{DirEntry, EntryType}

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
        val separator = if (contents.head.getType == EntryType.File) "\t\t" else "\t"
        val entryOutput = s"\n<${contents.head.getType}>$separator${contents.head.name}"
        loop(contents.tail, acc + entryOutput)
      }
    }

    loop(contents, "")
  }
}
