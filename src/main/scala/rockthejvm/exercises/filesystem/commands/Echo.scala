package rockthejvm.exercises.filesystem.commands

import rockthejvm.exercises.filesystem.State
import rockthejvm.exercises.filesystem.commands.Echo.{ADD_OPERATOR, APPEND_OPERATOR}
import rockthejvm.exercises.filesystem.files.{Directory, File}

import scala.io.StdIn

class Echo(args: String*) extends Command {

  override def apply(state: State): State = args match {
    case Seq() => state
    case Seq(h) => state.setMessage(h)

    case init :+ operator :+ filename if operator == ADD_OPERATOR =>
      doEcho(state, createContents(init), filename, appendMode = false)

    case init :+ operator :+ filename if operator == APPEND_OPERATOR =>
      doEcho(state, createContents(init), filename, appendMode = true)

    case _ => state

  }

  def createContents(content: Seq[String]): String = content.mkString(" ")

  def getRootAfterEcho(currentDirectory: Directory, path: List[String], contents: String, append: Boolean): Option[Directory] = path match {
    case Nil => None
    case List(entryName) =>

      val dirEntry = currentDirectory.findEntry(entryName)

      if (dirEntry.isEmpty) Some(currentDirectory.addEntry(new File(currentDirectory.path, entryName, contents)))
      else if (dirEntry.get.isDirectory) None
      else
        if (append) Some(currentDirectory.replaceEntry(entryName, dirEntry.get.asFile.appendContents(contents)).asDirectory)
        else Some(currentDirectory.replaceEntry(entryName, dirEntry.get.asFile.setContents(contents)).asDirectory)

    case entryName :: pathTail =>
      {
        for {
          nextDirectory <- currentDirectory.findEntry(entryName) if nextDirectory.isDirectory
          newNextDirectory <- getRootAfterEcho(nextDirectory.asDirectory, pathTail, contents, append)
          root <- Some(currentDirectory.replaceEntry(entryName, newNextDirectory))
        } yield root
      }.map(_.asDirectory)

  }

  def doEcho(state: State, content: String, filename: String, appendMode: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR))
      state.setMessage(s"Echo: filename must not contain separators ${Directory.SEPARATOR}")
    else {
      {
        for {
          newRoot <- getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, content, appendMode)
          wd <- newRoot.findDescendant(state.wd.getAllFoldersInPath)
        } yield State(newRoot, wd)
      }.getOrElse(
        state.setMessage(s"Echo: could not write to file: $filename")
      )

    }
  }

}

object Echo {
  val ADD_OPERATOR = ">"
  val APPEND_OPERATOR = ">>"
}
