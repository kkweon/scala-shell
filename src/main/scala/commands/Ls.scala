package commands

import filesystem.State
import files.{DirEntry, Directory}

class Ls extends Command {
  override def apply(state: State): State = {
    val contents = state.wd.contents
    val output = createOutput(contents)
    state.setMessage(output)
  }

  def createOutput(contents: List[DirEntry]): String =
    contents
      .map(c =>
        c.getType match {
          case "Directory" => s"${c.name}${Directory.SEPARATOR}"
          case _           => s"${c.name}"
      })
      .mkString("\n")
}
