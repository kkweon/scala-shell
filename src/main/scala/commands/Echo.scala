package commands

import filesystem.State
import files.{File, Directory}
import scala.annotation.tailrec

class Echo(args: List[String]) extends Command {

  /** if args is empty, state
    * if args is single, print
    * if >, echo to a file
    * if >>, append to a file
    * else console
    */
  override def apply(state: State): State =
    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args.head)
    else {
      val filename = args(args.length - 1)
      val operator = args(args.length - 2)
      val content = createContent(args, args.length - 2)

      if (">>".equals(operator))
        doEcho(state, content, filename, append = true)
      else if (">".equals(operator))
        doEcho(state, content, filename, append = false)
      else state.setMessage(createContent(args, args.length))
    }

  /** @param topIndex - not inclusive index
    */
  def createContent(args: List[String], topIndex: Int): String = {
    args.slice(0, topIndex).mkString(" ")
  }

  def getRootAfterEcho(currentDirectory: Directory,
                       path: List[String],
                       content: String,
                       append: Boolean): Directory = {
    if (path.isEmpty) currentDirectory
    else if (path.tail.isEmpty) {
      val dirEntry = currentDirectory.findEntry(path.head)
      if (dirEntry == null)
        currentDirectory.addEntry(
          new File(currentDirectory.path, path.head, content))
      else if (dirEntry.isDirectory) currentDirectory
      else if (append)
        currentDirectory.replaceEntry(path.head,
                                      dirEntry.asFile.appendContent(content))
      else
        currentDirectory.replaceEntry(path.head,
                                      dirEntry.asFile.setContent(content))
    } else {
      // always path.head is directory
      val nextDirectory = currentDirectory.findEntry(path.head).asDirectory
      val newNextDirectory =
        getRootAfterEcho(nextDirectory, path.tail, content, append)
      if (newNextDirectory == nextDirectory) currentDirectory
      else currentDirectory.replaceEntry(path.head, newNextDirectory)
    }
  }

  def doEcho(state: State,
             content: String,
             filename: String,
             append: Boolean): State =
    if (filename.contains(Directory.SEPARATOR))
      state.setMessage(s"$filename should not contain ${Directory.SEPARATOR}")
    else {
      val newRoot: Directory = getRootAfterEcho(
        state.root,
        state.wd.getAllFoldersInPath :+ filename,
        content,
        append)
      if (newRoot == state.root) state.setMessage(s"$filename: no such file")
      else State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
}
