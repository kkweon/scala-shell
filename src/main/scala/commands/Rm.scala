package commands

import files.Directory
import filesystem.State

class Rm(target: String) extends Command {
  override def apply(state: State): State = {
    // 1. get a working directory
    val wd = state.wd
    // 2. get an absolute path
    val absPath = getAbsPath(wd, target)
    // 3. Do some checks
    if (Directory.ROOT_PATH.equals(absPath))
      state.setMessage("Nuclear war is not supported yet")
    else doRm(state, absPath)
  }

  def getAbsPath(wd: Directory, name: String): String =
    if (name.substring(1).equals(Directory.SEPARATOR)) name
    else if (wd.isRoot) wd.path + name
    else wd.path + Directory.SEPARATOR + name

  def doRm(state: State, path: String): State = {
    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (!nextDir.isDirectory) currentDirectory
        else {
          val newNextDirectory = rmHelper(nextDir.asDirectory, path.tail)
          if (newNextDirectory == nextDir) currentDirectory
          else currentDirectory.replaceEntry(path.head, newNextDirectory)
        }
      }
    }
    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)
    if (newRoot == state.root)
      state.setMessage(s"$path: no such file or directory")
    else State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))

  }

}
