package commands

import filesystem.State
import files.{DirEntry, Directory}
import scala.annotation.tailrec

/**
  * cd /something/..
  * cd a/b/c
  * cd ..
  * cd .
  */
class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    // 1. Find root
    val root = state.root
    val wd = state.wd
    // 2. Find the absolute path to cd
    val absPath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else {
        if (wd.isRoot) wd.path + dir
        else wd.path + Directory.SEPARATOR + dir
      }

    // 3. Find the directory
    val destDir = doFindEntry(root, absPath)
    // 4. Change the state with the new directory
    if (destDir == null || !destDir.isDirectory) {
      state.setMessage(s"$dir: no such directory")
    } else {
      State(root, destDir.asDirectory)
    }
  }

  /**
    * @param root - Current Path
    * @param path - Absolute path "/absolute/path/like/this"
    */
  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory,
                        path: List[String]): DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    @tailrec
    def collapseRelativeTokens(path: List[String],
                               result: List[String]): List[String] =
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    // 1. Tokens
    val tokens: List[String] =
      path.substring(1).split(Directory.SEPARATOR).toList
    // 1.5 Eliminate relative tokens
    val newTokens = collapseRelativeTokens(tokens, List())

    // 2. Navigate to the correct entry
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}
