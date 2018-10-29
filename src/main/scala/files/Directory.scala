package files

import scala.annotation.tailrec
import filesystem.FileSystemException

class Directory(override val parentPath: String,
                override val name: String,
                val contents: List[DirEntry])
    extends DirEntry(parentPath, name) {
  def hasEntry(name: String): Boolean =
    findEntry(name) != null

  def addEntry(newDirectory: DirEntry): Directory =
    new Directory(parentPath, name, contents :+ newDirectory)

  def getAllFoldersInPath: List[String] =
    path.substring(1).split(Directory.SEPARATOR).filter(!_.isEmpty).toList

  def findDescendant(path: List[String]): Directory =
    if (path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)

  def findDescendant(relativePath: String): Directory =
    if (relativePath.isEmpty) this
    else findDescendant(relativePath.split(Directory.SEPARATOR).toList)
  def removeEntry(entry: String): Directory =
    if (!hasEntry(entry)) this
    else new Directory(parentPath, name, contents.filterNot(_.name == name))

  def findEntry(entryName: String): DirEntry = {
    @tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry = {
      if (contentList.isEmpty) null
      else if (contentList.head.name.equals(name)) contentList.head
      else findEntryHelper(name, contentList.tail)
    }

    findEntryHelper(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory =
    new Directory(parentPath,
                  name,
                  contents.filter(!_.name.equals(entryName)) :+ newEntry)

  override def asDirectory: Directory = this
  override def getType: String = "Directory"
  override def asFile: File =
    throw new FileSystemException(s"$name is not a file")

  def isRoot: Boolean = parentPath.isEmpty

  override def isDirectory: Boolean = true
  override def isFile: Boolean = false
}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
}
