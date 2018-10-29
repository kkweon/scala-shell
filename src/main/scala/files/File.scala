package files

import filesystem.FileSystemException
class File(override val parentPath: String,
           override val name: String,
           val contents: String)
    extends DirEntry(parentPath, name) {
  override def getType: String = "File"
  override def asDirectory: Directory =
    throw new FileSystemException(s"$name is not a directory")
  override def asFile: File = this

  override def isFile: Boolean = true
  override def isDirectory: Boolean = false

  def setContent(content: String): File =
    new File(parentPath, name, content)
  def appendContent(content: String): File =
    setContent(s"$contents\n$content")

}

object File {
  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}
