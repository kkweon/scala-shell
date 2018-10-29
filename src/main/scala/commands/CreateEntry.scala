package commands

import filesystem.State
import files.{DirEntry, Directory}

abstract class CreateEntry(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists")
    } else if (name.contains(Directory.SEPARATOR)) {
      // nested dir is prohibited
      state.setMessage(s"$name must not contain a separator")
    } else if (checkIllegal(name)) {
      state.setMessage(s"$name: illegal entry name")
    } else doCreate(state, name)
  }

  def checkIllegal(name: String): Boolean = name.contains(".")

  def doCreate(state: State, name: String): State = {
    val wd: Directory = state.wd

    // 1. all the directories in the full path
    val allDirsInPath = wd.getAllFoldersInPath
    // 2. create a new directory entry in the wd
    val newEntry = doCreateEntry(state)
    // 3. update the whold directory structure starting from the root
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    // 4. find new working directory instance given wd's full path
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def doCreateEntry(state: State): DirEntry

  def updateStructure(currentDirectory: Directory,
                      path: List[String],
                      newEntry: DirEntry): Directory = {

    if (path.isEmpty) currentDirectory.addEntry(newEntry)
    else {
      val oldEntry = currentDirectory.findEntry(path.head)
      currentDirectory.replaceEntry(
        oldEntry.name,
        updateStructure(oldEntry.asDirectory, path.tail, newEntry))

    }

  }
}
