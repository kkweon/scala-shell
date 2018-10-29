package commands

import filesystem.State
import files.{DirEntry, Directory}

class Mkdir(name: String) extends CreateEntry(name) {
  override def doCreateEntry(state: State): DirEntry =
    Directory.empty(state.wd.path, name)
}
