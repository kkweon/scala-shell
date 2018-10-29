package commands

import filesystem.State
import files.{File, DirEntry}

class Touch(name: String) extends CreateEntry(name) {
  override def doCreateEntry(state: State): DirEntry =
    File.empty(state.wd.path, name)
}
