package commands

import filesystem.State

class Exit extends Command {
  override def apply(state: State): State = state.exit(0)
}
