package filesystem

import files.Directory
import commands.Command

object Filesystem extends App {
  val root = Directory.ROOT
  var initialState = State(root, root)
  initialState.show()

  io.Source.stdin
    .getLines()
    .foldLeft(initialState)((state, nextLine) => {
      val nextState = Command.from(nextLine).apply(state)

      nextState match {
        case ExitState(_, _, _, exitCode) => {
          System.exit(exitCode)
          state
        }
        case _ => {
          nextState.show()
          nextState.setMessage("")
        }
      }

    })
}
