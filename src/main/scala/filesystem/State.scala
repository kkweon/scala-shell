package filesystem

import files.Directory

class State(val root: Directory, val wd: Directory, val output: String) {
  def show(): Unit = {
    if (!output.isEmpty) println(output)
    print(State.SHELL_TOKEN)
  }

  def setMessage(message: String): State =
    State(root, wd, message)

  def exit(exitCode: Int = 0): State =
    new ExitState(root, wd, "", exitCode)
}

case class ExitState(override val root: Directory,
                     override val wd: Directory,
                     override val output: String,
                     val exitCode: Int)
    extends State(root, wd, output)

object State {
  val SHELL_TOKEN = "$ "

  def apply(root: Directory, wd: Directory, output: String = ""): State =
    new State(root, wd, output)
}
