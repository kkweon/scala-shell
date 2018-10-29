package commands

import org.scalatest._
import filesystem.State
import files.Directory

class TouchSpec extends FlatSpec with Matchers {
  it should "create a new file" in {
    val state: State = State.apply(Directory.ROOT, Directory.ROOT)
    val command = new Touch("hello")
    val newState: State = command.apply(state)

    newState.wd.contents.exists(c => c.name === "hello" && c.getType === "File")
  }
}
