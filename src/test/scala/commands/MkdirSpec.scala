package commands

import org.scalatest._
import filesystem.State
import files.Directory

class MkdirSpec extends FlatSpec with Matchers {
  it should "create a new directory" in {
    val state: State = State.apply(Directory.ROOT, Directory.ROOT)
    val mkdir = new Mkdir("name")
    val newState: State = mkdir.apply(state)

    newState.wd.contents.exists(_.name == "name")
  }
}
