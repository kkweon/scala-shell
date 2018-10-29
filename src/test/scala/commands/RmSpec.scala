package commands

import org.scalatest._
import filesystem.State
import files.Directory

class RmSpec extends FlatSpec with Matchers {
  it should "remove a file" in {
    val prevState: State =
      new Touch("newFile").apply(State(Directory.ROOT, Directory.ROOT))
    val nextState: State = new Rm("newFile").apply(prevState)
    nextState.wd.findEntry("newFile") !== null
  }
}
