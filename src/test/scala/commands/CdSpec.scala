package commands

import org.scalatest._
import filesystem.State
import files.Directory

class CdSpec extends FlatSpec with Matchers {
  it should "change directory" in {
    val beforeState: State =
      new Mkdir("hello").apply(
        State.apply(Directory.ROOT, Directory.ROOT)
      )
    val nextState: State = new Cd("hello").apply(beforeState)
  }
}
