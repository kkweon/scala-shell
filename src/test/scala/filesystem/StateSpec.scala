package filesystem

import org.scalatest._

class StateSpec extends FlatSpec with Matchers {
  "State.SHELL_TOKEN" should "exists" in {
    State.SHELL_TOKEN === "$ "
  }
}
