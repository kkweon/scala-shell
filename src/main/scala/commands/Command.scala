package commands

import filesystem.State

trait Command extends (State => State)

object Command {
  val MKDIR = "mkdir"
  val EXIT = "exit"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"
  val RM = "rm"
  val ECHO = "echo"
  val CAT = "cat"

  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state
  }

  def incompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State =
      state.setMessage(s"$name incomplete command!")
  }

  def from(input: String): Command = {
    val tokens: Array[String] = input.split(" ")

    if (input.isEmpty || tokens.isEmpty)
      emptyCommand
    else {
      tokens(0) match {
        case EXIT => new Exit
        case LS   => new Ls
        case PWD  => new Pwd
        case x    => checkBeforeCreate(x, tokens)
      }
    }
  }

  def checkBeforeCreate(input: String, tokens: Array[String]): Command =
    if (tokens.length < 2) incompleteCommand(input)
    else {
      val token = tokens(1)
      input match {
        case MKDIR => new Mkdir(token)
        case TOUCH => new Touch(token)
        case CD    => new Cd(token)
        case RM    => new Rm(token)
        case CAT   => new Cat(token)
        case ECHO  => new Echo(tokens.tail.toList)
        case _     => new UnknownCommand
      }
    }
}
