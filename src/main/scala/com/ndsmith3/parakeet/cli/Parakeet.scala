package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter
import scala.io.StdIn.readLine

object Parakeet extends App {
  while (true) {
    readLine("parakeet> ") match {
      case "exit" => sys.exit(0)
      case exp    => println(Interpreter.interpret(exp))
    }
  }
}
