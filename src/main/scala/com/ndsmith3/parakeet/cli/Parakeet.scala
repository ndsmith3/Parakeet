package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter
import scala.io.StdIn.readLine

object Parakeet extends App {
  while (true) {
    val input = readLine("parakeet> ")
    Interpreter.interpret(input)
  }
}
