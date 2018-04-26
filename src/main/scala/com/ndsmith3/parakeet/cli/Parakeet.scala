package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter

object Parakeet extends App {
  val input: String = "1 + 2 * (6 / 2)"
  Interpreter.interpret(input)
}
