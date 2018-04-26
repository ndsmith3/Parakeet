package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Lexer

object Parakeet extends App {
  val input: String = "1 + 1 - 2 * 4 / 1"
  println(Lexer.read(input))
}
