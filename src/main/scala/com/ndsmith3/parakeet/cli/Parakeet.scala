package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.ast.AbstractSyntaxTree
import com.ndsmith3.parakeet.Parser
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

object Parakeet extends App {
  val input: String           = "1 + 1 - 2 * 4 / 2"
  val tokens: List[Token]     = Lexer.tokenize(input)
  val AST: AbstractSyntaxTree = Parser.parse(tokens)
  println(AST)
}
