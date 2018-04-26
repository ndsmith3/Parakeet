package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.tokenizer.{Token, Tokenizer}

object Lexer {
  def read(input: String): List[Token] = scan(input)

  private def scan(currInput: String, tokens: List[Option[Token]] = Nil): List[Token] = currInput match {
    case "" => tokens.flatten
    case inp =>
      val (newToken, newInput) = Tokenizer.tokenize(inp)
      scan(newInput, tokens :+ newToken)
  }
}
