package com.ndsmith3.parakeet.tokenizer

import scala.annotation.tailrec

object Tokenizer {
  def tokenize(input: String): (Option[Token], String) = input.head match {
    case ' '                  => (None, input.tail)
    case '+'                  => (Some(Add), input.tail)
    case '-'                  => (Some(Subtract), input.tail)
    case '*'                  => (Some(Multiply), input.tail)
    case '/'                  => (Some(Divide), input.tail)
    case char if char.isDigit => parseInteger(input)
  }

  private def parseInteger(str: String): (Option[IntegerToken], String) = {
    @tailrec
    def scan(currStr: String, currIntString: String = ""): (Option[IntegerToken], String) =
      if (currStr.isEmpty || !currStr.head.isDigit) (Some(IntegerToken(currIntString.toInt)), currStr)
      else scan(currStr.tail, currIntString + currStr.head)

    scan(str)
  }
}
