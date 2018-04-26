package com.ndsmith3.parakeet.lexer

import scala.annotation.tailrec

object Lexer {
  def tokenize(input: String): List[Token] = {
    @tailrec
    def tokenizeInput(currString: String, currTokens: List[Option[Token]] = Nil): List[Token] = {
      lazy val (token, newString) = getToken(currString)
      if (currString.isEmpty) currTokens.flatten else tokenizeInput(newString, currTokens :+ token)
    }

    tokenizeInput(input)
  }

  private def getToken(str: String): (Option[Token], String) = str.head match {
    case ' '                  => (None, str.tail)
    case '+'                  => (Some(Add), str.tail)
    case '-'                  => (Some(Subtract), str.tail)
    case '*'                  => (Some(Multiply), str.tail)
    case '/'                  => (Some(Divide), str.tail)
    case '('                  => (Some(LeftParenthesis), str.tail)
    case ')'                  => (Some(LeftParenthesis), str.tail)
    case char if char.isDigit => parseInteger(str)
  }

  private def parseInteger(str: String): (Option[IntegerToken], String) = {
    @tailrec
    def scan(currStr: String, currIntString: String = ""): (Option[IntegerToken], String) =
      if (currStr.isEmpty || !currStr.head.isDigit) (Some(IntegerToken(currIntString.toInt)), currStr)
      else scan(currStr.tail, currIntString + currStr.head)

    scan(str)
  }
}
