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
    case ')'                  => (Some(RightParenthesis), str.tail)
    case char if char.isDigit => parseNumber(str)
  }

  private def parseNumber(str: String): (Option[NumericToken], String) = {
    @tailrec
    def scan(currStr: String, currIntString: String = ""): (Option[NumericToken], String) =
      if (currStr.isEmpty || !currStr.head.isDigit && currStr.head != '.')
        (Some(IntegerToken(currIntString.toInt)), currStr)
      else if (currStr.head == '.')
        parseFloat(currStr, currIntString)
      else
        scan(currStr.tail, currIntString + currStr.head)

    scan(str)
  }

  private def parseFloat(str: String, floatString: String): (Option[FloatToken], String) = {
    @tailrec
    def scan(currStr: String, currFloatString: String = floatString): (Option[FloatToken], String) =
      if (currStr.isEmpty || !currStr.head.isDigit && currStr.head != '.')
        (Some(FloatToken(currFloatString.toFloat)), currStr)
      else scan(currStr.tail, currFloatString + currStr.head)

    scan(str)
  }
}
