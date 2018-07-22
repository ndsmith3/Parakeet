package com.ndsmith3.parakeet.lexer

import com.ndsmith3.parakeet.exception.{ExpectedCharacterException, UnexpectedCharacterException}

import scala.annotation.tailrec

object Lexer {
  private val unallowedTokens = Set(',', ';', ':', '(', ')', '\n', '/', '+', '-', '*')
  private val assignToken     = "let"

  def tokenize(input: String): List[Token] = {
    @tailrec
    def tokenizeInput(currString: String, currTokens: List[Option[Token]] = None :: Nil): List[Token] = {
      lazy val (token, newString) = getToken(currString, currTokens.last)

      currString match {
        case "" => currTokens.flatten
        case _  => tokenizeInput(newString, currTokens :+ token)
      }
    }

    tokenizeInput(input)
  }

  private def getToken(str: String, lastToken: Option[Token]): (Option[Token], String) =
    str.head match {
      case ' '                             => (None, str.tail)
      case '\n' if isWhiteSpace(lastToken) => (None, str.tail)
      case '+'                             => (Some(AddToken), str.tail)
      case '-'                             => (Some(SubtractToken), str.tail)
      case '*'                             => (Some(MultiplyToken), str.tail)
      case '/'                             => (Some(DivideToken), str.tail)
      case '%'                             => (Some(ModulusToken), str.tail)
      case '^'                             => (Some(PowerToken), str.tail)
      case '('                             => (Some(LeftParenthesisToken), str.tail)
      case ')'                             => (Some(RightParenthesisToken), str.tail)
      case '{'                             => (Some(OpenBraceToken), str.tail)
      case '}'                             => (Some(CloseBraceToken), str.tail)
      case ':'                             => (Some(ColonToken), str.tail)
      case '='                             => (Some(EqualsToken), str.tail)
      case ';'                             => (Some(SemicolonToken), str.tail)
      case '\n'                            => (Some(SemicolonToken), str.tail)
      case ','                             => (Some(CommaToken), str.tail)
      case '"'                             => parseString(str)
      case '.'                             => parseFloat(str)
      case 'l' if isAssignStatement(str)   => (Some(AssignToken), str.substring(3))
      case char if char.isDigit            => parseNumber(str)
      case char if char.isLetter           => parseID(str)
      case char                            => throw new UnexpectedCharacterException(char)
    }

  private def isWhiteSpace(lastToken: Option[Token]): Boolean = lastToken match {
    case Some(SemicolonToken) | None => true
    case _                           => false
  }

  private def isAssignStatement(str: String): Boolean = str.startsWith(assignToken)

  private def parseString(str: String): (Option[StringToken], String) = {
    @tailrec
    def scan(currStr: String, currStringValue: String = ""): (Option[StringToken], String) =
      currStr match {
        case ""                            => throw new ExpectedCharacterException('"')
        case _ if currStr.startsWith("\"") => (Some(StringToken(currStringValue)), currStr.tail)
        case _                             => scan(currStr.tail, currStringValue + currStr.head)
      }

    // Scan on tail to avoid the first quotation mark
    scan(str.tail)
  }

  private def parseFloat(str: String, floatString: String = ""): (Option[FloatToken], String) = {
    @tailrec
    def scan(currStr: String, currFloatString: String = floatString): (Option[FloatToken], String) =
      currStr match {
        case _ if isCompleteNumber(currStr) => (Some(FloatToken(currFloatString.toDouble)), currStr)
        case _                              => scan(currStr.tail, currFloatString + currStr.head)
      }

    scan(str)
  }

  private def parseNumber(str: String): (Option[NumericToken], String) = {
    @tailrec
    def scan(currStr: String, currIntString: String = ""): (Option[NumericToken], String) =
      currStr match {
        case _ if isCompleteNumber(currStr) => (Some(IntegerToken(currIntString.toInt)), currStr)
        case _ if isFloat(currStr)          => parseFloat(currStr, currIntString)
        case _                              => scan(currStr.tail, currIntString + currStr.head)
      }

    scan(str)
  }

  private def isCompleteNumber(str: String): Boolean = str.isEmpty || !str.head.isDigit && str.head != '.'
  private def isFloat(str: String): Boolean          = str.head == '.'

  private def parseID(str: String): (Option[IDToken], String) = {
    @tailrec
    def scan(currStr: String, currNameString: String = ""): (Option[IDToken], String) =
      currStr match {
        case ""                                         => (Some(IDToken(currNameString)), currStr)
        case _ if unallowedTokens contains currStr.head => (Some(IDToken(currNameString)), currStr)
        case _ if currStr.startsWith(" ")               => (Some(IDToken(currNameString)), currStr.tail)
        case _                                          => scan(currStr.tail, currNameString + currStr.head)
      }

    scan(str)
  }
}
