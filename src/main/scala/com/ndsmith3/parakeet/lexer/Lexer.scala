package com.ndsmith3.parakeet.lexer

import com.ndsmith3.parakeet.exception.{ExpectedCharacterException, UnexpectedCharacterException}

import scala.annotation.tailrec

object Lexer {
  def tokenize(input: String): List[Token] = {
    @tailrec
    def tokenizeInput(currString: String, currTokens: List[Option[Token]] = None :: Nil): List[Token] = {
      lazy val (token, newString) = getToken(currString, currTokens.last)
      if (currString.isEmpty) currTokens.flatten
      else tokenizeInput(newString, currTokens :+ token)
    }

    tokenizeInput(input)
  }

  private def getToken(str: String, lastToken: Option[Token]): (Option[Token], String) =
    str.head match {
      case ' '                                                            => (None, str.tail)
      case '+'                                                            => (Some(AddToken), str.tail)
      case '-'                                                            => (Some(SubtractToken), str.tail)
      case '*'                                                            => (Some(MultiplyToken), str.tail)
      case '/'                                                            => (Some(DivideToken), str.tail)
      case '%'                                                            => (Some(ModulusToken), str.tail)
      case '^'                                                            => (Some(PowerToken), str.tail)
      case '('                                                            => (Some(LeftParenthesisToken), str.tail)
      case ')'                                                            => (Some(RightParenthesisToken), str.tail)
      case '{'                                                            => (Some(OpenBraceToken), str.tail)
      case '}'                                                            => (Some(CloseBraceToken), str.tail)
      case ':'                                                            => (Some(ColonToken), str.tail)
      case '='                                                            => (Some(EqualsToken), str.tail)
      case ';'                                                            => (Some(SemicolonToken), str.tail)
      case '\n' if lastToken == Some(SemicolonToken) || lastToken == None => (None, str.tail)
      case '\n'                                                           => (Some(SemicolonToken), str.tail)
      case 'l' if isAssignStatement(str)                                  => (Some(AssignToken), str.substring(3))
      case '"'                                                            => parseString(str)
      case '.'                                                            => parseFloat(str)
      case char if char.isDigit                                           => parseNumber(str)
      case char if char.isLetter                                          => parseID(str)
      case char                                                           => throw new UnexpectedCharacterException(char)
    }

  private def isAssignStatement(str: String): Boolean = str(1) == 'e' && str(2) == 't'

  private def parseString(str: String): (Option[StringToken], String) = {
    def scan(currStr: String, currStringValue: String = ""): (Option[StringToken], String) =
      if (currStr.isEmpty) throw new ExpectedCharacterException('"')
      else
        currStr.head match {
          case '"'  => (Some(StringToken(currStringValue)), currStr.tail)
          case char => scan(currStr.tail, currStringValue + char)
        }

    // Scan on tail to avoid the first quotation mark
    scan(str.tail)
  }

  private def parseNumber(str: String): (Option[NumericToken], String) = {
    @tailrec
    def scan(currStr: String, currIntString: String = ""): (Option[NumericToken], String) =
      if (isCompleteNumber(currStr)) (Some(IntegerToken(currIntString.toInt)), currStr)
      else if (isFloat(currStr)) parseFloat(currStr, currIntString)
      else scan(currStr.tail, currIntString + currStr.head)

    scan(str)
  }

  private def isCompleteNumber(str: String): Boolean = str.isEmpty || !str.head.isDigit && str.head != '.'
  private def isFloat(str: String): Boolean          = str.head == '.'

  private def parseFloat(str: String, floatString: String = ""): (Option[FloatToken], String) = {
    @tailrec
    def scan(currStr: String, currFloatString: String = floatString): (Option[FloatToken], String) =
      if (isCompleteNumber(currStr)) (Some(FloatToken(currFloatString.toDouble)), currStr)
      else scan(currStr.tail, currFloatString + currStr.head)

    scan(str)
  }

  private def parseID(str: String): (Option[IDToken], String) = {
    def scan(currStr: String, currNameString: String = ""): (Option[IDToken], String) =
      if (currStr.isEmpty || (Set(';', ':', '\n') contains currStr.head)) (Some(IDToken(currNameString)), currStr)
      else if (currStr.head == ' ') (Some(IDToken(currNameString)), currStr.tail)
      else scan(currStr.tail, currNameString + currStr.head)

    scan(str)
  }
}
