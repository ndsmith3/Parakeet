package com.ndsmith3.parakeet.lexer

import com.ndsmith3.parakeet.exception.UnexpectedCharacterException
import org.scalatest.FlatSpec

class LexerSpec extends FlatSpec {
  "The Lexer" should "return AddToken :: Nil when given \"+\"" in {
    assert(Lexer.tokenize("+") == AddToken :: Nil)
  }

  it should "return SubtractToken :: Nil when given \"-\"" in {
    assert(Lexer.tokenize("-") == SubtractToken :: Nil)
  }

  it should "return MultiplyToken :: Nil when given \"*\"" in {
    assert(Lexer.tokenize("*") == MultiplyToken :: Nil)
  }

  it should "return DivideToken :: Nil when given \"/\"" in {
    assert(Lexer.tokenize("/") == DivideToken :: Nil)
  }

  it should "return ModulusToken :: Nil when given \"%\"" in {
    assert(Lexer.tokenize("%") == ModulusToken :: Nil)
  }

  it should "return PowerToken :: Nil when given \"^\"" in {
    assert(Lexer.tokenize("^") == PowerToken :: Nil)
  }

  it should "return LeftParenthesisToken :: Nil when given \"(\"" in {
    assert(Lexer.tokenize("(") == LeftParenthesisToken :: Nil)
  }

  it should "return RightParenthesisToken :: Nil when given \")\"" in {
    assert(Lexer.tokenize(")") == RightParenthesisToken :: Nil)
  }

  it should "return StringToken(\"abcdefg\") :: Nil when given \"abcdefg\"" in {
    assert(Lexer.tokenize("\"abcdefg\"") == StringToken("abcdefg") :: Nil)
  }

  it should "return IntegerToken(1) :: Nil when given \"1\"" in {
    assert(Lexer.tokenize("1") == IntegerToken(1) :: Nil)
  }

  it should "return FloatToken(2.2) :: Nil when given \"2.2\"" in {
    assert(Lexer.tokenize("2.2") == FloatToken(2.2) :: Nil)
  }

  it should "return FloatToken(0.1) :: Nil when given \".1\"" in {
    assert(Lexer.tokenize(".1") == FloatToken(0.1) :: Nil)
  }

  it should "return EqualsToken :: Nil when given \"=\"" in {
    assert(Lexer.tokenize("=") == EqualsToken :: Nil)
  }

  it should "return AssignToken :: Nil when given \"let\"" in {
    assert(Lexer.tokenize("let") == AssignToken :: Nil)
  }

  it should "return ConstantToken(\"abcdefg\") when given \"abcdefg\"" in {
    assert(Lexer.tokenize("abcdefg") == ConstantToken("abcdefg") :: Nil)
  }

  it should "return Nil when given \"\"" in {
    assert(Lexer.tokenize("") == Nil)
  }

  it should "throw an UnexpectedCharacterException when given an undefined character" in {
    assertThrows[UnexpectedCharacterException] {
      Lexer.tokenize("#")
    }
  }
}
