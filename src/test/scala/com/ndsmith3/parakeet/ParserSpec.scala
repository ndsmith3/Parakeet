package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.lexer._
import com.ndsmith3.parakeet.exception.{NoClosingParenthesisException, UnexpectedTokenException}
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {
  "The Parser" should "return Integer(1) when given IntegerToken(1) :: Nil" in {
    assert(Parser.parse(IntegerToken(1) :: Nil) == Integer(1))
  }

  it should "return Float(2.2) when given FloatToken(2.2) :: Nil" in {
    assert(Parser.parse(FloatToken(2.2) :: Nil) == Float(2.2))
  }

  it should "return Character('a') when given CharacterToken('a') :: Nil" in {
    assert(Parser.parse(CharacterToken('a') :: Nil) == Character('a'))
  }

  it should "return ASTString(\"abcdefg\") when given StringToken(\"abcdefg\") :: Nil" in {
    assert(Parser.parse(StringToken("abcdefg") :: Nil) == ASTString("abcdefg"))
  }

  it should "return BinaryOperation(Integer(1), Subtract, Integer(1)) when given IntegerToken(1) :: SubtractToken :: IntegerToken(1) :: Nil" in {
    assert(
      Parser
        .parse(IntegerToken(1) :: SubtractToken :: IntegerToken(1) :: Nil) == BinaryOperation(Integer(1),
                                                                                              Subtract,
                                                                                              Integer(1))
    )
  }

  it should "return BinaryOperation(Float(1), Modulus, Float(1)) when given FloatToken(1) :: ModulusToken :: FloatToken(1) :: Nil" in {
    assert(
      Parser
        .parse(FloatToken(1) :: ModulusToken :: FloatToken(1) :: Nil) == BinaryOperation(Float(1), Modulus, Float(1))
    )
  }

  it should "return BinaryOperation(Integer(2), Power, Integer(2)) when given IntegerToken(2) :: PowerToken :: IntegerToken(2) :: Nil" in {
    assert(
      Parser.parse(IntegerToken(2) :: PowerToken :: IntegerToken(2) :: Nil) == BinaryOperation(Integer(2),
                                                                                               Power,
                                                                                               Integer(2)))
  }

  it should "throw an error when there is no closing parenthesis" in {
    assertThrows[NoClosingParenthesisException] {
      Parser.parse(LeftParenthesisToken :: IntegerToken(1) :: IntegerToken(2) :: Nil)
    }
  }

  it should "throw an error when given a closing parenthesis without an opening parenthesis first" in {
    assertThrows[UnexpectedTokenException] {
      Parser.parse(RightParenthesisToken :: Nil)
    }
  }

  it should "throw an error when given Nil" in {
    assertThrows[Exception] {
      Parser.parse(Nil)
    }
  }
}
