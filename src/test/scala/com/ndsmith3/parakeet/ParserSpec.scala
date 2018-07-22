package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.exception.{
  ExpectedTokenException,
  ExpectedExpressionException,
  NoClosingParenthesisException,
  UnexpectedTokenException
}
import com.ndsmith3.parakeet.lexer._
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {
  "The Parser" should "return Integer(1) when given IntegerToken(1) :: Nil" in {
    assert(Parser.parse(IntegerToken(1) :: Nil) == CompoundStatement(List(Integer(1))))
  }

  it should "return Float(2.2) when given FloatToken(2.2) :: Nil" in {
    assert(Parser.parse(FloatToken(2.2) :: Nil) == CompoundStatement(List(Float(2.2))))
  }

  it should "return Character('a') when given CharacterToken('a') :: Nil" in {
    assert(Parser.parse(CharacterToken('a') :: Nil) == CompoundStatement(List(Character('a'))))
  }

  it should "return ASTString(\"abcdefg\") when given StringToken(\"abcdefg\") :: Nil" in {
    assert(Parser.parse(StringToken("abcdefg") :: Nil) == CompoundStatement(List(ASTString("abcdefg"))))
  }

  it should "return BinaryOperation(Integer(1), Subtract, Integer(1)) when given IntegerToken(1) :: SubtractToken :: IntegerToken(1) :: Nil" in {
    assert(
      Parser
        .parse(IntegerToken(1) :: SubtractToken :: IntegerToken(1) :: Nil) == CompoundStatement(
        List(BinaryOperation(Integer(1), Subtract, Integer(1))))
    )
  }

  it should "return BinaryOperation(Float(1), Modulus, Float(1)) when given FloatToken(1) :: ModulusToken :: FloatToken(1) :: Nil" in {
    assert(
      Parser
        .parse(FloatToken(1) :: ModulusToken :: FloatToken(1) :: Nil) == CompoundStatement(
        List(BinaryOperation(Float(1), Modulus, Float(1))))
    )
  }

  it should "return BinaryOperation(Integer(2), Power, Integer(2)) when given IntegerToken(2) :: PowerToken :: IntegerToken(2) :: Nil" in {
    assert(
      Parser.parse(IntegerToken(2) :: PowerToken :: IntegerToken(2) :: Nil) == CompoundStatement(
        List(BinaryOperation(Integer(2), Power, Integer(2)))))
  }

  it should "return Assignment(\"abcdefg\", 2) when given AssignToken :: IDToken(\"abcdefg\") :: EqualsToken :: IntegerToken(2) :: Nil" in {
    assert(
      Parser
        .parse(AssignToken :: IDToken("abcdefg") :: EqualsToken :: IntegerToken(2) :: Nil) == CompoundStatement(
        List(Assignment("abcdefg", Integer(2)))))
  }

  it should "throw an UnexpectedTokenException when given a closing parenthesis without an opening parenthesis first" in {
    assertThrows[UnexpectedTokenException] {
      Parser.parse(RightParenthesisToken :: Nil)
    }
  }

  it should "throw an Exception when given Nil" in {
    assertThrows[ExpectedTokenException] {
      Parser.parse(Nil)
    }
  }
  
  it should "return TypeDeclaration(\"foo\", \"String\") when given IDToken(\"foo\") :: ColonToken :: IDToken(\"String\")" in {
    assert(
      Parser.parse(IDToken("foo") :: ColonToken :: IDToken("String") :: Nil) == CompoundStatement(
        List(TypeDeclaration("foo", "String"))))
  }
}
