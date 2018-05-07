package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.exception.{NoClosingParenthesisException, UnexpectedTokenException}
import com.ndsmith3.parakeet.lexer._

import scala.annotation.tailrec

object Parser {
  type IntermediateAST = (AbstractSyntaxTree, List[Token])

  def parse(tokens: List[Token]): AbstractSyntaxTree = expression(tokens)._1

  private def expression(tokens: List[Token]): IntermediateAST = {
    val (beginningNode, currTokens) = term(tokens)

    currTokens match {
      case (AddToken | SubtractToken) :: _ => accumulateExpression(beginningNode, currTokens)
      case _                               => (beginningNode, currTokens)
    }
  }

  @tailrec
  private def accumulateExpression(currNode: AbstractSyntaxTree, tokens: List[Token]): IntermediateAST = {
    lazy val (right, currTokens) = term(tokens.tail)

    tokens match {
      case AddToken :: _      => accumulateExpression(BinaryOperation(currNode, Add, right), currTokens)
      case SubtractToken :: _ => accumulateExpression(BinaryOperation(currNode, Subtract, right), currTokens)
      case _                  => (currNode, tokens)
    }
  }

  private def term(tokens: List[Token]): IntermediateAST = {
    val (beginningNode, currTokens) = pow(tokens)

    currTokens match {
      case (MultiplyToken | DivideToken | ModulusToken) :: _ => accumulateTerm(beginningNode, currTokens)
      case _                                                 => (beginningNode, currTokens)
    }
  }

  @tailrec
  private def accumulateTerm(currNode: AbstractSyntaxTree, tokens: List[Token]): IntermediateAST = {
    lazy val (right, currTokens) = factor(tokens.tail)

    tokens match {
      case MultiplyToken :: _ => accumulateTerm(BinaryOperation(currNode, Multiply, right), currTokens)
      case DivideToken :: _   => accumulateTerm(BinaryOperation(currNode, Divide, right), currTokens)
      case ModulusToken :: _  => accumulateTerm(BinaryOperation(currNode, Modulus, right), currTokens)
      case _                  => (currNode, tokens)
    }
  }

  private def pow(tokens: List[Token]): IntermediateAST = {
    val (beginningNode, currTokens) = factor(tokens)

    currTokens match {
      case PowerToken :: _ => accumulatePow(beginningNode, currTokens)
      case _               => (beginningNode, currTokens)
    }
  }

  @tailrec
  private def accumulatePow(currNode: AbstractSyntaxTree, tokens: List[Token]): IntermediateAST = {
    lazy val (right, currTokens) = factor(tokens.tail)

    tokens match {
      case PowerToken :: _ => accumulatePow(BinaryOperation(currNode, Power, right), currTokens)
      case _               => (currNode, tokens)
    }
  }

  private def factor(tokens: List[Token]): IntermediateAST = tokens match {
    case (int: IntegerToken) :: tail    => (Integer(int.value), tail)
    case (float: FloatToken) :: tail    => (Float(float.value), tail)
    case (str: StringToken) :: tail     => (ASTString(str.value), tail)
    case (char: CharacterToken) :: tail => (Character(char.value), tail)
    case LeftParenthesisToken :: tail   => innerExpression(tail)
    case AssignToken :: tail            => assignStatement(tail)
    case token :: _                     => throw new UnexpectedTokenException(token)
    case Nil                            => throw new Exception("No tokens found")
  }

  private def innerExpression(tokens: List[Token]): IntermediateAST = {
    val (node, currTokens) = expression(tokens)

    if (currTokens.head == RightParenthesisToken) (node, currTokens.tail)
    else throw new NoClosingParenthesisException()
  }

  private def assignStatement(tokens: List[Token]): IntermediateAST = tokens match {
    case ConstantToken(name) :: EqualsToken :: (prim: PrimitiveToken) :: tail => (Assignment(name, prim.value), tail)
    case _                                                                    => throw new UnexpectedTokenException(EqualsToken)
  }
}
