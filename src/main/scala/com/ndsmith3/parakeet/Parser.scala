package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, BinaryOperation, Constant}
import com.ndsmith3.parakeet.lexer._

import scala.annotation.tailrec

object Parser {
  type IntermediateAST = (AbstractSyntaxTree, List[Token])

  def parse(tokens: List[Token]): AbstractSyntaxTree = expression(tokens)._1

  private def factor(tokens: List[Token]): IntermediateAST = tokens match {
    case (int: IntegerToken) :: tail => (Constant(int), tail)
    case LeftParenthesis :: tail     => innerExpression(tail)
    case _                           => throw new Exception("Invalid Character")
  }

  private def innerExpression(tokens: List[Token]): IntermediateAST = {
    val (node, currTokens) = expression(tokens)
    (node, currTokens.tail)
  }

  private def term(tokens: List[Token]): IntermediateAST = {
    val (beginningNode, currTokens) = factor(tokens)

    currTokens match {
      case (Multiply | Divide) :: _ => accumulateTerm(beginningNode, currTokens)
      case _                        => (beginningNode, currTokens)
    }
  }

  @tailrec
  private def accumulateTerm(currNode: AbstractSyntaxTree, tokens: List[Token]): IntermediateAST = {
    lazy val (right, currTokens) = factor(tokens.tail)

    tokens match {
      case Multiply :: _ =>
        accumulateTerm(BinaryOperation(currNode, Multiply, right), currTokens)
      case Divide :: _ =>
        accumulateTerm(BinaryOperation(currNode, Divide, right), currTokens)
      case _ => (currNode, tokens)
    }
  }

  private def expression(tokens: List[Token]): IntermediateAST = {
    val (beginningNode, currTokens) = term(tokens)

    currTokens match {
      case (Add | Subtract) :: _ => accumulateExpression(beginningNode, currTokens)
      case _                     => (beginningNode, currTokens)
    }
  }

  @tailrec
  private def accumulateExpression(currNode: AbstractSyntaxTree, tokens: List[Token]): IntermediateAST = {
    lazy val (right, currTokens) = term(tokens.tail)

    tokens match {
      case Add :: _ =>
        accumulateExpression(BinaryOperation(currNode, Add, right), currTokens)
      case Subtract :: _ =>
        accumulateExpression(BinaryOperation(currNode, Subtract, right), currTokens)
      case _ => (currNode, tokens)
    }
  }
}
