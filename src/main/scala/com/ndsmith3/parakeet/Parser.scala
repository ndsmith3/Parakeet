package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, BinaryOperation, Constant}
import com.ndsmith3.parakeet.lexer._

import scala.annotation.tailrec

object Parser {
  def parse(tokens: List[Token]): AbstractSyntaxTree = expression(tokens)._1

  def factor(tokens: List[Token]): (AbstractSyntaxTree, List[Token]) = tokens match {
    case (int: IntegerToken) :: tail => (Constant(int), tail)
    case _                           => throw new Exception("Invalid Character")
  }

  def term(tokens: List[Token]): (AbstractSyntaxTree, List[Token]) = {
    val (beginningNode, currTokens) = factor(tokens)
    currTokens match {
      case (Add | Subtract) :: _ => accumulateTerm(beginningNode, currTokens)
      case _                     => (beginningNode, currTokens)
    }
  }

  @tailrec
  def accumulateTerm(currNode: AbstractSyntaxTree, tokens: List[Token]): (AbstractSyntaxTree, List[Token]) = {
    tokens match {
      case (Add | Subtract) :: _ =>
        val (right, currTokens) = factor(tokens.tail)
        accumulateTerm(BinaryOperation(currNode, tokens.head.asInstanceOf[BinaryOperationToken], right), currTokens)
      case _ => (currNode, tokens)
    }
  }

  def expression(tokens: List[Token]): (AbstractSyntaxTree, List[Token]) = {
    val (beginningNode, currTokens) = term(tokens)
    currTokens match {
      case (Multiply | Divide) :: _ => accumulateExpression(beginningNode, currTokens)
      case _                        => (beginningNode, currTokens)
    }
  }

  @tailrec
  def accumulateExpression(currNode: AbstractSyntaxTree, tokens: List[Token]): (AbstractSyntaxTree, List[Token]) =
    tokens match {
      case (Multiply | Divide) :: _ =>
        val (right, currTokens) = term(tokens.tail)
        accumulateExpression(BinaryOperation(currNode, tokens.head.asInstanceOf[BinaryOperationToken], right),
                             currTokens)
      case _ => (currNode, tokens)
    }
}
