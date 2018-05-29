package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.exception.{
  ExpectedExpressionException,
  NoClosingParenthesisException,
  UnexpectedTokenException
}
import com.ndsmith3.parakeet.lexer._

import scala.annotation.tailrec

object Parser {
  type IntermediateAST     = (AbstractSyntaxTree, List[Token])
  type AbstractSyntaxTrees = List[AbstractSyntaxTree]

  def parse(tokens: List[Token]): AbstractSyntaxTree                      = compoundExpression(tokens)
  private def compoundExpression(tokens: List[Token]): AbstractSyntaxTree = CompoundStatement(statementList(tokens))

  private def statementList(tokens: List[Token]): AbstractSyntaxTrees = {
    def accumulateStatements(currTokens: List[Token], statements: AbstractSyntaxTrees): AbstractSyntaxTrees = {
      val (newStatement, nextTokens): IntermediateAST = statement(currTokens)
      val nextStatements: AbstractSyntaxTrees         = statements :+ newStatement
      nextTokens match {
        case (SemicolonToken :: Nil) | Nil => nextStatements
        case SemicolonToken :: tail        => accumulateStatements(tail, nextStatements)
      }
    }

    accumulateStatements(tokens, Nil)
  }

  private def statement(tokens: List[Token]): (AbstractSyntaxTree, List[Token]) = tokens match {
    case _ :: (_: BinaryOperationToken) :: _ => expression(tokens)
    case AssignToken :: tail                 => assignStatement(tail)
    case ConstantToken(name) :: tail         => (ID(name), tail)
    case unexpectedToken :: _                => throw new UnexpectedTokenException(unexpectedToken)
  }

  private def assignStatement(tokens: List[Token]): IntermediateAST = tokens match {
    case ConstantToken(name) :: EqualsToken :: tail =>
      val (assignmentValue, remainingTokens) = expression(tail)

      assignmentValue match {
        case primitive: Primitive   => (Assignment(name, primitive), remainingTokens)
        case binOp: BinaryOperation => (Assignment(name, binOp), remainingTokens)
        case _                      => throw new ExpectedExpressionException()
      }

    case _ => throw new UnexpectedTokenException(EqualsToken)
  }

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
    case ConstantToken(name) :: tail    => (ID(name), tail)
    case LeftParenthesisToken :: tail   => innerExpression(tail)
    case AssignToken :: tail            => assignStatement(tail)
    case unexpectedToken :: _           => throw new UnexpectedTokenException(unexpectedToken)
    case Nil                            => throw new Exception("No tokens found")
  }

  private def innerExpression(tokens: List[Token]): IntermediateAST = {
    val (node, currTokens) = expression(tokens)

    if (currTokens.head == RightParenthesisToken) (node, currTokens.tail)
    else throw new NoClosingParenthesisException()
  }
}
