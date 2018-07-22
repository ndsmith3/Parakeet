package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.exception._
import com.ndsmith3.parakeet.lexer._

import scala.annotation.tailrec

object Parser {
  type IntermediateAST     = (AbstractSyntaxTree, List[Token])
  type AbstractSyntaxTrees = List[AbstractSyntaxTree]

  def parse(tokens: List[Token]): CompoundStatement                      = compoundExpression(tokens)
  private def compoundExpression(tokens: List[Token]): CompoundStatement = CompoundStatement(statementList(tokens))

  private def statementList(tokens: List[Token]): AbstractSyntaxTrees = {
    def accumulateStatements(currTokens: List[Token], statements: AbstractSyntaxTrees): AbstractSyntaxTrees = {
      val (newStatement, nextTokens): IntermediateAST = statement(currTokens)
      val nextStatements: AbstractSyntaxTrees         = statements :+ newStatement

      nextTokens match {
        case (SemicolonToken :: Nil) | Nil => nextStatements
        case SemicolonToken :: tail        => accumulateStatements(tail, nextStatements)
        case _                             => throw new ExpectedTokenException(SemicolonToken)
      }
    }

    accumulateStatements(tokens, Nil)
  }

  private def statement(tokens: List[Token]): (AbstractSyntaxTree, List[Token]) = tokens match {
    case AssignToken :: tail                           => assignStatement(tail)
    case IDToken(name) :: ColonToken :: tail           => typeDeclarationStatement(name, tail)
    case IDToken(name) :: LeftParenthesisToken :: tail => functionCall(name, tail)
    case IDToken(name) :: tail                         => (ID(name), tail)
    case _ :: tail                                     => expression(tokens)
    case Nil                                           => throw new ExpectedTokenException(SemicolonToken)
  }

  private def assignStatement(tokens: List[Token]): IntermediateAST =
    tokens match {
      case IDToken(name) :: EqualsToken :: tail =>
        val (assignmentValue, remainingTokens) = statement(tail)
        (Assignment(name, assignmentValue), remainingTokens)
      case IDToken(name) :: LeftParenthesisToken :: tail =>
        val (args, assignmentTokens) = getArgs(tail)
        assignmentTokens match {
          case EqualsToken :: expressionTokens =>
            val (expressionValue, remainingTokens) = expression(expressionTokens)
            (Function(name, args, expressionValue), remainingTokens)
          case _ => throw new ExpectedTokenException(EqualsToken)
        }
      case _ => throw new ExpectedTokenException(EqualsToken)
    }

  private def getArgs(tokens: List[Token], args: List[TypeDeclaration] = Nil): (List[TypeDeclaration], List[Token]) =
    tokens match {
      case IDToken(argName) :: ColonToken :: IDToken(argType) :: CommaToken :: tail =>
        getArgs(tail, args :+ TypeDeclaration(argName, argType))
      case IDToken(argName) :: ColonToken :: IDToken(argType) :: RightParenthesisToken :: tail =>
        (args :+ TypeDeclaration(argName, argType), tail)
      case _ => throw new ParakeetException("TODO: Expected Argument Exception")
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
    case IDToken(name) :: tail          => (ID(name), tail)
    case LeftParenthesisToken :: tail   => innerExpression(tail)
    case unexpectedToken :: _ =>
      throw new UnexpectedTokenException(unexpectedToken)
    case Nil => throw new ParakeetException("No tokens found")
  }

  private def innerExpression(tokens: List[Token]): IntermediateAST = {
    val (node, currTokens) = expression(tokens)

    currTokens match {
      case RightParenthesisToken :: tail => (node, tail)
      case _                             => throw new NoClosingParenthesisException()
    }
  }

  private def typeDeclarationStatement(constantName: String, tokens: List[Token]): IntermediateAST =
    tokens match {
      case IDToken(typeName) :: tail => (TypeDeclaration(constantName, typeName), tail)
      case _                         => throw new ExpectedTypeException()
    }

  private def functionCall(name: String, tokens: List[Token]): IntermediateAST = {
    val (args, remainingTokens) = parseArgs(tokens)
    (FunctionCall(name, args), remainingTokens)
  }

  private def parseArgs(tokens: List[Token]): (List[AbstractSyntaxTree], List[Token]) = {
    def getNextArg(currTokens: List[Token],
                   currArgs: List[AbstractSyntaxTree] = Nil): (List[AbstractSyntaxTree], List[Token]) = {
      val (nextArg, nextTokens) = statement(currTokens)
      nextTokens match {
        case CommaToken :: tail            => getNextArg(tail, currArgs :+ nextArg)
        case RightParenthesisToken :: tail => (currArgs :+ nextArg, tail)
        case _                             => throw new ExpectedArgumentException()
      }
    }

    getNextArg(tokens)
  }
}
