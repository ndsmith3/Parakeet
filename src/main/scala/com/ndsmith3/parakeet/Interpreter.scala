package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.ast.Operator.eval
import com.ndsmith3.parakeet.exception.UnknownTokenException
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

import scala.language.implicitConversions

object Interpreter {
  def interpret(input: String): AbstractSyntaxTree = {
    val tokens: List[Token]     = Lexer.tokenize(input)
    val AST: AbstractSyntaxTree = Parser.parse(tokens)
    visit(AST)
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree, currAST: AbstractSyntaxTree = EOF): AbstractSyntaxTree =
    abstractSyntaxTree match {
      case int: Integer    => int
      case float: Float    => float
      case char: Character => char
      case str: ASTString  => str
      case ID(constantName) =>
        currAST.scope.get(constantName) match {
          case Some(primitive) => primitive
          case None            => throw new UnknownTokenException(constantName)
        }
      case BinaryOperation(left, operator, right) => eval(operator, visit(left), visit(right))
      case compoundStatement: CompoundStatement =>
        compoundStatement.statements.foldLeft(compoundStatement.statements.head) { (currAST, currStatement) =>
          currStatement match {
            case Assignment(constantName, value) =>
              IntermediateAbstractSyntaxTree(currAST.scope + (constantName -> visit(value)))
            case ID(_)                                  => visit(currStatement, currAST)
            case BinaryOperation(left, operator, right) => eval(operator, visit(left, currAST), visit(right, currAST))
          }
        }
    }

  implicit def astToNumeric(ast: AbstractSyntaxTree): Numeric = ast.asInstanceOf[Numeric]
}
