package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.ast.Operator.eval
import com.ndsmith3.parakeet.exception.{ReassignmentException, UnknownTokenException}
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
      case int: Integer                           => int
      case float: Float                           => float
      case char: Character                        => char
      case str: ASTString                         => str
      case ID(constantName)                       => getConstant(constantName, currAST)
      case BinaryOperation(left, operator, right) => eval(operator, visit(left), visit(right))
      case compoundStatement: CompoundStatement   => execute(compoundStatement)
    }

  private def getConstant(constantName: String, currAST: AbstractSyntaxTree): AbstractSyntaxTree =
    currAST.scope.get(constantName) match {
      case Some(primitive) => primitive
      case None            => throw new UnknownTokenException(constantName)
    }

  private def execute(compoundStatement: CompoundStatement): AbstractSyntaxTree =
    compoundStatement.statements.foldLeft(compoundStatement.statements.head) { (currAST, currStatement) =>
      currStatement match {
        case Assignment(name, _) if currAST.scope contains name => throw new ReassignmentException(name)
        case Assignment(name, value)                            => IntermediateAbstractSyntaxTree(currAST.scope + (name -> visit(value)))
        case ID(_)                                              => visit(currStatement, currAST)
        case BinaryOperation(left, operator, right)             => eval(operator, visit(left, currAST), visit(right, currAST))
      }
    }

  implicit def astToNumeric(ast: AbstractSyntaxTree): Numeric = ast.asInstanceOf[Numeric]
}
