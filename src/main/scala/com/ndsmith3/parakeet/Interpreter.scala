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

  private def visit(abstractSyntaxTree: AbstractSyntaxTree,
                    scope: Map[String, Primitive] = Map()): AbstractSyntaxTree =
    abstractSyntaxTree match {
      case int: Integer                           => int
      case float: Float                           => float
      case char: Character                        => char
      case str: ASTString                         => str
      case ID(constantName)                       => getConstant(constantName, scope)
      case BinaryOperation(left, operator, right) => eval(operator, visit(left, scope), visit(right, scope))
      case compoundStatement: CompoundStatement   => execute(compoundStatement)
    }

  private def getConstant(constantName: String, scope: Map[String, Primitive]): AbstractSyntaxTree =
    scope.get(constantName) match {
      case Some(primitive) => primitive
      case None            => throw new UnknownTokenException(constantName)
    }

  private def execute(compoundStatement: CompoundStatement): AbstractSyntaxTree = {
    def traverse(statements: List[AbstractSyntaxTree], scope: Map[String, Primitive] = Map()): AbstractSyntaxTree =
      statements match {
        case Nil              => throw new Exception("Passed Nil")
        case statement :: Nil => evalStatement(statement, scope)._1
        case statement :: tail =>
          val (_, newScope) = evalStatement(statement, scope)
          traverse(tail, newScope)
      }

    def evalStatement(statement: AbstractSyntaxTree,
                      scope: Map[String, Primitive]): (AbstractSyntaxTree, Map[String, Primitive]) =
      statement match {
        case Assignment(name, _) if scope contains name => throw new ReassignmentException(name)
        case Assignment(name, value)                    => (statement, scope + (name -> visit(value, scope)))
        case ID(_)                                      => (visit(statement, scope), scope)
        case BinaryOperation(left, operator, right)     => (eval(operator, visit(left, scope), visit(right, scope)), scope)
        case ast                                        => (visit(ast, scope), scope)
      }

    traverse(compoundStatement.statements)
  }

  implicit def astToNumeric(ast: AbstractSyntaxTree): Numeric = ast.asInstanceOf[Numeric]
}
