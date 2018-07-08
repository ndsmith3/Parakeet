package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.ast.Operator.eval
import com.ndsmith3.parakeet.exception.{ReassignmentException, UnknownTokenException}
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

import scala.language.implicitConversions

object Interpreter {
  type InterpreterState = (AbstractSyntaxTree, Map[String, Primitive])

  def interpret(input: String, scope: Map[String, Primitive] = Map()): InterpreterState = {
    val tokens: List[Token]                  = Lexer.tokenize(input)
    val compoundStatement: CompoundStatement = Parser.parse(tokens)
    execute(compoundStatement, scope)
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree, scope: Map[String, Primitive] = Map()): InterpreterState =
    abstractSyntaxTree match {
      case int: Integer     => (int, scope)
      case float: Float     => (float, scope)
      case char: Character  => (char, scope)
      case str: ASTString   => (str, scope)
      case ID(constantName) => (getConstant(constantName, scope), scope)
      case BinaryOperation(left, operator, right) =>
        (eval(operator, visit(left, scope)._1, visit(right, scope)._1), scope)
    }

  private def getConstant(constantName: String, scope: Map[String, Primitive]): AbstractSyntaxTree =
    scope.get(constantName) match {
      case Some(primitive) => primitive
      case None            => throw new UnknownTokenException(constantName)
    }

  private def execute(compoundStatement: CompoundStatement, scope: Map[String, Primitive]): InterpreterState = {
    def traverse(statements: List[AbstractSyntaxTree], currScope: Map[String, Primitive]): InterpreterState =
      statements match {
        case Nil              => throw new Exception("Passed Nil")
        case statement :: Nil => evalStatement(statement, currScope)
        case statement :: tail =>
          val (_, newScope) = evalStatement(statement, currScope)
          traverse(tail, newScope)
      }

    def evalStatement(statement: AbstractSyntaxTree, scope: Map[String, Primitive]): InterpreterState =
      statement match {
        case Assignment(name, _) if scope contains name => throw new ReassignmentException(name)
        case Assignment(name, value)                    => (statement, scope + (name -> astToPrimitive(visit(value, scope)._1)))
        case ID(_)                                      => (visit(statement, scope)._1, scope)
        case BinaryOperation(left, operator, right) =>
          (eval(operator, visit(left, scope)._1, visit(right, scope)._1), scope)
        case ast => (visit(ast, scope)._1, scope)
      }

    traverse(compoundStatement.statements, scope)
  }

  implicit def astToNumeric(ast: AbstractSyntaxTree): Numeric = ast.asInstanceOf[Numeric]
  implicit def astToPrimitive(ast: AbstractSyntaxTree): Primitive = ast.asInstanceOf[Primitive]
}
