package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.ast.Operator.eval
import com.ndsmith3.parakeet.exception.{ReassignmentException, UnknownTokenException}
import com.ndsmith3.parakeet.lexer.{Lexer, Token}
import com.ndsmith3.parakeet.storage.ValueTable.ValueTable
import com.ndsmith3.parakeet.storage.ValueTable

import scala.language.implicitConversions

object Interpreter {
  type InterpreterState = (AbstractSyntaxTree, ValueTable)

  def interpret(input: String, valueTable: ValueTable = Nil): InterpreterState = {
    val tokens: List[Token]                  = Lexer.tokenize(input)
    val compoundStatement: CompoundStatement = Parser.parse(tokens)
    execute(compoundStatement, valueTable)
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree, valueTable: ValueTable = Nil): InterpreterState =
    abstractSyntaxTree match {
      case int: Integer     => (int, valueTable)
      case float: Float     => (float, valueTable)
      case char: Character  => (char, valueTable)
      case str: ASTString   => (str, valueTable)
      case ID(constantName) => (ValueTable.get(valueTable, constantName), valueTable)
      case BinaryOperation(left, operator, right) =>
        (eval(operator, visit(left, valueTable)._1, visit(right, valueTable)._1), valueTable)
    }

  private def execute(compoundStatement: CompoundStatement, valueTable: ValueTable): InterpreterState = {
    def traverse(statements: List[AbstractSyntaxTree], currvalueTable: ValueTable): InterpreterState =
      statements match {
        case Nil              => throw new Exception("Passed Nil")
        case statement :: Nil => evalStatement(statement, currvalueTable)
        case statement :: tail =>
          val (_, newvalueTable) = evalStatement(statement, currvalueTable)
          traverse(tail, newvalueTable)
      }

    def evalStatement(statement: AbstractSyntaxTree, valueTable: ValueTable): InterpreterState =
      statement match {
        case Assignment(name, value)         => (statement, ValueTable.addValue(valueTable, name, astToPrimitive(value)))
        case TypeDeclaration(name, typeName) => (statement, ValueTable.addType(valueTable, name, typeName))
        case ID(_)                           => (visit(statement, valueTable)._1, valueTable)
        case ast                             => (visit(ast, valueTable)._1, valueTable)
      }

    traverse(compoundStatement.statements, valueTable)
  }

  implicit def astToNumeric(ast: AbstractSyntaxTree): Numeric     = ast.asInstanceOf[Numeric]
  implicit def astToPrimitive(ast: AbstractSyntaxTree): Primitive = ast.asInstanceOf[Primitive]
}
