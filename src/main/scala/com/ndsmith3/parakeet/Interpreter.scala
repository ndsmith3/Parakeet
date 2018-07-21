package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.ast.Operator.eval
import com.ndsmith3.parakeet.exception.{ReassignmentException, UnknownTokenException, ParakeetException}
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
      case FunctionCall(name, args) =>
        val function = ValueTable.get(valueTable, name).asInstanceOf[Function]
        val functionTypeValueTable: ValueTable = function.args.foldLeft(valueTable){ (table, decl) =>
          ValueTable.addType(table, decl.constantName, decl.constantType)
        }
        val functionValueTable: ValueTable = loadValues(valueTable, function.args, args)

        (visit(function.value, functionValueTable)._1, valueTable)
      case ID(constantName) => (ValueTable.get(valueTable, constantName), valueTable)
      case BinaryOperation(left, operator, right) =>
        (eval(operator, visit(left, valueTable)._1, visit(right, valueTable)._1), valueTable)
    }

  private def loadValues(table: ValueTable, argSpec: List[TypeDeclaration], args: List[AbstractSyntaxTree]): ValueTable = {
    def loadValue(currTable: ValueTable, currSpec: List[TypeDeclaration], currArgs: List[AbstractSyntaxTree]): ValueTable = {
      (currSpec, currArgs) match {
        case (Nil, Nil) => currTable
        case (_, Nil)   => throw new ParakeetException("TODO: Not enough args")
        case (Nil, _)   => throw new ParakeetException("TODO: Too many args")
        case ((spec :: specTail), (arg :: argTail)) => loadValue(ValueTable.addValue(currTable, spec.constantName, astToPrimitive(arg)), specTail, argTail)
      }
    }

    loadValue(table, argSpec, args)
  }

  private def execute(compoundStatement: CompoundStatement, valueTable: ValueTable): InterpreterState = {
    def traverse(statements: List[AbstractSyntaxTree], currvalueTable: ValueTable): InterpreterState =
      statements match {
        case Nil              => throw new ParakeetException("Passed Nil")
        case statement :: Nil => evalStatement(statement, currvalueTable)
        case statement :: tail =>
          val (_, newvalueTable) = evalStatement(statement, currvalueTable)
          traverse(tail, newvalueTable)
      }

    def evalStatement(statement: AbstractSyntaxTree, valueTable: ValueTable): InterpreterState = {
      statement match {
        case Assignment(name, value)         => (statement, ValueTable.addValue(valueTable, name, astToPrimitive(visit(value, valueTable)._1)))
        case Function(name, args, value)     => (statement, ValueTable.addValue(valueTable, name, astToPrimitive(statement)))
        case TypeDeclaration(name, typeName) => (statement, ValueTable.addType(valueTable, name, typeName))
        case ID(_)                           => (visit(statement, valueTable)._1, valueTable)
        case ast                             => (visit(ast, valueTable)._1, valueTable)
      }
    }

    traverse(compoundStatement.statements, valueTable)
  }

  implicit def astToNumeric(ast: AbstractSyntaxTree): Numeric     = ast.asInstanceOf[Numeric]
  implicit def astToPrimitive(ast: AbstractSyntaxTree): Primitive = ast.asInstanceOf[Primitive]
}
