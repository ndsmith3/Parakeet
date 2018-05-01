package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.ast.Operator.eval
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

import scala.language.implicitConversions

object Interpreter {
  def interpret(input: String): AbstractSyntaxTree = {
    val tokens: List[Token]     = Lexer.tokenize(input)
    val AST: AbstractSyntaxTree = Parser.parse(tokens)
    visit(AST)
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree): Primitive =
    abstractSyntaxTree match {
      case int: Integer                           => int
      case float: Float                           => float
      case char: Character                        => char
      case str: ASTString                         => str
      case BinaryOperation(left, operator, right) => eval(operator, visit(left), visit(right))
      case _                                      => throw new Exception("Unknown Node Type")
    }

  implicit def primitiveToNumeric(primitive: Primitive): Numeric = primitive.asInstanceOf[Numeric]
}
