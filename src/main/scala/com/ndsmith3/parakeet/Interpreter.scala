package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, BinaryOperation, Integer, Float, Numeric}
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

object Interpreter {
  def interpret(input: String): AbstractSyntaxTree = {
    val tokens: List[Token]     = Lexer.tokenize(input)
    val AST: AbstractSyntaxTree = Parser.parse(tokens)
    visit(AST)
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree): Numeric =
    abstractSyntaxTree match {
      case int: Integer                           => int
      case float: Float                           => float
      case BinaryOperation(left, operator, right) => operator.eval(visit(left), visit(right))
      case _                                      => throw new Exception("Unknown Node Type")
    }
}
