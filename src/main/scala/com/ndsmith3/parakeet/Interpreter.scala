package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, BinaryOperation, Integer}
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

object Interpreter {
  def interpret(input: String): Unit = {
    val tokens: List[Token]     = Lexer.tokenize(input)
    val AST: AbstractSyntaxTree = Parser.parse(tokens)
    println(visit(AST))
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree): Int = {
    abstractSyntaxTree match {
      case Integer(value)                   => value.value
      case BinaryOperation(left, op, right) => op.op(visit(left), visit(right))
      case _                                => throw new Exception("Unknown Node Type")
    }
  }
}
