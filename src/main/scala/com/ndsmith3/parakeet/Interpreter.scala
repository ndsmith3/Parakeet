package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, BinaryOperation, Integer, Float}
import com.ndsmith3.parakeet.lexer.{Lexer, Token}

object Interpreter {
  def interpret(input: String): Unit = {
    val tokens: List[Token]     = Lexer.tokenize(input)
    val AST: AbstractSyntaxTree = Parser.parse(tokens)
    println(visit(AST))
  }

  private def visit(abstractSyntaxTree: AbstractSyntaxTree): AnyVal =
    abstractSyntaxTree match {
      case Integer(value) => value.value
      case Float(value)   => value.value
      case BinaryOperation(left: Integer, operator, right: Integer) =>
        operator.op(visit(left).asInstanceOf[Int], visit(right).asInstanceOf[Int])
      case BinaryOperation(left: Float, operator, right: Float) =>
        operator.op(visit(left).asInstanceOf[scala.Float], visit(right).asInstanceOf[scala.Float])
      case _ => throw new Exception("Unknown Node Type")
    }
}
