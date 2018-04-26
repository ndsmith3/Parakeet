package com.ndsmith3.parakeet.lexer

trait Token
case object LeftParenthesis         extends Token
case object RightParenthesis        extends Token
case object EmptyToken              extends Token
case class IntegerToken(value: Int) extends Token

trait BinaryOperationToken extends Token {
  val marker: Char
  def op(x: Int, y: Int): Int
}

case object Add extends BinaryOperationToken {
  val marker                  = '+'
  def op(x: Int, y: Int): Int = x + y
}

case object Subtract extends BinaryOperationToken {
  val marker                  = '-'
  def op(x: Int, y: Int): Int = x - y
}

case object Multiply extends BinaryOperationToken {
  val marker                  = '*'
  def op(x: Int, y: Int): Int = x * y
}

case object Divide extends BinaryOperationToken {
  val marker                  = '/'
  def op(x: Int, y: Int): Int = x / y
}
