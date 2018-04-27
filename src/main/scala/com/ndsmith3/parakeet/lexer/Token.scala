package com.ndsmith3.parakeet.lexer

trait Token
case object LeftParenthesis  extends Token
case object RightParenthesis extends Token
case object EmptyToken       extends Token

trait NumericToken                  extends Token
case class IntegerToken(value: Int) extends NumericToken
case class FloatToken(value: Float) extends NumericToken

trait BinaryOperationToken extends Token {
  val marker: Char
  def op(x: Int, y: Int): Int
  def op(x: Float, y: Float): Float
}

case object Add extends BinaryOperationToken {
  val marker                        = '+'
  def op(x: Int, y: Int): Int       = x + y
  def op(x: Float, y: Float): Float = x + y
}

case object Subtract extends BinaryOperationToken {
  val marker                        = '-'
  def op(x: Int, y: Int): Int       = x - y
  def op(x: Float, y: Float): Float = x - y
}

case object Multiply extends BinaryOperationToken {
  val marker                        = '*'
  def op(x: Int, y: Int): Int       = x * y
  def op(x: Float, y: Float): Float = x * y
}

case object Divide extends BinaryOperationToken {
  val marker                        = '/'
  def op(x: Int, y: Int): Int       = x / y
  def op(x: Float, y: Float): Float = x / y
}
