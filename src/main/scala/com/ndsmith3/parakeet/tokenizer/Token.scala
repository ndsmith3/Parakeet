package com.ndsmith3.parakeet.tokenizer

trait Token
case object EmptyToken              extends Token
case class IntegerToken(value: Int) extends Token

trait BinaryOperationToken extends Token {
  val marker: Char
  def op(x: IntegerToken, y: IntegerToken): IntegerToken
}

case object Add extends BinaryOperationToken {
  val marker                                             = '+'
  def op(x: IntegerToken, y: IntegerToken): IntegerToken = IntegerToken(x.value + y.value)
}

case object Subtract extends BinaryOperationToken {
  val marker                                             = '-'
  def op(x: IntegerToken, y: IntegerToken): IntegerToken = IntegerToken(x.value - y.value)
}

case object Multiply extends BinaryOperationToken {
  val marker                                             = '*'
  def op(x: IntegerToken, y: IntegerToken): IntegerToken = IntegerToken(x.value * y.value)
}

case object Divide extends BinaryOperationToken {
  val marker                                             = '/'
  def op(x: IntegerToken, y: IntegerToken): IntegerToken = IntegerToken(x.value / y.value)
}
