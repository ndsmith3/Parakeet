package com.ndsmith3.parakeet.ast

trait Operator extends AbstractSyntaxTree {
  def eval(x: Numeric, y: Numeric): Numeric
}

case object Add extends Operator {
  def eval(x: Numeric, y: Numeric): Numeric = x + y
}

case object Subtract extends Operator {
  def eval(x: Numeric, y: Numeric): Numeric = x - y
}

case object Multiply extends Operator {
  def eval(x: Numeric, y: Numeric): Numeric = x * y
}

case object Divide extends Operator {
  def eval(x: Numeric, y: Numeric): Numeric = x / y
}

case object Modulus extends Operator {
  def eval(x: Numeric, y: Numeric): Numeric = x % y
}

case object Power extends Operator {
  def eval(x: Numeric, y: Numeric): Numeric = x ^ y
}

case class BinaryOperation(left: AbstractSyntaxTree, op: Operator, right: AbstractSyntaxTree) extends AbstractSyntaxTree

