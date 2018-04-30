package com.ndsmith3.parakeet.ast

object Operator extends AbstractSyntaxTree {
  def eval(operator: Operator, x: Numeric, y: Numeric): Numeric = operator match {
    case Add      => x + y
    case Subtract => x - y
    case Multiply => x * y
    case Divide   => x / y
    case Modulus  => x % y
    case Power    => x ^ y
  }
}

trait Operator
case object Add      extends Operator
case object Subtract extends Operator
case object Multiply extends Operator
case object Divide   extends Operator
case object Modulus  extends Operator
case object Power    extends Operator

case class BinaryOperation(left: AbstractSyntaxTree, op: Operator, right: AbstractSyntaxTree) extends AbstractSyntaxTree
