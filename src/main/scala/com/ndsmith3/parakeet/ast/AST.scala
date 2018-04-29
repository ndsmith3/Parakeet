package com.ndsmith3.parakeet.ast

trait AbstractSyntaxTree

trait Numeric extends AbstractSyntaxTree {
  val value: AnyVal
  def +(that: Numeric): Numeric
  def -(that: Numeric): Numeric
  def *(that: Numeric): Numeric
  def /(that: Numeric): Numeric
  def %(that: Numeric): Numeric

  override def toString: String = value.toString
}

case class Integer(value: Int) extends Numeric {
  override def +(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Integer(value + thatValue)
    case Float(thatValue)   => Float(value + thatValue)
  }

  override def -(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Integer(value - thatValue)
    case Float(thatValue)   => Float(value - thatValue)
  }

  override def *(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Integer(value * thatValue)
    case Float(thatValue)   => Float(value * thatValue)
  }

  override def /(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Integer(value / thatValue)
    case Float(thatValue)   => Float(value / thatValue)
  }

  override def %(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Integer(value % thatValue)
    case Float(thatValue)   => Float(value   % thatValue)
  }
}

case class Float(value: scala.Float) extends Numeric {
  override def +(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Float(value + thatValue)
    case Float(thatValue)   => Float(value + thatValue)
  }

  override def -(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Float(value - thatValue)
    case Float(thatValue)   => Float(value - thatValue)
  }

  override def *(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Float(value * thatValue)
    case Float(thatValue)   => Float(value * thatValue)
  }

  override def /(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Float(value / thatValue)
    case Float(thatValue)   => Float(value / thatValue)
  }

  override def %(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Float(value % thatValue)
    case Float(thatValue)   => Float(value % thatValue)
  }
}

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

case class BinaryOperation(left: AbstractSyntaxTree, op: Operator, right: AbstractSyntaxTree) extends AbstractSyntaxTree
