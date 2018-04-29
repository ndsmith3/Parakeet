package com.ndsmith3.parakeet.ast

trait Numeric extends Primitive {
  val value: AnyVal
  def +(that: Numeric): Numeric
  def -(that: Numeric): Numeric
  def *(that: Numeric): Numeric
  def /(that: Numeric): Numeric
  def %(that: Numeric): Numeric
  def ^(that: Numeric): Numeric

  override def toString: String = value.toString
}

case class Integer(value: Int) extends Numeric {
  override def +(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value + thatValue)
    case Float(thatValue)     => Float(value + thatValue)
    case Character(thatValue) => Integer(value + thatValue)
  }

  override def -(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value - thatValue)
    case Float(thatValue)     => Float(value - thatValue)
    case Character(thatValue) => Integer(value - thatValue)
  }

  override def *(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value * thatValue)
    case Float(thatValue)     => Float(value * thatValue)
    case Character(thatValue) => Integer(value * thatValue)
  }

  override def /(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value / thatValue)
    case Float(thatValue)     => Float(value / thatValue)
    case Character(thatValue) => Integer(value / thatValue)
  }

  override def %(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value % thatValue)
    case Float(thatValue)     => Float(value   % thatValue)
    case Character(thatValue) => Integer(value % thatValue)
  }

  override def ^(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Integer(Math.pow(value, thatValue).toInt)
    case Float(thatValue)   => Float(Math.pow(value, thatValue))
    case Character(_)       => throw new Exception("Cannot perform exponential on Character.")
  }
}

case class Float(value: Double) extends Numeric {
  override def +(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Float(value + thatValue)
    case Float(thatValue)     => Float(value + thatValue)
    case Character(thatValue) => Float(value + thatValue)
  }

  override def -(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Float(value - thatValue)
    case Float(thatValue)     => Float(value - thatValue)
    case Character(thatValue) => Float(value - thatValue)
  }

  override def *(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Float(value * thatValue)
    case Float(thatValue)     => Float(value * thatValue)
    case Character(thatValue) => Float(value * thatValue)
  }

  override def /(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Float(value / thatValue)
    case Float(thatValue)     => Float(value / thatValue)
    case Character(thatValue) => Float(value / thatValue)
  }

  override def %(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Float(value % thatValue)
    case Float(thatValue)     => Float(value % thatValue)
    case Character(thatValue) => Float(value % thatValue)
  }

  override def ^(that: Numeric): Numeric = that match {
    case Integer(thatValue) => Float(Math.pow(value, thatValue))
    case Float(thatValue)   => Float(Math.pow(value, thatValue))
    case Character(_)       => throw new Exception("Cannot perform exponential on Character.")
  }
}

case class Character(value: Char) extends Numeric {
  override def +(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value + thatValue)
    case Float(thatValue)     => Float(value + thatValue)
    case Character(thatValue) => Character((value + thatValue).toChar)
  }

  override def -(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value - thatValue)
    case Float(thatValue)     => Float(value - thatValue)
    case Character(thatValue) => Character((value - thatValue).toChar)
  }

  override def *(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value * thatValue)
    case Float(thatValue)     => Float(value * thatValue)
    case Character(thatValue) => Character((value * thatValue).toChar)
  }

  override def /(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value / thatValue)
    case Float(thatValue)     => Float(value / thatValue)
    case Character(thatValue) => Character((value / thatValue).toChar)
  }

  override def %(that: Numeric): Numeric = that match {
    case Integer(thatValue)   => Integer(value % thatValue)
    case Float(thatValue)     => Float(value % thatValue)
    case Character(thatValue) => Character((value % thatValue).toChar)
  }

  override def ^(that: Numeric): Numeric = throw new Exception("Cannot perform exponential on Character.")
}
