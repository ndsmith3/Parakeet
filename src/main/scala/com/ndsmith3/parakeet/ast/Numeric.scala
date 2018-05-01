package com.ndsmith3.parakeet.ast

import com.ndsmith3.parakeet.exception.CharacterPowerException

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
    case Character(_)       => throw new CharacterPowerException()
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
    case Character(_)       => throw new CharacterPowerException()
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

  override def ^(that: Numeric): Numeric = throw new CharacterPowerException()
}
