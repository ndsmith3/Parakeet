package com.ndsmith3.parakeet.ast

import org.scalatest.FlatSpec

class NumericSpec extends FlatSpec {
  "A Numeric" should "have a toString value of its value wrapped in a string" in {
    val int: Integer = Integer(1)
    assert(int.isInstanceOf[Numeric]) // asserts that an integer is in fact a numeric
    assert(int.toString == "1")
  }

  "An Integer" should "return Integer(2) when it has a value of 1 and is added to Integer(1)" in {
    assert(Integer(1) + Integer(1) == Integer(2))
  }

  it should "return Integer(0) when it has a value of 1 and is subtracted from Integer(1)" in {
    assert(Integer(1) - Integer(1) == Integer(0))
  }

  it should "return Integer(1) when it has a value of 1 and is multiplied to Integer(1)" in {
    assert(Integer(1) * Integer(1) == Integer(1))
  }

  it should "return Integer(1) when it has a value of 1 and is divided by Integer(1)" in {
    assert(Integer(1) / Integer(1) == Integer(1))
  }

  it should "return Integer(0) when it has a value of 1 and is modded by Integer(1)" in {
    assert(Integer(1) % Integer(1) == Integer(0))
  }

  it should "return Integer(1) when it has a value of 1 and is taken to the power of Integer(1)" in {
    assert((Integer(1) ^ Integer(1)) == Integer(1))
  }

  it should "return Float(2) when it has a value of 1 and is added to Float(1)" in {
    assert(Integer(1) + Float(1) == Float(2))
  }

  it should "return Float(0) when it has a value of 1 and is subtracted by Float(1)" in {
    assert(Integer(1) - Float(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is multiplied to Float(1)" in {
    assert(Integer(1) * Float(1) == Float(1))
  }

  it should "return Float(1) when it has a value of 1 and is divided by Float(1)" in {
    assert(Integer(1) / Float(1) == Float(1))
  }

  it should "return Float(0) when it has a value of 1 and is modded by Float(1)" in {
    assert(Integer(1) % Float(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is taken to the power of Float(1)" in {
    assert((Integer(1) ^ Float(1)) == Float(1))
  }

  it should "return Integer(2) when it has a value of 1 and is added to Character(1)" in {
    assert(Integer(1) + Character(1) == Integer(2))
  }

  it should "return Integer(0) when it has a value of 1 and is subtracted by Character(1)" in {
    assert(Integer(1) - Character(1) == Integer(0))
  }

  it should "return Integer(1) when it has a value of 1 and is multiplied to Character(1)" in {
    assert(Integer(1) * Character(1) == Integer(1))
  }

  it should "return Integer(1) when it has a value of 1 and is divided by Character(1)" in {
    assert(Integer(1) / Character(1) == Integer(1))
  }

  it should "return Integer(0) when it has a value of 1 and is modded by Character(1)" in {
    assert(Integer(1) % Character(1) == Integer(0))
  }

  it should "throw an error when taken to the power of a Character" in {
    assertThrows[Exception] {
      Integer(1) ^ Character(1)
    }
  }

  "A Float" should "return Float(2) when it has a value of 1 and is added to Integer(1)" in {
    assert(Float(1) + Integer(1) == Float(2))
  }

  it should "return Float(0) when it has a value of 1 and is subtracted by Integer(1)" in {
    assert(Float(1) - Integer(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is multiplied by Integer(1)" in {
    assert(Float(1) * Integer(1) == Float(1))
  }

  it should "return Float(1) when it has a value of 1 and is divided by Integer(1)" in {
    assert(Float(1) / Integer(1) == Float(1))
  }

  it should "return Float(0) when it has a value of 1 and is modded by Integer(1)" in {
    assert(Float(1) % Integer(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is taken to the power of Integer(1)" in {
    assert((Float(1) ^ Integer(1)) == Float(1))
  }

  it should "return Float(2) when it has a value of 1 and is added to Float(1)" in {
    assert(Float(1) + Float(1) == Float(2))
  }

  it should "return Float(0) when it has a value of 1 and is subtracted by Float(1)" in {
    assert(Float(1) - Float(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is multiplied to Float(1)" in {
    assert(Float(1) * Float(1) == Float(1))
  }

  it should "return Float(1) when it has a value of 1 and is divided by Float(1)" in {
    assert(Float(1) / Float(1) == Float(1))
  }

  it should "return Float(0) when it has a value of 1 and is modded by Float(1)" in {
    assert(Float(1) % Float(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is taken to the power of Float(1)" in {
    assert((Float(1) ^ Float(1)) == Float(1))
  }

  it should "return Float(2) when it has a value of 1 and is added to Character(1)" in {
    assert(Float(1) + Character(1) == Float(2))
  }

  it should "return Float(0) when it has a value of 1 and is subtracted by Character(1)" in {
    assert(Float(1) - Character(1) == Float(0))
  }

  it should "return Integer(1) when it has a value of 1 and is multiplied to Character(1)" in {
    assert(Float(1) * Character(1) == Float(1))
  }

  it should "return Float(1) when it has a value of 1 and is divided by Character(1)" in {
    assert(Float(1) / Character(1) == Float(1))
  }

  it should "return Integer(0) when it has a value of 1 and is modded by Character(1)" in {
    assert(Float(1) % Character(1) == Float(0))
  }

  it should "throw an error when taken to the power of a Character" in {
    assertThrows[Exception] {
      Float(1) ^ Character(1)
    }
  }

  "A Character" should "return Integer(2) when it has a value of 1 and is added to Integer(1)" in {
    assert(Character(1) + Integer(1) == Integer(2))
  }

  it should "return Integer(0) when it has a value of 1 and is subtracted from Integer(1)" in {
    assert(Character(1) - Integer(1) == Integer(0))
  }

  it should "return Integer(1) when it has a value of 1 and is multiplied to Integer(1)" in {
    assert(Character(1) * Integer(1) == Integer(1))
  }

  it should "return Integer(1) when it has a value of 1 and is divided by Integer(1)" in {
    assert(Character(1) / Integer(1) == Integer(1))
  }

  it should "return Integer(0) when it has a value of 1 and is modded by Integer(1)" in {
    assert(Character(1) % Integer(1) == Integer(0))
  }

  it should "return Float(2) when it has a value of 1 and is added to Float(1)" in {
    assert(Character(1) + Float(1) == Float(2))
  }

  it should "return Float(0) when it has a value of 1 and is subtracted by Float(1)" in {
    assert(Character(1) - Float(1) == Float(0))
  }

  it should "return Float(1) when it has a value of 1 and is multiplied to Float(1)" in {
    assert(Character(1) * Float(1) == Float(1))
  }

  it should "return Float(1) when it has a value of 1 and is divided by Float(1)" in {
    assert(Character(1) / Float(1) == Float(1))
  }

  it should "return Float(0) when it has a value of 1 and is modded by Float(1)" in {
    assert(Character(1) % Float(1) == Float(0))
  }

  it should "return Character(2) when it has a value of 1 and is added to Character(1)" in {
    assert(Character(1) + Character(1) == Character(2))
  }

  it should "return Character(0) when it has a value of 1 and is subtracted by Character(1)" in {
    assert(Character(1) - Character(1) == Character(0))
  }

  it should "return Integer(1) when it has a value of 1 and is multiplied to Character(1)" in {
    assert(Character(1) * Character(1) == Character(1))
  }

  it should "return Integer(1) when it has a value of 1 and is divided by Character(1)" in {
    assert(Character(1) / Character(1) == Character(1))
  }

  it should "return Integer(0) when it has a value of 1 and is modded by Character(1)" in {
    assert(Character(1) % Character(1) == Character(0))
  }

  it should "throw an error when taken to the power of any Numeric type" in {
    assertThrows[Exception] {
      Character(1) ^ Integer(1)
    }
  }
}
