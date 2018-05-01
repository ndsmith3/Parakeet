package com.ndsmith3.parakeet.ast

import Operator.eval
import org.scalatest.FlatSpec

class OperatorSpec extends FlatSpec {
  "The Add Operator" should "evaluate adding Numerics" in {
    assert(eval(Add, Integer(1), Integer(1)) == Integer(2))
  }

  "The Subtract Operator" should "evaluate subtracting Numerics" in {
    assert(eval(Subtract, Integer(1), Integer(1)) == Integer(0))
  }

  "The Multiply Operator" should "evaluate multiplying Numerics" in {
    assert(eval(Multiply, Integer(1), Integer(1)) == Integer(1))
  }

  "The Divide Operator" should "evaluate dividing Numerics" in {
    assert(eval(Divide, Integer(1), Integer(1)) == Integer(1))
  }

  "The Modulus Operator" should "evaluate to find the remainder of Numerics" in {
    assert(eval(Modulus, Integer(1), Integer(1)) == Integer(0))
  }

  "The Power Operator" should "evaluate to find the power of Numerics" in {
    assert(eval(Power, Integer(1), Integer(1)) == Integer(1))
  }
}
