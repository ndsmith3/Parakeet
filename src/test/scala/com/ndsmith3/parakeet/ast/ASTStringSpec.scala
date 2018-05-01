package com.ndsmith3.parakeet.ast

import org.scalatest.FlatSpec

class ASTStringSpec extends FlatSpec {
  "An ASTString" should "have a toString value equal to its value" in {
    assert(ASTString("abcdefg").toString == "abcdefg")
  }

  it should "be castable to Primitive" in {
    assertCompiles("ASTString(\"abcedfg\").asInstanceOf[Primitive]")
  }
}
