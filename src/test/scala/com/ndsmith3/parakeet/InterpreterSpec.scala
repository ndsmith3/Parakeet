package com.ndsmith3.parakeet

import com.ndsmith3.parakeet.ast.{ASTString, Float, Integer}
import org.scalatest.{FlatSpec, Matchers}

class InterpreterSpec extends FlatSpec with Matchers {
  "The Interpreter" should "return an Integer of value 1 when given an input '1'" in {
    val ast = Interpreter.interpret("1")
    assert(ast == Integer(1))
  }

  it should "return a Float of value 2.2 when given an input '2.2'" in {
    val ast = Interpreter.interpret("2.2")
    assert(ast == Float(2.2))
  }

  it should "return a String of value 'abcdefg' when given an input '\"abcdefg\"'" in {
    val ast = Interpreter.interpret("\"abcdefg\"")
    assert(ast == ASTString("abcdefg"))
  }

  it should "return a Int of value 2 when given an input '1 + 1'" in {
    val ast = Interpreter.interpret("1 + 1")
    assert(ast == Integer(2))
  }
}
