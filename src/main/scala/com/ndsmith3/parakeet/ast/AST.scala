package com.ndsmith3.parakeet.ast

import com.ndsmith3.parakeet.lexer.{BinaryOperationToken, FloatToken, IntegerToken}

trait AbstractSyntaxTree
case class Integer(value: IntegerToken) extends AbstractSyntaxTree
case class Float(value: FloatToken)     extends AbstractSyntaxTree
case class BinaryOperation(left: AbstractSyntaxTree, op: BinaryOperationToken, right: AbstractSyntaxTree)
    extends AbstractSyntaxTree
