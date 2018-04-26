package com.ndsmith3.parakeet.ast

import com.ndsmith3.parakeet.lexer.{BinaryOperationToken, IntegerToken}

trait AbstractSyntaxTree
case class Constant(value: IntegerToken) extends AbstractSyntaxTree
case class BinaryOperation(left: AbstractSyntaxTree, op: BinaryOperationToken, right: AbstractSyntaxTree)
    extends AbstractSyntaxTree
