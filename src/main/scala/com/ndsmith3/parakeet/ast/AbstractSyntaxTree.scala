package com.ndsmith3.parakeet.ast

trait AbstractSyntaxTree
trait Primitive extends AbstractSyntaxTree

case class Assignment(constantName: String, value: AbstractSyntaxTree) extends AbstractSyntaxTree
case class ID(constantName: String)                                    extends AbstractSyntaxTree
case class CompoundStatement(statements: List[AbstractSyntaxTree])     extends AbstractSyntaxTree
