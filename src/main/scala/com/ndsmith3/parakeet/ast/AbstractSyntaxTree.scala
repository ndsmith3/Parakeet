package com.ndsmith3.parakeet.ast

trait AbstractSyntaxTree {
  val scope: Map[String, Primitive] = Map()
}
trait Primitive extends AbstractSyntaxTree

case class IntermediateAbstractSyntaxTree(override val scope: Map[String, Primitive]) extends AbstractSyntaxTree
case class Assignment(constantName: String, value: AbstractSyntaxTree)                extends AbstractSyntaxTree
case class ID(constantName: String)                                                   extends AbstractSyntaxTree
case class CompoundStatement(statements: List[AbstractSyntaxTree])                    extends AbstractSyntaxTree
case object EOF                                                                       extends AbstractSyntaxTree
