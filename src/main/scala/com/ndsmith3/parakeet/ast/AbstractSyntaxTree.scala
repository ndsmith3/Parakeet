package com.ndsmith3.parakeet.ast

trait AbstractSyntaxTree {
  val typeName: String = "Unit"
}

trait Primitive extends AbstractSyntaxTree

case class Assignment(constantName: String, value: AbstractSyntaxTree) extends AbstractSyntaxTree
case class ID(constantName: String)                                    extends AbstractSyntaxTree
case class CompoundStatement(statements: List[AbstractSyntaxTree])     extends AbstractSyntaxTree
case class TypeDeclaration(constantName: String, constantType: String) extends AbstractSyntaxTree
case class FunctionCall(functionName: String, args: List[AbstractSyntaxTree]) extends AbstractSyntaxTree

case class Function(name: String, args: List[TypeDeclaration], value: AbstractSyntaxTree) extends Primitive {
  override val typeName = s"(${args.map(_.constantType).mkString(", ")}) -> ${value.typeName}"
}
