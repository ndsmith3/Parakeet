package com.ndsmith3.parakeet.ast

trait AbstractSyntaxTree
trait Primitive                                         extends AbstractSyntaxTree
case class Assignment(constantName: String, value: Any) extends AbstractSyntaxTree
