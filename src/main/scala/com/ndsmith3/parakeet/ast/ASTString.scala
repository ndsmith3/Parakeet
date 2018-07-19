package com.ndsmith3.parakeet.ast

/*
 * This class is an intermediary method for string implementation. Once data type definitions are implemented,
 * Strings will be implemented as a list of characters, as opposed to a primitive type.
 */

case class ASTString(value: String) extends Primitive {
  override val typeName = "String"
  override def toString: String = value
}
