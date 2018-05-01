package com.ndsmith3.parakeet.lexer

trait Token
case object LeftParenthesisToken      extends Token
case object RightParenthesisToken     extends Token
case class StringToken(value: String) extends Token

trait NumericToken                     extends Token
case class IntegerToken(value: Int)    extends NumericToken
case class FloatToken(value: Double)   extends NumericToken
case class CharacterToken(value: Char) extends NumericToken

trait BinaryOperationToken extends Token
case object AddToken       extends BinaryOperationToken
case object SubtractToken  extends BinaryOperationToken
case object MultiplyToken  extends BinaryOperationToken
case object DivideToken    extends BinaryOperationToken
case object ModulusToken   extends BinaryOperationToken
case object PowerToken     extends BinaryOperationToken
