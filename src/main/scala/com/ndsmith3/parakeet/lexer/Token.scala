package com.ndsmith3.parakeet.lexer

trait Token
case object LeftParenthesisToken       extends Token
case object RightParenthesisToken      extends Token
case object AssignToken                extends Token
case object EqualsToken                extends Token
case object SemicolonToken             extends Token
case object ColonToken                 extends Token
case class IDToken(name: String) extends Token

trait PrimitiveToken extends Token {
  val value: Any
}

case class StringToken(value: String)  extends PrimitiveToken
trait NumericToken                     extends PrimitiveToken
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
