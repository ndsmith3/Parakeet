package com.ndsmith3.parakeet.exception

import com.ndsmith3.parakeet.lexer.Token
class NoClosingParenthesisException()          extends Exception("Expected Closing Parenthesis.")
class UnexpectedTokenException(token: Token)   extends Exception(s"Unexpected Token: $token.")
class ExpectedTokenException(token: Token)     extends Exception(s"Expected Token: $token")
class UnexpectedCharacterException(char: Char) extends Exception(s"Unexpected Character: $char.")
class CharacterPowerException                  extends Exception(s"Cannot perform exponential operation on Character.")
