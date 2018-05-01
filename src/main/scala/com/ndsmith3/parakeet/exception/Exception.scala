package com.ndsmith3.parakeet.exception

import com.ndsmith3.parakeet.lexer.Token

class NoClosingParenthesisException()        extends Exception("Expected Closing parenthesis.")
class UnexpectedTokenException(token: Token) extends Exception(s"Unexpected Token: $token")
