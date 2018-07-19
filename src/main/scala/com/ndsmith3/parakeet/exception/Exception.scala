package com.ndsmith3.parakeet.exception

import com.ndsmith3.parakeet.lexer.Token

class ParakeetException(val message: String) extends Throwable

class NoClosingParenthesisException()             extends ParakeetException("Expected Closing Parenthesis.")
class UnexpectedTokenException(token: Token)      extends ParakeetException(s"Unexpected Token: $token.")
class ExpectedTokenException(token: Token)        extends ParakeetException(s"Expected Token: $token")
class ExpectedTypeException                       extends ParakeetException(s"Expected Type Declaration")
class UnexpectedCharacterException(char: Char)    extends ParakeetException(s"Unexpected Character: $char.")
class ExpectedCharacterException(char: Char)      extends ParakeetException(s"Expected Character: $char.")
class CharacterPowerException                     extends ParakeetException(s"Cannot perform exponential operation on Character.")
class ExpectedExpressionException                 extends ParakeetException(s"Expected Expression.")
class UnknownTokenException(constantName: String) extends ParakeetException(s"Unknown Token: $constantName")
class ReassignmentException(constantName: String) extends ParakeetException(s"Cannot rewrite constant: $constantName")
class IdentifierUndefinedException(id: String)    extends ParakeetException(s"Identifier $id is undefined")
class TypeDefinedException(id: String)            extends ParakeetException(s"Type of $id is already defined.")

class IncompatibleTypeException(id: String, actualType: String, newType: String)
    extends ParakeetException(s"Type of $id is defined as $actualType and is incompatible with type $newType")
