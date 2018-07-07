package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter
import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, Primitive}
import com.ndsmith3.parakeet.exception.{ExceptionTranscriber, ParakeetException}

import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}

object Parakeet extends App {
  var scope: Map[String, Primitive] = Map()

  while (true) {
    readLine("parakeet> ") match {
      case "exit" | null => sys.exit(0)
      case expression =>
        Try(Interpreter.interpret(expression, scope)) match {
          case Success((ast, newScope)) =>
            scope = newScope
            println(ast)
          case Failure(exception) =>
            println(ExceptionTranscriber.transcribe(exception.asInstanceOf[ParakeetException]))
        }
    }
  }
}
