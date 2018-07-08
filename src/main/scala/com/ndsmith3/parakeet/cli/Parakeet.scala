package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter
import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, Primitive}
import com.ndsmith3.parakeet.exception.{ExceptionTranscriber, ParakeetException}

import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.{Try, Success, Failure}

object Parakeet extends App {
  args.length match {
    case 0 => repl
    case 1 => runFile(args(0))
  }

  def repl: Unit = {
    var scope: Map[String, Primitive] = Map()
    while (true) {
      readLine("parakeet> ") match {
        case "exit" | null => sys.exit(0)
        case source        => scope = runSource(source, scope)
      }
    }
  }

  def runFile(filePath: String): Unit = runSource(Source.fromFile(filePath).getLines.mkString("\n"))

  def runSource(sourceString: String, scope: Map[String, Primitive] = Map()): Map[String, Primitive] =
    Try(Interpreter.interpret(sourceString, scope)) match {
      case Success((ast, newScope)) =>
        println(ast)
        newScope
      case Failure(exception) =>
        println(ExceptionTranscriber.transcribe(exception.asInstanceOf[ParakeetException]))
        scope
    }
}
