package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter
import com.ndsmith3.parakeet.ast.{AbstractSyntaxTree, Primitive}
import com.ndsmith3.parakeet.exception.{ExceptionTranscriber, ParakeetException}
import com.ndsmith3.parakeet.storage.ValueTable.ValueTable
import com.ndsmith3.parakeet.storage.ValueTable

import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.{Try, Success, Failure}

object Parakeet extends App {
  args.length match {
    case 0 => repl
    case 1 => runFile(args(0))
  }

  def repl: Unit = {
    var valueTable: ValueTable = Nil
    while (true) {
      readLine("parakeet> ") match {
        case "exit" | null => sys.exit(0)
        case source        => valueTable = runSource(source, valueTable)
      }
    }
  }

  def runFile(filePath: String): Unit = runSource(Source.fromFile(filePath).getLines.mkString("\n"))

  def runSource(sourceString: String, valueTable: ValueTable = Nil): ValueTable =
    Try(Interpreter.interpret(sourceString, valueTable)) match {
      case Success((ast, newTable)) =>
        println(ast)
        newTable
      case Failure(exception) =>
        println(ExceptionTranscriber.transcribe(exception.asInstanceOf[ParakeetException]))
        valueTable
    }
}
