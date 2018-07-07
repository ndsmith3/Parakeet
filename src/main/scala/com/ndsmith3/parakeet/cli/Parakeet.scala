package com.ndsmith3.parakeet.cli

import com.ndsmith3.parakeet.Interpreter
import com.ndsmith3.parakeet.ast.Primitive
import scala.io.StdIn.readLine

object Parakeet extends App {
  var scope: Map[String, Primitive] = Map()
  while (true) {
    readLine("parakeet> ") match {
      case "exit" | null => sys.exit(0)
      case exp =>
        val (ast, newScope) = Interpreter.interpret(exp, scope)
        scope = newScope
        println(ast)
    }
  }
}
