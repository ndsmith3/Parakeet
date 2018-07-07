# Parakeet

An interpreted, statically typed, lazily evaluated functional programming language. Parakeet is an attempt to learn more about programming languages and interpreters.

### Goal
Parakeet's goal is to be a language with pure functional features while remaining lightweight. Many purely functional languages are hindered by slow compilers and difficult syntax for programmers coming from popular programming languages in other paradigms. Parakeet is designed to be easily picked up by all programmers, whether it is their first functional language or not, while being a good tool for quick iteration.

## Use

Parakeet is currently only usable as a REPL application.

### SBT Commands

 * `compile`: Compile the project
 * `test`: Compile the project and run the test suite
 * `run`: Compile the project and run the Parakeet REPL
 * `clean`: Deletes files created by SBT
 * `package`: Produces a .jar artifact
 * `tasks`: See other available tasks

### Other Commands

 * `scalafmt`: Scala formatter for the project
  * Formats source files in the style outlined in the project's `.scalafmt.conf` file

### Application Commands

This section assumes you have a built version of Parakeet with the binary name `parakeet`

 * `parakeet`: Launches the REPL

## Todo
* ~~AST Structure~~
* ~~Numeric Evaluation~~
    * ~~+, -, *, / operators~~
    * ~~Modulus operator~~
    * ~~Exponent operator~~
* ~~Primitive String Evalution~~
* ~~Start test suite for unit testing previously completed work~~
* ~~Primitive Type Storage~~
* ~~REPL storage between commands~~
* ~~Error handling for Parsing and Runtime~~
* Source files
* Type Declaration
* Type Restriction
* Entrypoint definition
* Algebraic Data Type evalution
* String Data Type
* Lazy Evaluation
* Custom Data Type Storage
* Pattern Matching
* Robust Stacktraces for Error Handling
* Type Inference
