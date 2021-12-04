# amy3

[![Haskell CI](https://github.com/Meowcolm024/amy3/actions/workflows/haskell.yml/badge.svg)](https://github.com/Meowcolm024/amy3/actions/workflows/haskell.yml)

Amy with simple polymorphic type.

## Build

Clone:

``` sh
$ git clone https://github.com/Meowcolm024/amy3.git
```

Build using stack:

``` sh
$ stack build
```

## Usage

To show help:

``` sh
$ stack run -- -h                           
amy3 - The amy3 language interpreter/compiler

Usage: amy3-exe [TARGET...] [-i|--interpret] [-O|--optimize] [-o|--output FILE]
  amy3 is a subset of the Scala Programming Language. It can be directly
  interpreted or compiled to JavaScript

Available options:
  -i,--interpret           Interpret program
  -O,--optimize            Turn on optimization
  -o,--output FILE         Write output to FILE
  -h,--help                Show this help text
```

Run an example:

``` sh
$ stack run -- -i examples/Hi.scala examples/Lib.scala
false
true
Maybe.Just(true)
Input a number ('x' to finish):  
```

## Difference

1. removed *modules*
2. removed *abstract class* and *case class* style, ADT, in favor of the *enum* syntax
3. need to supply a main function using `@main`
4. added parametric types for ADT and functions

## Known issues

The generated JavaScript program is not stable, it may exit will with `max stack size reached`, which is quite strange. This is probably realted to the weired readline...

## Primitives

The following functions are primitive function:

``` scala
// print string
print(s: String): Unit
// print string with new line
println(s: String): Unit
// read string from console
readLine(): String
// parse string to int
toInt(i: String): Int
// convert any type to string
toString(s: Any): String
```
