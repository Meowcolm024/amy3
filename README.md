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
False
True
Maybe.Just(True)
Input a number ('x' to finish): 
```

## Features

OwO
