# Quickmaffs 

A small arithmetic language and set of tooling, intended to demonstrate 
how easy it is to write Language Servers using [Langoustine](https://github.com/neandertech/langoustine) 
and [Jsonrpclib](https://github.com/neandertech/jsonrpclib).

Through the magic of rabbit holes, the project has a batch interpreter and 
a REPL as well for some reason.

[Scala 3](https://docs.scala-lang.org/scala3/book/introduction.html#), 
[Scala CLI](https://scala-cli.virtuslab.org), 
[Cats-effect 3](https://typelevel.org/cats-effect/),
[FS2](https://fs2.io/#/)

## Pre-requisites

1. Ensure you have [Scala CLI](https://scala-cli.virtuslab.org) installed

## Build

* LSP: `make lsp`
* Batch interpreter: `make interpreter`
* REPL: `make repl`

Everything: `make all`

## Usage 

LSP: configure your editor to use `bin/lsp` as a language server for `*.qmf` files 

![](/docs/lsp.gif)

REPL: `bin/repl`

![](/docs/repl.gif)

Interpreter

![](/docs/interpreter.png)

