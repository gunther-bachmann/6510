# C64 assembler

This project is an example project that works as a basis for experiments with
rackets' capabilities to define (and interpret) languages, in this case: 6510 assembler opcodes

## prerequisites

* racket 7.3+ (`nix-env -i racket`)
* megaparsack (`raco pkg install megaparsack-lib`)
* threading library (`raco pkg install threading`)

## Structure

The 6510-reader is parsing a non racket text file, transforming it into racket code. The resulting racket code is then transformed via syntax macros into the final racket form which can then be interpreted.

The 6510 holds all syntax transformation rules for the translation into 6510 byte/assembler code.
* utils holds functions needed during syntax and execution phase of 6510.
* syntax-utils holds functions useful during syntax phase of the transformation.
* reader is the parser that takes the text file and produces racket 6510 dsl code.
* example is an example file using arbitrary 6510 text syntax.
* example-rs is an example file in racket syntax (no special reader involved)
* interpreter holds the (currently minimal) interpreter of the bytecode


## Status

Open 6510-reader.rkt and execute a repl on that file (emacs: C-c C-c).
It provides some info what data is of interest for closer inspection.

To run tests, run `raco test 6510-reader.rkt` for example.

## Todos

### Next

#### implementation without new knowledge
* Warp multitude of flag-methods in 6510-interpreter into simple set of functions working on arbitrary flags, stored in a register, at some arbitrary position
* extend pretty print to show in memory location in the first 4 characters (e.g. C000 A9 41    LDA #$41
* extend pretty print to write comments, too
* extend pretty print to write round instead of angular brackets
#### ... with some new knowledge
* Increase test coverage
* Implement macros (as in macro assemblers)
#### ... lots of new concepts
* Document functions and their usage (with scrible? inline?)
* Identify concept to carry parser context from reader to racket macros.  
   The basic idea is to have line-number information of the original (parsed) source code
   available at macro expansion time, to provide better error messaging etc.
   This is currently implemented (rather awkwardly I might add) by explicitly passing
   this information from parser to macro.
* Implement multi file assembler programs with references crossing file boundaries  
   This will probably include to export/import symbols.

### Some day
* Extend interpreter to be able to interpret more up to all opcodes
* Write a language to generate parser combination using an extended ebnf syntax, enriched with code, to allow a more compact and less verbose definition of the syntax/parser used here.

### Maybe
* Extend interpreter to allow for c64-like output functions when calling rom addresses
* Emulate text mode (fixed charset) of c64
