# C64 assembler

This project is an example project that works as a basis for experiments with
rackets' capabilities to define (and interpret) languages, in this case: 6510 assembler opcodes

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
