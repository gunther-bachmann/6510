# C64 assembler

This project is an example project that works as a basis for experiments with
rackets' capabilities to define (and interpret) languages, in this case: 6510 assembler opcodes

## Structure

The 6510-reader is parsing a non racket text file, transforming it into racket code. The resulting racket code is then transformed via syntax macros into the final racket form which can then be interpreted.

The 6510 holdst he interpreter and syntax transformation.
*-utils holds functions needed during syntax and execution phase of 6510.
*-reader is the parser that takes the text file and produces racket 6510 dsl code.
*-example is an example file using arbitrary 6510 text syntax.

## Status

Open 6510-reader.rkt and execute a repl on that file (emacs: C-c C-c).
It displays the list of bytes the program is compiled to and the opcode as it is processed by the syntax macros.
