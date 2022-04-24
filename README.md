# C64 assembler

This project is an example project that works as a basis for experiments with
rackets' capabilities to define (and interpret) languages, in this case: 6510 assembler opcodes

## prerequisites

* racket 7.3+ (`nix-env -i racket`)
* megaparsack (`raco pkg install megaparsack-lib`)
* threading library (`raco pkg install threading`)

## usage

### running

You may execute the example program directly:
```sh
  ./6510-example.rkt
```
This will
1. translate the given assembler program into the 6510 codes
2. run an interpreter on that code (printing out 'AB' as a result)
3. and write a `test.d64` disk image, usable by vice (c64 emulator)
(take a look at the commands actually executed by `6510-reader.rkt`)

Another way to run the example is to open `6510-example.rkt` in emacs and start racket via `C-c C-c`.
After the program is run you can in addition do one of the following
- print out a prettified internal representation of the compiled program
  via `(display pretty-program)`
- print out the intermediate translation of the original source code into the internal representation
  via `(display raw-program)`
- start the c64 emulator with the disk image attached to drive 8 and run the following commands w/i the emulator
```sh
load "*",8,1
sys 49152
```

### error messages

Error message are somewhat cryptic so I collected some examples to make reading them a bit easier.

If illegal opcodes or other elements are used that are not understood by the compiler, the following error message may occur:

The following error is given, if on line 12 (it's always -1 because of the swallowed first she-bang line),
an unknown opcode (starting with k) is given:

    ; .../6510/string:11:8: parse error
    ;   unexpected: k
    ;   expected: end of input
    ; Context:
    ;  .../6510-reader.rkt:346:0 literal-read-syntax

The following error is given, if on line 12 (off by one, see above) an opcode operand is expected
that may either start with `$` (hex number), `%` (dual number), `(` for indirect addressing operands
`:` for label references, `A` for accumluator operand, an integer for a decimal number operand
BUT no implicit may be given (by the opcode identified, in that case it was a `jsr`)

    ; .../6510/string:11:12: parse error
    ;   unexpected: #
    ;   expected: '$', '%', '(', ':', 'A', integer, or no implicit
    ; Context:
    ;  .../6510-reader.rkt:346:0 literal-read-syntax
    
The following error is given, if on line 35 (off by one, see above), an operand is encountered
(starting with `$`, which is ok) not satifying the number format expected.
Expected is a valid operand starting either with `#`, ... or being an integer w/i the given range
BUT no implicit may be given (by the opcode identified, in that case it was a `jsr`)

    ; /home/pe/repo/+1/6510/string:34:12: parse error
    ;   unexpected: $
    ;   expected: '#', '(', ':', 'A', integer in range [$00,$FF], integer in range [$0000,$FFFF], or no implicit
    ; Context:
    ;  /home/pe/repo/+1/6510/6510-reader.rkt:346:0 literal-read-syntax
    

## Structure

The `6510-reader.rkt` is parsing a non racket text file, transforming it into racket code. The resulting racket code is then transformed via
syntax macros into the final racket form which can then be interpreted.

The `6510.rkt` holds all syntax transformation rules for the translation into 6510 byte/assembler code.
* `6510-utils` holds functions needed during syntax and execution phase of 6510.
* `6510-syntax-utils` holds functions useful during syntax phase of the transformation.
* `6510-reader` is the parser that takes the text file and produces racket 6510 dsl code.
* `6510-example` is an example file using arbitrary 6510 text syntax.
* `6510-example-rs` is an example file in racket syntax (no special reader involved)
* `6510-interpreter` holds the (currently minimal) interpreter of the bytecode


## Status

Open 6510-reader.rkt and execute a repl on that file (emacs: C-c C-c).
It provides some info what data is of interest for closer inspection.

To run tests, run `raco test 6510-reader.rkt` for example.

## Todos

### Next

#### implementation without new knowledge
* Warp multitude of flag-methods in 6510-interpreter into simple set of functions working on arbitrary flags, stored in a register, at some
  arbitrary position
* extend pretty print to show in memory location in the first 4 characters (e.g. C000 A9 41    LDA #$41
* extend pretty print to write comments, too
* extend pretty print to write round instead of angular brackets
#### ... with some new knowledge
* Increase test coverage
* Implement macros (as in macro assemblers)
#### ... lots of new concepts
* Document functions and their usage (with scrible? inline?)
  see https://docs.racket-lang.org/scribble/srcdoc.html,
  and https://stackoverflow.com/questions/58981544/docstrings-in-racket
  example https://github.com/racket/gui/blob/master/gui-lib/framework/main.rkt
* Identify concept to carry parser context from reader to racket macros.  
   The basic idea is to have line-number information of the original (parsed) source code
   available at macro expansion time, to provide better error messaging etc.
   This is currently implemented (rather awkwardly I might add) by explicitly passing
   this information from parser to macro.
* Implement multi file assembler programs with references crossing file boundaries  
   This will probably include to export/import symbols.

### Some day
* Extend interpreter to be able to interpret more up to all opcodes
* Write a language to generate parser combination using an extended ebnf syntax, enriched with code, to allow a more compact and less
  verbose definition of the syntax/parser used here.

### Maybe
* Extend interpreter to allow for c64-like output functions when calling rom addresses
* Emulate text mode (fixed charset) of c64
