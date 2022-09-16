# C64 assembler

This project is an example project that works as a basis for experiments with
rackets' capabilities to define (and interpret) languages, in this case: 6510 assembler opcodes

## prerequisites

* racket 7.3+ (`nix-env -i racket`)
* megaparsack (`raco pkg install megaparsack-lib`) 
* threading library (`raco pkg install threading`) 
* pvector (`raco pkg install pvector`)

## usage

### running

You may execute the example program directly:
```sh
  ./6510-example.rkt
```
This will
1. translate the given assembler program into the 6510 codes
2. run a debugger on that code (executing `r` will print 'ABC' five times)
3. and write a `test.d64` disk image, usable by vice (c64 emulator)
(take a look at the commands actually executed by `6510-reader.rkt`)

Another way to run the example is to open `6510-example.rkt` in emacs and start racket via `C-c C-c`.
It will open the debugger which allows for close inspection of the assembled program (hit `?` then `enter` to get help)
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
an unknown opcode (e.g. starting with k in column 8) is given:

    ; .../6510/string:11:8: parse error
    ;   unexpected: k
    ;   expected: end of input
    ; Context:
    ;  .../6510-reader.rkt:346:0 literal-read-syntax

The following error is given, if on line 12 (off by one, see above), column 14 an opcode operand is expected
that may either start with `$` (hex number), `%` (dual number), `(` for indirect addressing operands
`:` for label references, `A` for accumluator operand, an integer for a decimal number operand
BUT no implicit may be given (by the opcode identified, in that case it was a `jsr`)

    ; .../6510/string:11:14: parse error
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

* `6510.rkt` holds all syntax transformation rules for the translation into 6510 byte/assembler code.
* `6510-utils` holds functions needed during syntax and execution phase of 6510.
* `6510-syntax-utils` holds functions useful during syntax phase of the transformation.
* `6510-parser` is the parser that takes the text file and produces racket 6510 dsl code.
* `6510-reader` is used to parse complete files automatically (using the 6510-parser)
* `6510-example` is an example file using arbitrary 6510 text syntax (making use of the 6510-reader).
* `6510-example-rs` is an example file in racket syntax (no special reader involved)
* `6510-interpreter` holds the interpreter of the bytecode
* `6510-disassembler` allows to produce source code from bytes
* `6510-debugger` allows stepwise execution of code


## Status

To run tests, run `raco test -y --drdr 6510-parser.rkt` for example.

To run all tests, run `raco test -y --drdr -x -j 8 .`

To build all, run `raco make -v -j 8 *.rkt`

## Todos

### Next

#### debugger

idea: use same commands as vice monitor (see https://vice-emu.sourceforge.io/vice_12.html)
- (g)oto <address> :: goto / jump to the given address
- (n)ext <count> :: execute the next <count> number of instructions
- r <reg> = <value> :: assign a value to a register (pc, sp, a, x, y)
- reset <type> :: reset the cpu (0 = soft, 1 = hard ...)
- (ret)urn :: execute up to (including) the next rts/rti
- (z) / step <count> :: same as next
- (c)ompare <address> .. <address> <address> :: compare memory
- (f)ill <address> .. <address> <byte> (, <byte>)* :: fill the address range with bytes, repeating
- (h)unt <address> .. <address> <byte> (, <byte>)* :: find the given byte sequence in the address range
- i <address> .. <address> :: display memory as PETSCII text
- ii <address> .. <address> :: display memory as screen code text
- (m)em <address> .. <address> display memory
- (t)/move <address> .. <address> <address> :: move/copy memory
- > <address> <byte> (, <byte>)* :: write into address
- a <address> <instruction> (, <instruction>)* :: assemble
- (d)isass <address> .. <address> :: disassemble
- break (load|store|exec) (<address> (.. <address>)? (if <condition>)?)? :: (list or) set break point, when loading/storing or executing (within) the given address, when condition hits
- enable <breakpoint-id>
- disable <breakpoint-id>
- cond <breakpoint-id> if <condition> :: set condition on breakpoint
- (del)ete <breakpoint-id> :: remove the breakpoint
- ignore <breakpoint-id> (<count>)? :: ignore the given breakpoint for count times
- (tr)ace (load|store|exec) (<address> (.. <address>)? (if <condition>)?)? :: (list or) set trace point, when loading/storing or executing (within) the given address, when condition hits
- (un)til <address> :: create temporary breakpoint at address and run until there
- (w)atch (load|store) (<address> (.. <address>)? (if <condition>)?)? :: (list or) set watch point, when loading/storing or executing (within) the given address, when condition hits
- e(x)it :: exit monitor and return to execution
- (q)uit :: exit monitor
- help <command> :: print help
- ~ <number> : display number in decimal, hex, octal and binary

 break point = stop in monitor / debugger on read or write or execute
 trace point = print status but continue running on read or write or execute
 watch point = stop into monitor / debugger on read or write
 
* step-wise debugger
** [X] step
** [X] inspect/change state
** [X] continue
** [ ] continue to next rts/rti
** [X] back
** [ ] back to previous jmp/jsr interrupt
** Inspect/change stack
** inspect/change memory
** [X] set additional break points
* break points
** [X] static break point (on pc)
** conditional break points 
*** conditional elements ::
    - hit#,
    - memory content comparison,
    - flag test,
    - stack height (sp),
    - stack value comparison,
    - pc value (code line),
    - current opcode (set),
    - current addressing mode
** [X] time travel debugging (backwards)
** time portal (points) :: spots to remember state and navigate to (later)

#### mil (minimal lisp)

* [X] extend interpreter to run all (documented) opcodes
* extend interpreter to load/link and run several modules
* extend interpreter to breakpoints/debugging/timetravel etc.
* keep a clean bootstrapping process in mind
* define lisp concepts to initially support
  - 1st be able to interpret itself
  - 2nd be able compile itself
  - minimal operations
    - operations/definitions: def, car, cdr, list, quote, quasi-quote, unquote, 
  - elements
    - byte, nil, string, symbol, operation
  - map symbol -> code
  
* manually translate source -> byte coded ast
* implement reader (string -> ast)
  - ( a . b ) :: pair
  - ( a . ( b . ( c . nil ) ) :: (list a b c)
  - `( ... ) :: (quasi-quote ... )
  - ,( ... ) :: (unquote ...)
  - '( ... ) :: (quote ... )
  data structure pair:
    encoding
      - 0 = nil
* write lisp interpreter in (c/6510 code)
* define lisp extension to allow compilation
  - this lisp should then be used to compile itself to assembly
* define lisp extensions to incrementally support
  - hygenic macros

#### implementation without new knowledge
* wrap multitude of flag-methods in 6510-interpreter into simple set of functions working on arbitrary flags, stored in a register, at some
  arbitrary position
* extend pretty print to write comments, too
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
* Write a language to generate parser combination using an extended ebnf syntax, enriched with code, to allow a more compact and less
  verbose definition of the syntax/parser used here.

### Maybe
* Extend interpreter to allow for c64-like output functions when calling rom addresses
* [-] Emulate text mode (fixed charset) of c64
