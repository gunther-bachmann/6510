#lang racket/base

#|

 page type for holding long texts

 a text is organized as a list of text pages.
 each text page holds a number of text sections (which follow directly on one another)
 text is held

 | offset | description             |
 |--------+-------------------------|
 |     00 | page type = 0011 0000   |
 |     01 | offset to start of text |
 |     02 | offset to end of text   |
 |        | ...                     |
 |     ff | next page               |


 text section w/i text page, allow for lines spanning over pages

 | offset | description                              |
 |--------+------------------------------------------|
 |     00 | number of characters                     |
 |     01 | indentation level + LF Flag [fiii  iiii] |
 |        | n chars                                  |


 operations:
   get offset to text section of line #n from start 0 = first section
   get # of lines on this page

   write text of line t_y col t_x to screen s_x s_y, width w (space for non existent)
   - spaces before because of indent: indent - t_x       (only if t_x < indent)
   - offset into a: t_x - indent (only if t_x >= indent and t_x < indent + n_a)
            #chars to write from a
   - offset into b: (only if f_a = 0 and ...)
            #chars to write from b
   - spaces after:  if f_a = 0: indent - t_x + n_a + n_b (only positive values are relevant)
                    if f_a = 1: indent - t_x + n_a       (only positive values are relevant)

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+-----+-----+---+-----+    t_x < indent
            | indent |aaaaa|bbbbb| spaces  |    indent - t_x + n_a + n_b < width (spaces needed at end)
            +---+----+-----+-----+---------+    f_a = 0 (no line feed in a)
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----------+-----+---+-----+    t_x > indent
            |aaaaaaaaaaaaaa|bbbbb| spaces  |    indent + n_a - t_x + n_b < width (spaces needed at end)
            +---+----------+-----+---------+    f_a = 0 (no line feed in a)
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----------------+---+-----+    t_x > indent + n_a
            |bbbbbbbbbbbbbbbbbbbb| spaces  |    indent + n_a - t_x + n_b < width (spaces needed at end)
            +---+----------------+---------+    f_a = 0 (no line feed in a)
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+--------------------+-----+    t_x > indent + n_a
            |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|    indent + n_a - t_x + n_b >= width (no spaces needed at end)
            +---+--------------------------+    f_a = 0 (no line feed in a)
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+---------+-----+--+       t_x < indent
            | indent |aaaaaaaaa|bbbbbbbb|       indent + n_a + n_b - t_x >= width (no spaces needed)
            +---+----+---------+--------+       indent + n_a - t_x < width (b is needed!)
                |                               f_a = 0 (no line feed in a)
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+--------------+-----+--+       t_x > indent
            |aaaaaaaaaaaaaaaaaa|bbbbbbbb|       indent + n_a + n_b - t_x >= width (no spaces needed)
            +---+--------------+--------+       indent + n_a - t_x < width (b is needed)
                |                               f_a = 0 (no line feed in a)
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+-----------+---+-----+    t_x < indent
            | indent |aaaaaaaaaaa| spaces  |    indent + n_a - t_x < width (spaces needed at end)
            +---+----+-----------+---------+    f_a = 1 (line feed in a)
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----------------+---+-----+    t_x > indent
            |aaaaaaaaaaaaaaaaaaaa| spaces  |    indent + n_a - t_x < width (spaces needed at end)
            +---+----------------+---------+    f_a = 1 (line feed in a)
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+---------------+-----+    t_x < indent
            | indent |aaaaaaaaaaaaaaaaaaaaa|    indent + n_a - t_x > width (no spaces needed at end)
            +---+----+---------------------+    f_a = irrelevant
                |
               t_x

               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+--------------------+-----+    t_x >= indent
            |aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|    indent + n_a - t_x > width (no spaces needed at end)
            +---+--------------------------+    f_a = irrelevant
                |
               t_x

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol))

(module+ test #| require |#
  (require (only-in racket/string
                    string-replace)
           (only-in uuid
                    uuid-string)
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    estimated-code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    cpu-state-clock-cycles
                    memory-list)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    VM_MEMORY_MANAGEMENT_CONSTANTS
                    ZP_RP))

  (define test-runtime
    (append

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))
