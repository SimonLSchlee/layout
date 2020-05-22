#lang racket/base

(provide dc/c data/c)

(require json
         (only-in racket/draw dc<%>)
         (only-in racket/class is-a?/c))

(define dc/c   (is-a?/c dc<%>))
(define data/c jsexpr?)

