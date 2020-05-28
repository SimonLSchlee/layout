#lang racket/base

(provide (all-defined-out))

(require racket/contract
         "../private/parameter.rkt"
         "generator.rkt")

(define mode/c (or/c 'ref 'generate))
(define-guarded-parameter current-mode mode/c 'ref)
