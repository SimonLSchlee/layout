#lang racket/base

(provide (all-defined-out))

(require racket/contract
         "../data.rkt"
         "../private/parameter.rkt")

(define value-generator/c (-> ref/kinds? reference? ref/kind/c))
(define-named-parameter current-value-generator #f)

(define (apply-generator kinds ref)
  (define generator (current-value-generator))
  (unless generator
    (error 'apply-generator "current-value-generator is #f, you need to set a value generator for mode 'generate"))
  (generator kinds ref))
