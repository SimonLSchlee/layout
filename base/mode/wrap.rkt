#lang racket/base

(require racket/contract
         "../types.rkt"
         "private/parameter.rkt")

(provide wrapper/c
         no-wrap
         current-wrapper

         (contract-out
          [wrap          wrapper/c]
          [apply-wrapper (-> sizeable/c pos? piece?)]))


(define wrapper/c (-> sizeable/c sizeable/c))
(define (no-wrap x) x)
(define-named-parameter current-wrapper no-wrap)

(define (wrap s)
  ((current-wrapper) s))

(define (apply-wrapper s size)
  (define wrapped (wrap s))
  (wrapped size))
