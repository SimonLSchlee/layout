#lang racket/base
(provide debug)

(require racket/format)

(define-syntax-rule (debug a ...)
  (begin (displayln (~a (quote a) ": " a)) ...))
