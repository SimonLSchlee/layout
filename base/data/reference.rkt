#lang racket/base

(provide (all-defined-out))

(require racket/contract)

(define reference? symbol?)
(define ref/kind?  (or/c 'string 'real 'boolean 'list 'url/picture))
(define ref/kind/c (or/c string? real? boolean? list?))
(define ref/c      (or/c reference? ref/kind/c))
(define ref/kinds? (vectorof ref/kind?))
(define ref-op/c   (-> ref/kinds? ref/c ref/kind/c))

(define string/ref? (or/c reference? string?))

