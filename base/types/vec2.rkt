#lang racket/base

(provide (struct-out vec2)
         vec2-zero vec2-two)

(require racket/match
         racket/contract)

(struct vec2 (x y) #:transparent)
(define vec2-zero (vec2 0 0))
(define vec2-two  (vec2 2 2))

