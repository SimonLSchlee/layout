#lang racket/base

(provide anchor-point?)

(require syntax/parse/define
         (for-syntax racket/base)
         "direction.rkt")

(define-syntax-rule (provide/define-anchor-points table [name x y] ...)
  (begin
    (provide name) ...
    (define name  (dir x y)) ...
    (provide table)
    (define table (hasheq (~@ (quote name) name) ...))))

;; ↖ ↑ ↗
;; ← • →
;; ↙ ↓ ↘
(provide/define-anchor-points anchor-points
                              [↖ -1 -1] [↑  0 -1] [↗  1 -1]
                              [← -1  0] [•  0  0] [→  1  0]
                              [↙ -1  1] [↓  0  1] [↘  1  1])

(define (anchor-point? ap)
  (hash-ref anchor-points ap #f))
