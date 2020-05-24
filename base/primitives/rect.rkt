#lang racket/base

(provide rect)

(require "../types.rkt")

(define (rect width height)
  (λ (size)
    (modify #:bounds
            (λ (self)
              (bounds width height)))))
