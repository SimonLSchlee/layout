#lang racket/base

(provide rect)

(require "../types.rkt")

(define (rect width height)
  (λ (size)
    (modify nothing
            #:bounds
            (λ (self)
              (bounds width height)))))
