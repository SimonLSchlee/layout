#lang racket/base

(require racket/contract
         racket/match
         "../pixelratio.rkt"
         "piece.rkt"
         "bounds.rkt")

(provide axis/c
         (contract-out [struct spacer ((amount pixelratio?))])
         spacer->sizeable)

(define axis/c (or/c 'h 'v))
(struct spacer (amount) #:extra-constructor-name make-spacer)

(define (spacer->sizeable spacer axis)
  (λ (size) (spacer->piece spacer axis)))

(define/contract (spacer->piece spacer axis)
  (-> spacer? axis/c any/c)
  (modify nothing
          #:bounds (spacer->bounds spacer axis)))

(define (spacer->bounds spacer axis)
  (match axis
    ['h (bounds (spacer-amount spacer) (ratio 1))]
    ['v (bounds (ratio 1) (spacer-amount spacer))]))
