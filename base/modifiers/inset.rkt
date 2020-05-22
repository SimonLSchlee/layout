#lang racket/base

(provide inset)

(require racket/contract
         "../types.rkt"
         "../mode.rkt")

(define/contract (inset offset content)
  (-> pr2? sizeable/c sizeable/c)
  (define twice (pr2* offset pr2-two))
  (λ (size)
    (define inner-size (pr2->pos (pr2- (pos->pr2 size) twice)
                                 size))
    (define piece (apply-wrapper content inner-size))
    (container (λ (self)
                 (pr2+ twice (ui-bounds piece)))
               (λ (self pos callback)
                 (callback piece (pos+ pos (pr2->pos offset size)))))))

(module+ test
  (require rackunit/chk
           "../primitives/rect.rkt")

  (define size (pr2 (pixel 100) (pixel 100)))

  (define (test expected content)
    (define offset (pr2 (pixel 10) (pixel 10)))
    (define layout (inset offset content))
    (define piece  (layout size))
    (define bounds (ui-bounds piece))
    (chk #t (pr2= expected bounds)))

  (test (pr2 (pixel 60) (pixel 60))   (rect (pixel 40) (pixel 40)))

  (define (s v)
    (pr2 v v))
  (test (s (pixelratio 20 1))   (rect (ratio 1) (ratio 1))))

