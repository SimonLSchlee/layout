#lang racket/base

(provide inset)

(require racket/contract
         racket/match
         "../types.rkt"
         "../mode.rkt")

(define/contract (inset content
                        #:size   [size   pr2-zero]
                        #:left   [left   pr-0]
                        #:right  [right  pr-0]
                        #:top    [top    pr-0]
                        #:bottom [bottom pr-0])
  (->* (sizeable/c)
       (#:size   pr2?
        #:left   pr?
        #:right  pr?
        #:top    pr?
        #:bottom pr?)
       sizeable/c)
  (match-define (vec2 h v) size)
  (inset/impl (pr+ left h) (pr+ right h) (pr+ top v) (pr+ bottom v) content))

(define (inset/impl left right top bottom content)
  (define lt (pr2 left  top))
  (define rb (pr2 right bottom))
  (λ (size)
    (define inner-size (pr2->pos (pr2- (pr2- (pos->pr2 size) lt) rb)
                                 size))
    (define piece (apply-wrapper content inner-size))
    (container (λ (self)
                 (pr2+ (pr2+ lt (ui-bounds piece)) rb))
               (λ (self pos callback)
                 (callback piece (pos+ pos (pr2->pos lt size)))))))

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

