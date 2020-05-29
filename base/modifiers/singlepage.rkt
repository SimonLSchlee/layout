#lang racket/base

(provide single-page)

(require racket/contract
         racket/match
         racket/class
         "../types.rkt"
         "../mode.rkt")

(define (single-page content)
  (λ (size)
    (define piece (apply-wrapper content size))
    (modify piece
            #:bounds
            (λ (self)
              (match-define (vec2 width height) size)
              (bounds (pixel width) (pixel height)))
            #:draw
            (λ (self pos)
              (define dc (current-dc))
              (send dc start-page)
              (parameterize ([page-started #t])
                (ui-draw piece pos))
              (send dc end-page)))))
