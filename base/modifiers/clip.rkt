#lang racket/base

(provide clip
         clip-piece)

(require racket/contract
         racket/draw
         racket/class
         racket/match
         "../types.rkt"
         "../mode.rkt")

(define/contract (clip content)
  (-> sizeable/c sizeable/c)
  (λ (size)
    (define piece (apply-wrapper content size))
    (clip-piece piece size)))

(define (clip-piece piece size)
  (modify piece
          #:bounds
          (λ (self) (size->bounds size))
          #:draw
          (λ (self pos)
            (define dc (current-dc))
            (match-define (vec2 x y) pos)
            (match-define (vec2 w h) size)

            (define old (send dc get-clipping-region))
            (send dc set-clipping-rect x y w h)
            (ui-draw piece pos)
            (send dc set-clipping-region old))))
