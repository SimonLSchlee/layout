#lang racket/base

(provide outline)

(require racket/contract
         racket/class
         racket/match
         "../types.rkt"
         "../mode.rkt"
         "../private/drawoutline.rkt")

(define/contract (outline content)
  (-> sizeable/c sizeable/c)
  (λ (size)
    (define piece (apply-wrapper content size))
    (modify piece
            #:draw
            (λ (self pos)
              (ui-draw piece pos)

              (define bounds (ui-bounds self))
              (match-define (vec2 x y) pos)
              (match-define (vec2 w h) (pr2->pos bounds size))
              (draw-outline (current-dc) x y w h)))))
