#lang racket/base

(provide outline)

(require racket/contract
         racket/draw
         racket/class
         racket/match
         "../types.rkt"
         "../mode.rkt"
         "../private/drawoutline.rkt")

(define color/c (is-a?/c color%))
(define black   (make-color 0 0 0))

(define/contract (outline content
                          #:color [color black]
                          #:style [style 'solid])
  (->* (sizeable/c) (#:color color/c #:style symbol?) sizeable/c)
  (λ (size)
    (define piece (apply-wrapper content size))
    (modify piece
            #:draw
            (λ (self pos)
              (define dc (current-dc))
              (match-define (vec2 x y) pos)
              (match-define (vec2 w h) (pr2->pos (ui-bounds self) size))
              (draw-outline dc x y w h color style)
              (ui-draw piece pos)))))
