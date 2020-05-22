#lang racket/base

(provide debug-wrapper)

(require racket/draw
         racket/match
         racket/class
         "../types.rkt"
         "../private/drawoutline.rkt"
         "wrap.rkt"
         "current.rkt")

(define (debug-wrapper s)
  (λ (size)
    (define piece (s size))
    (modify piece
            #:draw
            (λ (self pos)
              (when (page-started)
                (define dc (current-dc))
                (match-define (vec2 x y) pos)

                (match-define (vec2 sw sh) size)
                (draw-outline dc x y sw sh size-color 'dot)

                (match-define (vec2 w h) (pr2->pos (ui-bounds self) size))
                (draw-outline dc x y w h bounds-color 'solid))

              (ui-draw piece pos)))))

(define size-color   (make-color 220 0 0))
(define bounds-color (make-color 0 0 0))
