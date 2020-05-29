#lang racket/base

(provide horizontal-line vertical-line)

(require racket/draw
         racket/class
         racket/match
         "../types.rkt"
         "../mode.rkt")

(define (pen-width dc)
  (define pen (send dc get-pen))
  (send pen get-width))

(define horizontal-line
  (λ (size)
    (leaf (λ (self)
            (bounds (ratio 1) (pixel (pen-width (current-dc)))))
          (λ (self pos)
            (define dc (current-dc))
            (match-define (vec2 x y) pos)
            (match-define (vec2 w h) (pr2->pos (ui-bounds self) size))
            (define pw (pen-width dc))
            (define x2 (+ x (max 0 (- w pw))))
            (define y2 (+ y (/ pw 2)))
            (send dc draw-line x y2 x2 y2)))))

(define vertical-line
  (λ (size)
    (leaf (λ (self)
            (bounds (pixel (pen-width (current-dc))) (ratio 1)))
          (λ (self pos)
            (define dc (current-dc))
            (match-define (vec2 x y) pos)
            (match-define (vec2 w h) (pr2->pos (ui-bounds self) size))
            (define pw (pen-width dc))
            (define x2 (+ x (/ pw 2)))
            (define y2 (+ y (max 0 (- h pw))))
            (send dc draw-line x2 y x2 y2)))))


(module+ main
  (require layout/interactive
           "../modifiers/style.rkt")

  (define thick (style #:pen (get-pen (make-color 120 120 0) 8 'solid)))
  (interact-gui (vec2 800 800) (thick horizontal-line))
  (interact-gui (vec2 800 800) (thick vertical-line)))
