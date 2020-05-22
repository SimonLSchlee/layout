#lang racket/base

(provide draw-outline)

(require racket/draw
         racket/class)


(define invisible    (make-color 0 0 0 0))
(define (draw-outline dc x y w h color style)
  (define pen   (send dc get-pen))
  (define brush (send dc get-brush))
  (send dc set-brush invisible 'solid)
  (send dc set-pen color 2 style)
  (send dc draw-rectangle x y w h)
  (send dc set-pen pen)
  (send dc set-brush brush))
