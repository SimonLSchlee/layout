#lang racket/base

(provide draw-outline
         draw-outline/color+style)

(require racket/class
         "color.rkt")


(define (draw-outline dc x y w h)
  (define brush (send dc get-brush))
  (send dc set-brush invisible 'solid)
  (send dc draw-rectangle x y w h)
  (send dc set-brush brush))

(define (draw-outline/color+style dc x y w h color style)
  (define pen   (send dc get-pen))
  (define brush (send dc get-brush))
  (send dc set-brush invisible 'solid)
  (send dc set-pen color 1 style)
  (send dc draw-rectangle x y w h)
  (send dc set-pen pen)
  (send dc set-brush brush))
