#lang racket/base

(require racket/contract
         "../pixelratio.rkt"
         "../vec2.rkt"
         "pos.rkt"
         "bounds.rkt"
         "piece.rkt")

(provide size?
         (contract-out
          [piece-size (-> piece? size? size?)]
          [size->bounds (-> size? bounds?)]))

(define size?       pos?)

(define (piece-size piece max-size)
  (define bounds (ui-bounds piece))
  (define size   (pr2->pos bounds max-size))
  size)

(define (size->bounds size)
  (bounds (pixel (vec2-x size))
          (pixel (vec2-y size))))
