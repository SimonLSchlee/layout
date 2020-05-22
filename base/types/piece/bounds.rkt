#lang racket/base

(require racket/contract
         "../pixelratio.rkt"
         "../pixelratiovec2.rkt"
         "../vec2.rkt"
         "pos.rkt")

(provide (rename-out [pr2  bounds]
                     [pr2? bounds?])
         (contract-out [pr2->pos (-> pr2? pos? pos?)]))


(define (pr2->pos pr2 size)
  (pos (pr->pixel (vec2-x pr2) (vec2-x size))
       (pr->pixel (vec2-y pr2) (vec2-y size))))
