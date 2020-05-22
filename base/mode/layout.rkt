#lang racket/base

(provide layout-thk/c
         layout-apply)

(require racket/contract
         racket/class
         "../types.rkt"
         "current.rkt"
         "wrap.rkt")

(define layout-thk/c (-> any/c))

(define (layout-apply page-layout dc data)
  (define layout (calculate-layout dc data page-layout))

  (define-values (width height) (send dc get-size))
  (define size (vec2 width height))
  (parameterize ([current-dc dc])
    (define piece (apply-wrapper layout size))
    (ui-draw piece vec2-zero))
  (void))

(define (calculate-layout dc data layout)
  (parameterize ([current-dc   dc]
                 [current-data data])
    (layout)))
