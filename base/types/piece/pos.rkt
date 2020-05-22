#lang racket/base

(require racket/contract
         "../vec2.rkt"
         "../vec2-variant.rkt"
         "../pixelratio.rkt"
         "../pixelratiovec2.rkt")

(provide (contract-out [pos->pr2 (-> pos? pr2?)]))

(define position/c real?)
(provide/define-vec2-variant position/c pos pos?)

(define-vec2-op-provide/definer provide/define-pos-op pos?)

(provide/define-pos-op pos+ + vec2)
(provide/define-pos-op pos- - vec2)
(provide/define-pos-op pos* * vec2)
(provide/define-pos-op pos/ / vec2)

(define (pos->pr2 pos)
  (pr2 (pixel (vec2-x pos))
       (pixel (vec2-y pos))))
