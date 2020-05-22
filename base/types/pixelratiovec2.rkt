#lang racket/base

(require racket/contract
         racket/match
         "pixelratio.rkt"
         "vec2-variant.rkt"
         "vec2.rkt")

(provide pr2-zero pr2-one pr2-two
         full-size
         (contract-out
          [pr->pr2     (-> pixelratio? pr2?)]
          [scalar->pr2 (-> real? pr2?)]))

(provide/define-vec2-variant pixelratio? pr2 pr2?)

(define pr2-zero (pr2 pr-0 pr-0))
(define pr2-one  (pr2 pr-1 pr-1))
(define pr2-two  (pr2 pr-2 pr-2))

(define full-size (pr2 (ratio 1) (ratio 1)))

(define (pr->pr2 pr)
  (pr2 pr pr))

(define (scalar->pr2 scalar)
  (define s (scalar->pr scalar))
  (pr2 s s))

(define-vec2-op-provide/definer provide/define-pr2-op pr2?)

(provide/define-pr2-op pr2+ pr+ vec2)
(provide/define-pr2-op pr2- pr- vec2)
(provide/define-pr2-op pr2* pr* vec2)
(provide/define-pr2-op pr2/ pr/ vec2)

(provide/define-pr2-op pr2=  pr=  and)
(provide/define-pr2-op pr2<  pr<  and)
(provide/define-pr2-op pr2<= pr<= and)
(provide/define-pr2-op pr2>  pr>  and)
(provide/define-pr2-op pr2>= pr>= and)

(provide/define-pr2-op pr2-or-=  pr-or-=  or)
(provide/define-pr2-op pr2-or-<  pr-or-<  or)
(provide/define-pr2-op pr2-or-<= pr-or-<= or)
(provide/define-pr2-op pr2-or->  pr-or->  or)
(provide/define-pr2-op pr2-or->= pr-or->= or)
