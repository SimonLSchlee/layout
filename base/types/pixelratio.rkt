#lang racket/base

(require racket/contract
         racket/match)

(provide (contract-out [struct pixelratio ([pixel real?] [ratio real?])]
                       [pixel      (-> real? pixelratio?)]
                       [ratio      (-> real? pixelratio?)]
                       [scalar->pr (-> real? pixelratio?)]
                       [pr->pixel  (-> pixelratio? real? real?)])
         (rename-out [pixelratio? pr?])
         pr-0 pr-1 pr-2)

;; pixel + ratio * x
;; a small algebra for calculating pixels and ratios
(struct pixelratio (pixel ratio) #:transparent)
(define (pixel p) (pixelratio p 0))
(define (ratio r) (pixelratio 0 r))

(define (scalar->pr scalar)
  (pixelratio scalar scalar))

(define (pr->pixel pr x)
  (match-define (pixelratio p r) pr)
  (+ p (* r x)))

(define pr-0 (scalar->pr 0))
(define pr-1 (scalar->pr 1))
(define pr-2 (scalar->pr 2))

(define-syntax-rule (provide/define-op name operation accum)
  (begin
    (provide (contract-out [name (-> pixelratio? pixelratio? any/c)]))
    (define (name a b)
      (match* (a b)
        [{(pixelratio p1 r1) (pixelratio p2 r2)}
         (accum (operation p1 p2) (operation r1 r2))]))))

(provide/define-op pr+ + pixelratio)
(provide/define-op pr- - pixelratio)
(provide/define-op pr* * pixelratio)
(provide/define-op pr/ / pixelratio)

(provide/define-op pr=  =  and)
(provide/define-op pr<  <  and)
(provide/define-op pr<= <= and)
(provide/define-op pr>  >  and)
(provide/define-op pr>= >= and)

(provide/define-op pr-or-=  =  or)
(provide/define-op pr-or-<  <  or)
(provide/define-op pr-or-<= <= or)
(provide/define-op pr-or->  >  or)
(provide/define-op pr-or->= >= or)


(module+ test
  (require rackunit/chk)

  (chk (pixel 20) (pr+ (pixel 10)  (pixel 10)))
  (chk (ratio 1)  (pr+ (ratio 1/2) (ratio 1/2)))

  (chk (pixelratio 10  1/2)  (pr+ (pixel 10) (ratio 1/2)))
  (chk (pixelratio 10 -1/2)  (pr- (pixel 10) (ratio 1/2)))

  (chk (pixel 12)       (pr* (pixel 3)   (pixel 4)))
  (chk (ratio 1/4)      (pr* (ratio 1/2) (ratio 1/2)))
  (chk (pixelratio 0 0) (pr* (pixel 3)   (ratio 1/2)))

  (chk #t (pr= (pixel 10) (pixel 10)))
  (chk #t (pr= (ratio 1)  (ratio 1)))
  (chk #t (pr= (pixelratio 10 1)  (pixelratio 10 1)))
  (chk #f (pr= (pixel 1) (ratio 1)))

  (chk #f (pr<     (pixel 10) (pixel 20)))
  (chk #t (pr-or-< (pixel 10) (pixel 20)))

  )
