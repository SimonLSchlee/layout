#lang racket/base

(require racket/contract
         "../pixelratio.rkt"
         "pos.rkt"
         "bounds.rkt"
         "piece.rkt"
         "spacer.rkt")

(provide sizeable/c
         sizeables/c
         element/c
         elements/c

         (contract-out
          [element->sizeable   (-> element/c  axis/c sizeable/c)]
          [elements->sizeables (-> elements/c axis/c sizeables/c)]))

;; maxbounds -> piece
(define sizeable/c  (-> pos? piece?))
(define sizeables/c (listof sizeable/c))
(define element/c   (or/c spacer? sizeable/c))
(define elements/c  (listof element/c))

(define (element->sizeable element axis)
  (if (spacer? element)
      (spacer->sizeable element axis)
      element))

(define (elements->sizeables elements axis)
  (map (λ (e) (element->sizeable e axis)) elements))
