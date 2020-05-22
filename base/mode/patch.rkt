#lang racket/base

(require racket/contract
         "../data.rkt"
         "private/parameter.rkt"
         "reference.rkt")

(define patch-accumulator/c (-> reference? ref/kind/c any/c))

(provide (contract-out
          [patched patch-accumulator/c])
         current-patch)

(struct change        (path value) #:prefab)
(define changes/c     (listof change?))
(define-guarded-parameter current-patch changes/c null)


(define (patched ref value)
  (define abs-ref (ref/absolute ref))
  (current-patch (cons (change abs-ref value)
                       (current-patch))))

(define (apply-patch data patch)
  (raise "impl apply-patch"))
