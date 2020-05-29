#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../data.rkt"
         "../private/parameter.rkt"
         "reference.rkt")

(define patch-accumulator/c (-> reference? ref/kind/c any/c))

(provide current-patch
         changes/c
         (contract-out
          [struct change ([ref   reference?]
                          [value ref/kind/c])]
          [patched patch-accumulator/c]
          [apply-patch (-> data/c changes/c data/c)]))

(struct change    (ref value) #:transparent)
(define changes/c (listof change?))
(define-guarded-parameter current-patch changes/c null)


(define (patched ref value)
  (define abs-ref (ref/absolute ref))
  (current-patch (cons (change abs-ref value)
                       (current-patch))))

(define (apply-patch data patch)
  (define (apply-change data ref value)
    (define (update path current new)
      (define f (first path))
      (define r (rest path))
      (if (empty? r)
          (match current
            [(? hash?) (hash-set current f new)]
            [(? list?) (list-set current f new)])
          (match f
            [(? symbol? s)
             (hash-update current s
                          (λ (old)
                            (update r old new))
                          hash)]
            [(? path-index? i)
             (define len (length current))
             (define old
               (if (i . < . len)
                   (list-ref current i)
                   (raise "apply-patch auto generating new list elements is not implemented")))
             (list-set current i (update r old new))])))

    (define path (ref->path ref))
    (update path data value))

  (for/fold ([data data])
            ([c (in-list (reverse patch))])
    (match-define (change ref value) c)
    (apply-change data ref value)))
