#lang racket/base

(provide resolve-reference
         use)

(require racket/contract
         racket/list
         racket/match
         syntax/parse/define
         "current.rkt"
         "reference.rkt"
         "mode.rkt"
         "../data.rkt")

(define (err ref)
  (error 'resolve-reference
         "could not resolve reference: ~a"
         (ref/absolute ref)))

(define/contract (resolve-reference ref [fail (λ () (err ref))])
  (->* (pathish?) ((-> any/c)) any/c)
  (define (resolve current path return)
    (if (empty? path)
        current
        (let ([f (first path)]
              [r (rest path)])
          (unless (hash? current)
            (return (fail)))
          (define val (hash-ref current f (λ () (return (fail)))))
          (resolve val r return))))

  (define path (pathish->path ref))
  (let/ec return
    (resolve (current-data) path return)))


(define-simple-macro (use reference content)
  (use/impl reference (λ () content)))

(define (use/impl reference body-proc)
  (define (impl op)
    (parameterize ([current-data     (op reference)]
                   [current-ref-path (cons reference (current-ref-path))])
      (body-proc)))
  (match (current-mode)
    ['ref      (impl resolve-reference)]
    ['generate (impl resolve-reference/generate)]))

;; TODO generate key
(define (resolve-reference/generate ref)
  (resolve-reference ref (λ () #f)))
