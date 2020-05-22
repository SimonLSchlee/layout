#lang racket/base

(require racket/contract
         racket/match
         "../data.rkt"
         "mode.rkt"
         "resolve.rkt"
         "generator.rkt"
         "patch.rkt")

(define (ref/kinds kinds val/ref)
  (match val/ref
    [(? reference? ref) (apply-mode kinds ref)]
    [(? ref/kind/c val) val]))

(define (apply-mode kinds ref)
  (define mode (current-mode))
  (match mode
    ['ref      (resolve-reference ref)]
    ['generate (generate kinds ref)]))

(define (generate kinds ref)
  (define (generate-data)
    (define value     (apply-generator kinds ref))
    (patched ref value)
    value)

  (resolve-reference ref generate-data))

(require syntax/parse/define
         (for-syntax racket/base
                     "private/syntax.rkt"))

(define-simple-macro (provide/define-ref-op name:id kind:symbol ...)
  (begin
    (provide (contract-out [name (-> ref/c ref/kind/c)]))
    (define (name val/ref)
      (ref/kinds (vector kind ...)
                 val/ref))))

(define-syntax (provide/define-ref-variants stx)
  (syntax-parse stx
    [(_ name:id kind:symbol ...)
     (with-syntax ([name/f (variant/false #'name)])
       #'(begin
           (provide/define-ref-op name   kind ...)
           (provide/define-ref-op name/f kind ... 'bool)))]))

(define-simple-macro (provide/define-ref-ops [name:id kind:symbol ...] ...)
  (begin (provide/define-ref-variants name kind ...) ...))


(provide/define-ref-ops
 [ref/string      'string]
 [ref/real        'real]
 [ref/bool        'bool]
 [ref/list        'list]
 [ref/url/picture 'url/picture])
