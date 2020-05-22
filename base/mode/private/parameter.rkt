#lang racket/base

(provide define-named-parameter
         define-guarded-parameter)

(require racket/contract
         racket/string)

(define/contract (named-parameter name [init #f])
  (->* (symbol?) (any/c) any/c)
  (define name-str        (symbol->string name))
  (define parameter-name  (string-append "parameter-" name-str))
  (make-parameter init #f (string->symbol parameter-name)))

(define/contract (guarded-parameter name predicate? [init #f])
  (->* (symbol? (or/c (-> any/c any/c))) (any/c) any/c)
  (define name-str        (symbol->string name))
  (define parameter-name  (string-append "parameter-" name-str))
  (define guard-error     (string->symbol (string-append parameter-name "-guard")))
  (define (guard val)
    (if (predicate? val)
        val
        (error guard-error "called ~a with invalid value: ~a" name-str val)))
  (make-parameter init guard (string->symbol parameter-name)))

(define-syntax-rule (define-guarded-parameter name args ...)
  (define name (guarded-parameter (quote name) args ...)))

(define-syntax-rule (define-named-parameter name args ...)
  (define name (named-parameter (quote name) args ...)))
