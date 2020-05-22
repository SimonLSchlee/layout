#lang racket/base

(provide symbol
         variant/false)

(require syntax/parse
         racket/list
         racket/format)

(define (quoted-symbol? v)
  (and (list? v)
       (eq? 'quote (syntax-e (first v)))
       (symbol? (syntax-e (second v)))))

(define-syntax-class (symbol)
  (pattern s #:when (quoted-symbol? (syntax-e #'s))))

(define (variant/false stx)
  (define name (string->symbol (format "~a/f" (syntax-e stx))))
  (datum->syntax stx name stx))
