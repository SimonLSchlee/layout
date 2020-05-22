#lang racket/base

(provide (all-defined-out))

(require "../data.rkt"
         "private/parameter.rkt")

(define-guarded-parameter current-ref-path ref-path? null)

(define (ref/absolute ref)
  (define abs-path (cons ref (current-ref-path)))
  (path->ref abs-path))

(define (current-ref)
  (path->ref (current-ref-path)))

(define (path->ref path)
  (path->join (map symbol->string (reverse path))))
