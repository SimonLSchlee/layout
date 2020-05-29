#lang racket/base

(provide (all-defined-out))

(require "../data.rkt"
         "../private/parameter.rkt")

(define-guarded-parameter current-ref-path ref-path? null)

(define (ref/absolute ref)
  (define abs-path (cons ref (current-ref-path)))
  (reverse-path->ref abs-path))

(define (current-ref)
  (reverse-path->ref (current-ref-path)))

(define (reverse-path->ref path)
  (path->ref (reverse path)))
