#lang racket/base

(provide (all-defined-out))

(require "../data.rkt"
         "private/parameter.rkt")

(define-guarded-parameter current-dc   dc/c     #f)
(define-guarded-parameter current-data data/c   #f)
(define-guarded-parameter page-started boolean? #f)
