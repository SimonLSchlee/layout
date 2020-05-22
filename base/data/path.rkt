#lang racket/base

(require racket/contract
         racket/string)

(provide path/c
         (contract-out
          [path->split (-> symbol? path/c)]
          [path->join  (-> path/c symbol?)]))

(define path-values/c (or/c symbol? number?))
(define path/c        (listof path-values/c))

(define (string->path-val v)
  (define number (string->number v))
  (if number number (string->symbol v)))

(define (path-val->string p)
  (if (number? p)
      (number->string p)
      (symbol->string p)))

(define (path->split ref)
  (map string->path-val (string-split (symbol->string ref) ".")))

(define (path->join path)
  (string->symbol (string-join (map path-val->string path) ".")))
