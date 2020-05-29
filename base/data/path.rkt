#lang racket/base

(require racket/contract
         racket/string)

(provide path-index?
         path-value?
         path/c
         pathish?
         ref-path?
         (contract-out
          [ref->path     (-> path-value? path/c)]
          [path->ref     (-> path/c symbol?)]
          [pathish->path (-> pathish? path/c)]))

(define path-index? exact-nonnegative-integer?)
(define path-value? (or/c symbol? path-index?))
(define path/c      (listof path-value?))
(define pathish?    (or/c path-value? path/c))

(define (ref-path? x) (path/c x))

(define (string->path-val v)
  (define number (string->number v))
  (if number number (string->symbol v)))

(define (path-val->string p)
  (if (number? p)
      (number->string p)
      (symbol->string p)))

(define (ref->path ref)
  (if (path-index? ref)
      (list ref)
      (map string->path-val (string-split (symbol->string ref) "."))))

(define (path->ref path)
  (string->symbol (string-join (map path-val->string path) ".")))

(define (pathish->path val)
  (if (path-value? val)
      (ref->path val)
      val))
