#lang racket/base

(provide provide/define-vec2-variant
         define-vec2-op-provide/definer)

(require racket/match
         racket/contract
         "vec2.rkt")

(define-syntax-rule (provide/define-vec2-variant contract/pred? constructor predicate?)
  (begin (provide (contract-out [constructor (-> contract/pred? contract/pred? any/c)]
                                [predicate?  (-> any/c boolean?)]))
         (define (constructor x y)
           (vec2 x y))
         (define (predicate? v)
           (match v
             [(vec2 x y)
              (and (contract/pred? x)
                   (contract/pred? y))]
             [_ #f]))))

(define-syntax-rule (define-vec2-op-provide/definer definer-name predicate?)
  (define-syntax-rule (definer-name name op accum)
    (begin
      (provide (contract-out [name (-> predicate? predicate? any/c)]))
      (define (name l r)
        (match-define (vec2 x1 y1) l)
        (match-define (vec2 x2 y2) r)
        (accum (op x1 x2)
               (op y1 y2))))))
