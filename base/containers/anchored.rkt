#lang racket/base

(provide anchored
         anchored/fn)

(require racket/contract
         racket/match
         racket/list
         "../types.rkt"
         "../mode.rkt")

(define/contract (get-anchor-point center dir)
  (-> pr2? dir? pr2?)
  (pr2+ center (pr2* center (dir->pr2 dir))))

(define/contract (piece->center piece)
  (-> piece? pr2?)
  (define bottom-right (ui-bounds piece))
  (define center       (pr2/ bottom-right pr2-two))
  center)

(define/contract (piece->center/absolute piece size)
  (-> piece? pos? pos?)
  (pr2->pos (piece->center piece)
            size))

(define (definition? def)
  (and (list? def)
       (let ([f (first def)])
         (and (procedure? f)
              (procedure-arity-includes? f 1)))
       (for/and ([a (in-list (rest def))])
         (dir? a))))

(define defs/c (listof definition?))

(define/contract (anchored/fn #:size [size full-size] . anchor-defs)
  (->* () (#:size bounds?) #:rest defs/c sizeable/c)
  (λ (maxsize)
    (define center (pr2/ size pr2-two))
    (define msize  (pr2->pos size maxsize))

    (define (calc-location child-sizeable parent-anchor-dir child-anchor-dir msize)
      (define child-piece  (apply-wrapper child-sizeable msize))
      (define child-center (piece->center child-piece))
      (define child-pos    (anchors->pos child-center parent-anchor-dir child-anchor-dir))
      (cons child-piece (pr2->pos child-pos msize)))

    (define (anchors->pos child-center parent-anchor-dir child-anchor-dir)
      (define parent-anchor (get-anchor-point center parent-anchor-dir))
      (define child-anchor  (get-anchor-point child-center child-anchor-dir))
      (define child-pos     (pr2- parent-anchor child-anchor))
      child-pos)

    (define lst
      (for/list ([def (in-list anchor-defs)])
        (match def
          [(list child-sizeable parent-anchor-dir child-anchor-dir)
           (calc-location child-sizeable parent-anchor-dir child-anchor-dir msize)])))

    (container (λ (self)
                 size)
               (λ (self pos callback)
                 (for ([loc (in-list lst)])
                   (match-define (cons child child-pos) loc)
                   (define new-pos (pos+ pos child-pos))
                   (callback child new-pos))))))


(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser anchored
  [(anchored #:size size [def ...] ...)  #'(anchored/fn #:size size (list def ...) ...)]
  [(anchored [def ...] ...)       #'(anchored/fn (list def ...) ...)])

