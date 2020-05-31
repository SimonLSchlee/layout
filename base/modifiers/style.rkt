#lang racket/base

(provide style
         make-font
         make-color
         get-pen
         get-brush)

(require racket/contract
         racket/draw
         racket/class
         racket/match
         "../types.rkt"
         "../mode.rkt")

(define (get-pen . args)   (send/apply the-pen-list   find-or-create-pen   args))
(define (get-brush . args) (send/apply the-brush-list find-or-create-brush args))

(define font/c (is-a?/c font%))
(define color/c (is-a?/c color%))

(define (apply-nothing next)
  (next))

(define-syntax-rule (define-apply-op name get set)
  (define ((name val) next)
    (define dc (current-dc))
    (define old (send dc get))
    (send dc set val)
    (begin0 (next)
      (send dc set old))))

(define-apply-op apply-font  get-font  set-font)
(define-apply-op apply-pen   get-pen   set-pen)
(define-apply-op apply-brush get-brush set-brush)


(define/contract ((style #:font  [font  #f]
                         #:pen   [pen   #f]
                         #:brush [brush #f])
         content)
  (->* () (#:font  (or/c #f (is-a?/c font%))
           #:pen   (or/c #f (is-a?/c pen%))
           #:brush (or/c #f (is-a?/c brush%)))
       (-> sizeable/c sizeable/c))
  (define s1 (if font  (apply-font  font)  apply-nothing))
  (define s2 (if pen   (apply-pen   pen)   s1))
  (define s3 (if brush (apply-brush brush) s2))
  (define apply-style s3)
  (λ (size)
    (define piece (apply-style (λ () (apply-wrapper content size))))
    (modify piece
            #:bounds (λ (self)     (apply-style (λ () (ui-bounds piece))))
            #:draw   (λ (self pos) (apply-style (λ () (ui-draw piece pos)))))))

