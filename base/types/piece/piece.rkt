#lang racket/base

(require racket/contract
         "../pixelratio.rkt"
         "bounds.rkt"
         "pos.rkt")


(struct piece (bounds layout draw))
(define pieces/c (listof piece?))

(define bounds/c    (-> piece? bounds?))
(define layout-cb/c (-> piece? pos? any/c))
(define layout/c    (-> piece? pos? layout-cb/c any/c))
(define draw/c      (-> piece? pos? any/c))

(provide bounds/c
         layout/c
         draw/c
         (contract-out [struct piece
                         ((bounds bounds/c)
                          (layout layout/c)
                          (draw   draw/c))])
         pieces/c)

(define-syntax-rule (provide/define-method name contract op args ...)
  (begin
    (provide (contract-out [name contract]))
    (define (name self args ...)
      ((op self) self args ...))))

(provide/define-method ui-bounds bounds/c piece-bounds)
(provide/define-method ui-layout layout/c piece-layout pos cb)
(provide/define-method ui-draw   draw/c   piece-draw   pos)

(provide container-draw
         container
         leaf-layout
         leaf
         nothing-draw
         nothing
         modify)

(define (container-draw self pos)
  (ui-layout self pos ui-draw))

(define (container bounds layout)
  (piece bounds layout container-draw))

(define (leaf-layout self pos cb)
  (void))

(define (leaf bounds draw)
  (piece bounds leaf-layout draw))

(define (nothing-draw self pos)
  (void))

(define nothing
  (piece (λ (self) (bounds (pixel 0) (pixel 0)))
         leaf-layout
         nothing-draw))

(define (modify p
                #:bounds [bounds (piece-bounds p)]
                #:layout [layout (piece-layout p)]
                #:draw   [draw   (piece-draw p)])
  (piece bounds layout draw))
