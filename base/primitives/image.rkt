#lang racket/base

(provide image)

(require racket/contract
         racket/class
         racket/match
         racket/string
         racket/list
         net/url
         pict
         racket/draw
         "../data.rkt"
         "../types.rkt"
         "../mode.rkt")

(define invisible (make-color 0 0 0 0))

(define/contract (image url
                        #:size [size #f]
                        #:scale [scale #f]
                        #:mode [mode 'preserve])
  (->* (string/ref?)
       (#:size (or/c #f bounds?) #:scale (or/c #f list?) #:mode symbol?) sizeable/c)
  (define uri (ref/url/picture url))
  (λ (maxsize)
    (define picture (get-picture uri))
    (define bitmap
      (if size
          (let ()
            (match-define (vec2 w h) (pr2->pos size maxsize))
            (scaled w h mode picture))
          picture))
    (define width  (send bitmap get-width))
    (define height (send bitmap get-height))
    (leaf (if scale
              (λ (self) (bounds (pixel (floor (* width (first scale)))) (pixel (floor (* height (first scale))))))
              (λ (self) (bounds (pixel width) (pixel height))))
          (λ (self pos)
            (match-define (vec2 x y) pos)
            (define (img x y) (send (current-dc) draw-bitmap bitmap x y 'solid invisible bitmap))
            (if scale
                (let ()
                  (define-values (xs ys) (send (current-dc) get-scale))
                  (match-define (list nx ny) scale)
                  (send (current-dc) set-scale nx ny)
                  ;; scale coordinates up when image is scaled down and the reverse
                  (img (* x (/ 1 nx))
                       (* y (/ 1 ny)))
                  (send (current-dc) set-scale xs ys))
                (img x y))))))

(define (get-picture url)
  (make-object bitmap% (get-pure-port (string->url url))))

(define (scaled width height mode b)
  (pict->bitmap (scale-to-fit (bitmap b) width height #:mode mode)))
