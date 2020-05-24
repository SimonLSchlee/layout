#lang racket/base

(provide image)

(require racket/contract
         racket/class
         racket/match
         racket/string
         net/url
         pict
         racket/draw
         "../data.rkt"
         "../types.rkt"
         "../mode.rkt")

(define invisible (make-color 0 0 0 0))

(define/contract (image url
                        #:size [size full-size]
                        #:mode [mode 'preserve])
  (->* (string/ref?) (#:size bounds? #:mode symbol?) sizeable/c)
  (define uri (ref/url/picture url))
  (λ (maxsize)
    (match-define (vec2 w h) (pr2->pos size maxsize))
    (define bitmap (scaled w h mode (get-picture uri)))
    (define width  (send bitmap get-width))
    (define height (send bitmap get-height))
    (leaf (λ (self) (bounds (pixel width) (pixel height)))
          (λ (self pos)
            (match-define (vec2 x y) pos)
            (send (current-dc) draw-bitmap bitmap x y 'solid invisible bitmap)))))

(define (get-picture url)
  (make-object bitmap% (get-pure-port (string->url url))))

(define (scaled width height mode b)
  (pict->bitmap (scale-to-fit (bitmap b) width height #:mode mode)))
