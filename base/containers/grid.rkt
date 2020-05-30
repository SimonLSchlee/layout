#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../types.rkt"
         "../mode.rkt"
         "../utils/bounds.rkt"
         "../primitives/text.rkt"
         "../modifiers/clip.rkt"
         "../private/parameter.rkt"
         "box.rkt")

(provide grid)


(define (bounds-policy/no-check piece size) piece)

(define (bounds-policy/clip piece size)
  (with-handlers ([exn:bounds-overflow? (λ (e) (clip-piece piece size))])
    (check-bounds piece size)
    piece))

(define (bounds-policy/raise piece size)
  (check-bounds piece size)
  piece)

(define-named-parameter current-bounds-policy-default bounds-policy/raise)

(define bounds-policy/c (-> piece? size? piece?))

(define/contract (grid #:size [size full-size]
                       #:spacing [spacing pr2-zero]
                       #:stretchable-width  [swidth #f]
                       #:stretchable-height [sheight #f]
                       #:bounds-policy [bounds-policy (current-bounds-policy-default)]
                       . data)
  (->* ()
       (#:size bounds?
        #:spacing pr2?
        #:stretchable-width  boolean?
        #:stretchable-height boolean?
        #:bounds-policy bounds-policy/c)
       #:rest (listof (listof sizeable/c))
       any/c)
  (define stretch     (pos (if swidth 1 0) (if sheight 1 0)))
  (λ (max-size)
    (define rows    (length data))
    (define (calculate)
      (define columns       (length (first data)))
      (define row-heights   (make-vector rows))
      (define column-widths (make-vector columns))

      (define abs-spacing   (pr2->pos spacing max-size))
      (match-define (vec2 sx sy) abs-spacing)

      (for/fold ([y (- sy)]
                 #:result (void))
                ([(row ri) (in-indexed (in-list data))])
        (define y0 (+ y sy))
        (for/fold ([x (- sx)]
                   #:result (void))
                  ([(cell ci) (in-indexed (in-list row))])
          (define x0 (+ x sx))
          (define cell-pos  (pos x0 y0))
          (define remaining (pos/max vec2-zero (pos- max-size cell-pos)))
          (match-define (vec2 bx by) (ui-bounds (apply-wrapper cell remaining)))
          (define px (pixelratio-pixel bx))
          (define py (pixelratio-pixel by))
          (vector-accum row-heights   ri py)
          (vector-accum column-widths ci px)
          (+ x0 px))
        (+ y0 (vector-ref row-heights ri)))

      (define total-spacing  (pos* abs-spacing (vec2 (spaces columns) (spaces rows))))
      (define min-size       (pos+ total-spacing
                                   (pos (sum column-widths)
                                        (sum row-heights))))

      (define remaining-size (pos/max vec2-zero (pos- max-size min-size)))
      (define extra-size     (pos/ remaining-size (pos columns rows)))
      (match-define (vec2 extra-x extra-y) (pos* extra-size stretch))

      (define lst
        (for/fold ([lst null]
                   [y (- sy)]
                   #:result lst)
                  ([row        (in-list data)]
                   [min-height (in-vector row-heights)])
          (define y0     (+ y sy))
          (define height (+ min-height extra-y))
          (define new-lst
            (for/fold ([lst lst]
                       [x (- sx)]
                       #:result lst)
                      ([cell (in-list row)]
                       [min-width (in-vector column-widths)])
              (define x0          (+ x sx))
              (define width       (+ min-width extra-x))
              (define cell-size   (vec2 width height))
              (define child-piece (apply-wrapper cell cell-size))
              (define fixed-piece (bounds-policy child-piece cell-size))
              (values (cons (cons fixed-piece (pos x0 y0)) lst)
                      (+ x0 width))))
          (values new-lst
                  (+ y0 height))))

      (container (λ (self) (pos->pr2 min-size))
                 (λ (self pos callback)
                   (for ([loc (in-list lst)])
                     (match-define (cons child child-pos) loc)
                     (define new-pos (pos+ pos child-pos))
                     (callback child new-pos)))))

    (if (rows . > . 0)
        (calculate)
        nothing-piece)))


(define (vector-accum vec index val)
  (define old (vector-ref vec index))
  (define new (max old val))
  (vector-set! vec index new))

(define (sum v)
  (for/sum ([x (in-vector v)]) x))

(define (spaces x) (max 0 (sub1 x)))
