#lang racket/base

(require racket/contract
         racket/match
         racket/list
         racket/class
         "../types.rkt"
         "../mode.rkt"
         "../utils/bounds.rkt"
         "../primitives/text.rkt"
         "../modifiers/clip.rkt"
         "../modifiers/effect.rkt"
         "../private/parameter.rkt"
         "../private/color.rkt"
         "box.rkt")

(provide grid

         bounds-policy/c
         (contract-out
          [bounds-policy/no-check bounds-policy/c]
          [bounds-policy/clip     bounds-policy/c]
          [bounds-policy/raise    bounds-policy/c])

         grid-draw-effect/c
         (contract-out
          [draw-grid-lines        grid-draw-effect/c]
          [draw-column-background (-> brush/c brush/c grid-draw-effect/c)]
          [draw-row-background    (-> brush/c brush/c grid-draw-effect/c)]))


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

(define info/c (-> symbol? any))
(define brush/c (is-a?/c brush%))
(define grid-draw-effect/c (or/c #f (-> piece? pos? info/c any/c)))

(define/contract (grid #:size [size full-size]
                       #:spacing [spacing pr2-zero]
                       #:stretchable-width  [swidth #f]
                       #:stretchable-height [sheight #f]
                       #:bounds-policy [bounds-policy (current-bounds-policy-default)]
                       #:draw-before [draw-before #f]
                       #:draw-after  [draw-after #f]
                       . data)
  (->* ()
       (#:size bounds?
        #:spacing pr2?
        #:stretchable-width  boolean?
        #:stretchable-height boolean?
        #:bounds-policy bounds-policy/c
        #:draw-before grid-draw-effect/c
        #:draw-after  grid-draw-effect/c)
       #:rest (listof (listof sizeable/c))
       any/c)
  (define stretch (pos (if swidth 1 0) (if sheight 1 0)))
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

      (define total-spacing (pos* abs-spacing (vec2 (spaces columns) (spaces rows))))
      (define min-size      (pos+ total-spacing
                                  (pos (sum column-widths)
                                       (sum row-heights))))

      (define remaining-size     (pos/max vec2-zero (pos- max-size min-size)))
      (define used-extra-size    (pos* stretch remaining-size))
      (define divided-extra-size (pos/ used-extra-size (pos columns rows)))
      (match-define (vec2 extra-x extra-y) divided-extra-size)

      (define used-size (pos+ min-size used-extra-size))
      (define bounds (pos->pr2 (pos+ used-size stretch)))

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

      (define layout
        (container (λ (self) bounds)
                   (λ (self pos callback)
                     (for ([loc (in-list lst)])
                       (match-define (cons child child-pos) loc)
                       (define new-pos (pos+ pos child-pos))
                       (callback child new-pos)))))

      (define (grid-coords spacing extra extents [fence #t])
        (define s  spacing)
        (define hs (/ s 2))
        (define start (if fence 1 0))
        (for/fold ([e (- s)] [lst null] #:result (reverse lst))
                  ([index (in-range start (vector-length extents))]
                   [extent (in-vector extents)])
          (define e1 (+ e s extent extra))
          (values e1
                  (cons (+ e1 hs)
                        lst))))

      (define (info key)
        (match key
          ['size         used-size]
          ['grid-columns (grid-coords sx extra-x column-widths #f)]
          ['grid-rows    (grid-coords sy extra-y row-heights   #f)]
          ['grid (values (grid-coords sx extra-x column-widths)
                         (grid-coords sy extra-y row-heights))]))

      (define (effect-draw effect)
        (if effect
            (λ (self pos) (effect self pos info))
            #f))

      (if (or draw-before draw-after)
          (effect-piece #:before (effect-draw draw-before)
                        layout
                        #:after  (effect-draw draw-after))
          layout))

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

(define (size->adjusted-size dc size)
  (define pen (send dc get-pen))
  (define pw  (send pen get-width))
  (define hw  (/ pw 2))
  (define hw2 (vec2 hw hw))
  (pos/max vec2-zero (pos- size hw2)))

(define (draw-grid-lines self pos info)
  (define-values (columns rows) (info 'grid))

  (define dc (current-dc))
  (define size (info 'size))
  (match-define (vec2 xs ys) (size->adjusted-size dc size))

  (for ([c (in-list columns)])
    (define start (pos+ pos (vec2 c 0)))
    (define end   (pos+ pos (vec2 c ys)))
    (send dc draw-line (vec2-x start) (vec2-y start) (vec2-x end) (vec2-y end)))

  (for ([r (in-list rows)])
    (define start (pos+ pos (vec2 0  r)))
    (define end   (pos+ pos (vec2 xs r)))
    (send dc draw-line (vec2-x start) (vec2-y start) (vec2-x end) (vec2-y end))))

(define (((draw-background select next-proc corner-proc) b1 b2) self pos info)
  (define extents      (info select))
  (define other-extend (corner-proc (info 'size)))

  (define dc (current-dc))
  (define old-pen   (send dc get-pen))
  (define old-brush (send dc get-brush))
  (send dc set-pen invisible 1 'solid)

  (for/fold ([current pos] [alternate #t] #:result (void))
            ([e (in-list extents)])
    (define next   (pos+ pos  (next-proc e)))
    (define corner (pos+ next other-extend))
    (define size   (pos- corner current))
    (send dc set-brush (if alternate b1 b2))
    (send dc draw-rectangle (vec2-x current) (vec2-y current) (vec2-x size) (vec2-y size))
    (values next (not alternate)))

  (send dc set-pen   old-pen)
  (send dc set-brush old-brush))

(define (column/next e)     (vec2 e 0))
(define (row/next e)        (vec2 0 e))
(define (column/corner s)   (vec2 0 (vec2-y s)))
(define (row/corner s)      (vec2 (vec2-x s) 0))
(define draw-column-background (draw-background 'grid-columns column/next column/corner))
(define draw-row-background    (draw-background 'grid-rows       row/next    row/corner))

