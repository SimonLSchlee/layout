#lang racket/base

(provide interact
         interact-gui)

(require racket/gui/base
         racket/format
         racket/class
         racket/match
         framework

         "../base/mode.rkt"
         "../base/types.rkt")


(define (split direction)
  ((compose1 panel:splitter-mixin
             direction
             panel:dragable-mixin)
   panel%))

(define hsplit (split panel:horizontal-dragable-mixin))
(define vsplit (split panel:vertical-dragable-mixin))

(define background (make-color 120 120 120))


(define sizeable-editor
  (class canvas%
    (define piece #f)
    (define size  #f)

    (super-new [style '(border hscroll vscroll)])

    (define offset     40)
    (define offset2    (* offset 2))
    (define line-start 10)
    (define line-end   (- offset line-start))

    (define/public (set-sizeable sizeable s)
      (if s
          (parameterize ([page-started #t]
                         [current-dc (send this get-dc)])
            (set! size s)
            (set! piece (apply-wrapper sizeable s))

            (match-define (vec2 w h) (pr2->pos (ui-bounds piece) size))
            (send this init-auto-scrollbars
                  (inexact->exact (ceiling (+ w offset2)))
                  (inexact->exact (ceiling (+ h offset2)))
                  (send this get-scroll-pos 'horizontal)
                  (send this get-scroll-pos 'vertical)))
          (begin (send this init-auto-scrollbars #f #f 0 0)
                 (set! size #f)
                 (set! piece #f)))
      (send this refresh))

    (define/override (on-paint)
      (when piece
        (define dc (send this get-dc))
        (send dc draw-line offset 10 offset 30)
        (send dc draw-line 10 offset 30 offset)
        (parameterize ([page-started #t]
                       [current-dc dc])
          (ui-draw piece (vec2 40 40))))
      (void))))


(define current-indent (make-parameter ""))

(define ((build-choices-wrapper lb) sizeable)
  (λ (size)
    (define label (~a (current-indent) "size<" (vec2-x size) ", " (vec2-y size) ">"))
    (send lb append label (cons size sizeable))
    (parameterize ([current-indent (~a (current-indent) "  ")])
      (sizeable size))))


(define (interact-gui size sizeable-content)
  (define frame (new frame% [label "Piece tree interactive editor"]
                     [width 1000] [height 600]))
  (define l (new hsplit [parent frame]))
  (define e (new sizeable-editor [parent l]))

  (define v (new vertical-panel% [parent l]))
  (define options-row1 (new horizontal-panel% [parent v]
                            [stretchable-height #f]))

  (define (update-wrapper self . args)
    (match (send self get-string-selection)
      ["none"  (current-wrapper no-wrap)]
      ["debug" (current-wrapper debug-wrapper)])
    (update-selected))
  (new choice% [parent options-row1]
       [label "current wrapper: "] [choices (list "none" "debug")]
       [callback update-wrapper])

  (define (update-selected . args)
    (define selected (send b get-selection))
    (when selected
      (match-define (cons size selected-sizeable) (send b get-data selected))
      (send e set-sizeable selected-sizeable size)))

  (define b (new list-box% [parent v] [label #f] [choices null] [callback update-selected]))
  (parameterize ([current-wrapper (build-choices-wrapper b)])
    (send e set-sizeable sizeable-content size)
    (send b set-selection 0))

  (send frame show #t))


(define (interact sizeable-content)
  (λ (size)
    (define es (make-eventspace))
    (parameterize ([current-eventspace es]
                   [current-wrapper no-wrap])
      (interact-gui size sizeable-content))

    (define t (thread (λ () (sync es))))
    (thread-wait t)

    (apply-wrapper sizeable-content size)))

