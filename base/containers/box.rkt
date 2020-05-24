#lang racket/base

(provide hbox vbox)

(require racket/contract
         racket/match
         racket/format
         "../types.rkt"
         "../mode.rkt")

(define/contract (axis->dir/pr2 axis)
  (-> axis/c pr2?)
  (match axis
    ['h (pr2 pr-1 pr-0)]
    ['v (pr2 pr-0 pr-1)]))

(define/contract (elements->pieces size axis elements)
  (-> pos? axis/c elements/c pieces/c)
  (define dir/pr2  (axis->dir/pr2 axis))
  (for/fold ([lst null]
             [remaining (pos->pr2 size)]
             #:result (reverse lst))
            ([sizeable (in-list (elements->sizeables elements axis))]
             #:break (remaining . pr2-or-< . pr2-zero))
    (when (remaining . pr2-or-< . pr2-zero)
      ;; TODO better error reporting make it configurable via policies?
      (raise "not enough space to layout all elements"))

    (define piece    (apply-wrapper sizeable (pr2->pos remaining size)))
    (define consumed (pr2* dir/pr2 (ui-bounds piece)))
    (values (cons piece lst)
            (pr2- remaining consumed))))

(define (box-container axis extend-op max-op result-op)
  (λ (#:min [min #f] . children)
    (define dir/pr2 (axis->dir/pr2 axis))
    (define/contract (box-sizeable size)
      sizeable/c

      (define child-pieces (elements->pieces size axis children))
      (container
       (λ (self)
         (for/fold ([extend   pr-0]
                    [max-size 0]
                    #:result (result-op extend
                                        (if min (pixel max-size) (ratio 1))))
                   ([c (in-list child-pieces)])
           (define b (ui-bounds c))
           (values (pr+ extend (extend-op b))
                   (max max-size (pixelratio-pixel (max-op b))))))
       (λ (self pos callback)
         (for/fold ([pos (pos->pr2 pos)])
                   ([c (in-list child-pieces)])
           (define new-pos (pr2+ pos (pr2* dir/pr2 (ui-bounds c))))
           (callback c (pr2->pos pos size))
           new-pos))))
    box-sizeable))

(define (hbox-result extend max)  (bounds extend max))
(define (vbox-result extend max)  (bounds max extend))

(define hbox (box-container 'h vec2-x vec2-y hbox-result))
(define vbox (box-container 'v vec2-y vec2-x vbox-result))

