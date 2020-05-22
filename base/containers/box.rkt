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

(define (box-container axis bounds-op result-op)
  (λ children
    (define dir/pr2 (axis->dir/pr2 axis))
    (define/contract (box-sizeable size)
      sizeable/c

      (define child-pieces (elements->pieces size axis children))
      (container
       (λ (self)
         (for/fold ([extend pr-0]
                    #:result (result-op extend))
                   ([c (in-list child-pieces)])
           (pr+ extend (bounds-op (ui-bounds c)))))
       (λ (self pos callback)
         (for/fold ([pos (pos->pr2 pos)])
                   ([c (in-list child-pieces)])
           (define new-pos (pr2+ pos (pr2* dir/pr2 (ui-bounds c))))
           (callback c (pr2->pos pos size))
           new-pos))))
    box-sizeable))

(define (hbox-result extend)  (bounds extend (ratio 1)))
(define (vbox-result extend)  (bounds (ratio 1) extend))

(define hbox (box-container 'h vec2-x hbox-result))
(define vbox (box-container 'v vec2-y vbox-result))

