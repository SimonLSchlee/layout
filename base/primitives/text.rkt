#lang racket/base

(provide text)

(require racket/contract
         racket/class
         racket/match
         racket/string
         "../data.rkt"
         "../types.rkt"
         "../mode.rkt"
         "../containers/box.rkt")


(define/contract (line line)
  (-> string? sizeable/c)
  (λ (size)
    (leaf (λ (self)
            (define-values (width height delta-baseline vertical-space)
              (send (current-dc) get-text-extent line))
            (bounds (pixel width) (pixel height)))
          (λ (self pos)
            (match-define (vec2 x y) pos)
            (send (current-dc) draw-text line x y)))))

(define/contract (text text)
  (-> string/ref? sizeable/c)
  (define lines (string-split (ref/string text) "\n"))
  (match lines
    [(list single-line) (line single-line)]
    [(? list?)          (apply vbox #:min #t (map line lines))]))
