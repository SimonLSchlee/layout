#lang racket/base

(require "base.rkt")

(define (example)
  (define thirds (pr->pr2 (ratio 1/3)))
  (define (i) (image 'img #:size thirds))
  (define (nine i #:size [size full-size])
    (outline
     (anchored #:size size
               [(i) ↖ ↖] [(i) ↑ ↑] [(i) ↗ ↗]
               [(i) ← ←] [(i) • •] [(i) → →]
               [(i) ↙ ↙] [(i) ↓ ↓] [(i) ↘ ↘])))

  (define (scale content size)
    (anchored #:size size
              [content ↖ ↖]))

  (single-page
   (scale (nine (lambda () (scale (nine i) thirds)))
          (bounds (ratio 1) (pixel 600)))))

(define (render)
  (define A4-portrait (document "A4 210 x 297 mm" 'portrait))
  (define render-pdf (pdf A4-portrait "report" example))
  (render-pdf (hash 'img "https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Greycat.jpg/800px-Greycat.jpg")
              "test.pdf"))


(module+ main
  ;; (current-mode 'generate)
  ;; (current-wrapper debug-wrapper)
  (render))
