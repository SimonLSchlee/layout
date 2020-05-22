#lang racket/base

(provide (all-defined-out))
(require racket/contract
         racket/draw
         racket/class
         json
         "../data.rkt"
         "../mode.rkt")

(define setup/c      (-> any/c))
(define out/c        (or/c path-string? output-port? #f))

(define ((document paper-name orientation))
  (define s (new ps-setup%))
  (send s set-paper-name paper-name)
  (send s set-orientation orientation)
  (send s set-mode 'file)
  (current-ps-setup s))

(define/contract ((pdf document-setup document-label page-layout) data out)
  (-> setup/c string? layout-thk/c (-> data/c out/c any/c))
  (document-setup)

  (define dc (new pdf-dc% [interactive #f] [output out]))
  (send dc start-doc document-label)
  (layout-apply page-layout dc data)
  (send dc end-doc))
