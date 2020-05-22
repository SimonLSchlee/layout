#lang racket/base

(require racket/contract
         "vec2.rkt"
         "vec2-variant.rkt"
         "pixelratio.rkt"
         "pixelratiovec2.rkt")

(provide direction?
         (contract-out
          [dir->pr2 (-> dir? pr2?)]))

(define direction? (between/c -1 1))

(provide/define-vec2-variant direction? dir dir?)

(define (dir->pr2 dir)
  (pr2 (scalar->pr (vec2-x dir))
       (scalar->pr (vec2-y dir))))
