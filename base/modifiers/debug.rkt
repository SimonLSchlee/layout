#lang racket/base

(provide debug)

(require racket/contract
         "../types.rkt"
         "../mode.rkt")

(define/contract (debug content)
  (-> sizeable/c sizeable/c)
  (λ (size)
    (parameterize ([current-wrapper debug-wrapper])
      (apply-wrapper content size))))
