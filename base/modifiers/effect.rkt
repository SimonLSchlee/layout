#lang racket/base

(require racket/contract
         "../types.rkt"
         "../mode.rkt")

(provide (contract-out
          [effect
           (->* (sizeable/c)
                (#:before (or/c #f draw/c)
                 #:main   (or/c #f draw/c)
                 #:after  (or/c #f draw/c))
                sizeable/c)]
          [effect-piece
           (->* (piece?)
                (#:before (or/c #f draw/c)
                 #:main   (or/c #f draw/c)
                 #:after  (or/c #f draw/c))
                piece?)]))

(define (effect #:before [before #f]
                content
                #:main   [main #f]
                #:after  [after #f])
  (if (or before main after)
      (λ (size)
        (define piece (apply-wrapper content size))
        (effect-piece #:before before piece #:main main #:after after))
      content))

(define (effect-piece #:before [before #f]
                      piece
                      #:main   [main #f]
                      #:after  [after #f])
  (if (or before main after)
      (modify piece
              #:draw
              (λ (self pos)
                (when before
                  (before piece pos))

                (if main
                    (main piece pos)
                    (ui-draw piece pos))

                (when after
                  (after piece pos))))
      piece))
