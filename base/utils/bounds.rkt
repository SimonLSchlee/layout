#lang racket/base

(require racket/contract
         racket/format
         "../types.rkt")

(provide (contract-out
          [struct (exn:bounds-overflow exn:fail)
            ((message string?)
             (continuation-marks continuation-mark-set?)
             (piece piece?)
             (max-size size?))]
          [within-bounds? (-> piece? size? boolean?)]
          [check-bounds   (-> piece? size? any/c)]
          [size-check     (-> size? size? boolean?)]))

(struct exn:bounds-overflow exn:fail (piece max-size)
  #:extra-constructor-name make-exn:bounds-overflow
  #:transparent)


(define (within-bounds? piece max-size)
  (define size (piece-size piece max-size))
  (size-check size max-size))

(define (size-check size max-size)
  (and (vec2-zero . pos<= . size)
       (size      . pos<= . max-size)))

(define (check-bounds piece max-size)
  (define size (piece-size piece max-size))
  (unless (size-check size max-size)
    (define msg (format "Piece size ~a is exceeding the expected size: ~a" size max-size))
    (raise (make-exn:bounds-overflow msg (current-continuation-marks)
                                     piece max-size))))
