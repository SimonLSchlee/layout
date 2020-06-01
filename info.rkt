#lang info

(define collection "layout")
(define deps
  (list "base"
        "draw-lib"
        "pict-lib"
        "reprovide-lang-lib"
        "rackunit-chk"))

(define build-deps '("racket-doc"
                     "rackunit-doc"
                     "scribble-lib"))

(define pkg-authors '("schlee.simon@gmail.com"))
