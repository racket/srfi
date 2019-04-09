#lang info

(define collection 'multi)

(define build-deps '("mzscheme-doc"
                     "scheme-lib"
                     "base"
                     "scribble-lib"
                     "srfi-doc"
                     "racket-doc"
                     "r5rs-doc"
                     "r6rs-doc"
                     "compatibility-lib"))

(define pkg-desc "non-free documentation for \"srfi-lib\"")

(define pkg-authors '(mflatt noel chongkai jay))
