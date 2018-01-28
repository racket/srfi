#lang info

(define collection 'multi)

(define build-deps '("mzscheme-doc"
                     "scheme-lib"
                     "base"
                     "scribble-lib"
                     "srfi-doc"
                     "srfi-lib-nonfree"
                     "racket-doc"
                     "r5rs-doc"
                     "r6rs-doc"
                     "compatibility-lib"))
(define update-implies '("srfi-lib-nonfree"))

(define pkg-desc "documentation part of \"srfi nonfree\"")

(define pkg-authors '(mflatt noel chongkai jay))
