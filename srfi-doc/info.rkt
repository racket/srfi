#lang info

(define collection 'multi)

(define version "1.1")

(define build-deps '("mzscheme-doc"
                     "scheme-doc"
                     "scheme-lib"
                     "base"
                     "scribble-lib"
                     "srfi-lib"
                     "racket-doc"
                     "racket-index"
                     "compatibility-lib"))

(define deps '("base"))

(define update-implies '("srfi-lib"))

(define pkg-desc "documentation part of \"srfi\"")

(define pkg-authors '(mflatt noel chongkai jay))

(define license
  '(Apache-2.0 OR MIT))
