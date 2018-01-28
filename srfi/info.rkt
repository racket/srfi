#lang info

(define collection 'multi)

(define deps '("srfi-lib"
               "srfi-lib-nonfree"
               "srfi-doc"
               "srfi-doc-nonfree"))
(define implies '("srfi-lib"
                  "srfi-lib-nonfree"
                  "srfi-doc"
                  "srfi-foc-nonfree"))

(define pkg-desc "Legacy SRFI (Scheme) libraries")

(define pkg-authors '(mflatt noel chongkai jay))
