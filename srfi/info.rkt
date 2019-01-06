#lang info

(define collection 'multi)

(define deps '("srfi-lib"
               "srfi-doc"
               "srfi-doc-nonfree"))
(define implies '("srfi-lib"
                  "srfi-doc"
                  "srfi-doc-nonfree"))

(define pkg-desc "Legacy SRFI (Scheme) libraries")

(define pkg-authors '(mflatt noel chongkai jay))
