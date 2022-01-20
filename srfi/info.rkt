#lang info

(define collection 'multi)

(define deps '("srfi-lib"
               ["srfi-doc" #:version "1.1"]))
(define implies '("srfi-lib"
                  "srfi-doc"))

(define pkg-desc "Legacy SRFI (Scheme) libraries")

(define pkg-authors '(mflatt noel chongkai jay))

(define license
  '(Apache-2.0 OR MIT))
