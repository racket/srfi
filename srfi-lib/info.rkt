#lang info

(define collection 'multi)

(define version "1.1")

(define deps '("scheme-lib"
               "base"
               "srfi-lite-lib"
               "r6rs-lib"
               "compatibility-lib"))

(define implies '("srfi-lite-lib"))

(define pkg-desc "implementation (no documentation) part of \"srfi\"")

(define pkg-authors '(mflatt noel chongkai jay))

(define license
  '(Apache-2.0 OR MIT))
