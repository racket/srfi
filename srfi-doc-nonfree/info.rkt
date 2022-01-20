#lang info
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)
;;
;; This Racket code in this package, including this file, is under the
;; Apache 2.0 and MIT licenses. The user can choose the license under
;; which they will be using the software.
;;
;; However, the SRFI 5 document has a restrictive licenses:
;; see the file "srfi-5.html" for the specific license.
;; (The implementation of `srfi/5` found in the
;; "srfi-lib" package is not licensed restrictively.)
;;
;; NOTE: Since the license of the SRFI 5 document
;; doesn't have an SPDX license identifier, this file intentionally
;; DOES NOT include a definition for `license`.

(define collection 'multi)

(define build-deps '("scheme-lib"
                     "base"
                     "scribble-lib"
                     "racket-index"
                     "srfi-lib"
                     ["srfi-doc" #:version "1.1"]))

(define pkg-desc "non-free SRFI 5 standard document")

(define pkg-authors '(mflatt noel chongkai jay))
