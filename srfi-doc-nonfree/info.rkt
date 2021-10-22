#lang info
;; SPDX-License-Identifier: (Apache-2.0 OR MIT)
;;
;; This Racket code in this package, including this file, is under the
;; Apache 2.0 and MIT licenses. The user can choose the license under
;; which they will be using the software.
;;
;; However, the SRFI 29 and SRFI 5 documents have restrictive licenses:
;; see the individual documents for those licenses.
;; (The implementations of `srfi/29` and `srfi/5` found in the
;; "srfi-lib" package are not licensed restrictively.)
;;
;; NOTE: Since the license of the SRFI 29 and SRFI 5 documents
;; doesn't have an SPDX license identifier, this file intentionally
;; DOES NOT include a definition for `license`.

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
