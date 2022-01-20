#lang scribble/doc
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)
@;
@; This Racket code in this package, including this file, is under the
@; Apache 2.0 and MIT licenses. The user can choose the license under
@; which they will be using the software.
@;
@; However, the SRFI 5 document has a restrictive licenses:
@; see the file "srfi-5.html" for the specific license.
@; (The implementation of `srfi/5` found in the
@; "srfi-lib" package is not licensed restrictively.)
@(require srfi/scribblings/util
          scribble/manual
          scribble/eval
          scriblib/render-cond
          scribble/core
          scribble/tag
          scribble/decode
          scribble/html-properties
          (for-syntax scheme/base)
          (for-label srfi/5))

@; ----------------------------------------------------------------------

@title{Nonfree SRFI Specification Documents}
@; hack to get a link to the package:
@defmodule[#:require-form (λ (mod) (list @smaller{SRFI 5 specification}))
           srfi/5
           #:packages ("srfi-doc-nonfree")
           #:no-declare #:link-target? #f]

@(let ()
   ;; set up indirect linking
   (define (redirect taglet anchor)
     (redirect-target-element
      #f null (make-section-tag taglet) "srfi-std/srfi-5.html" anchor))
   (list @elem[#:style srfi-std]{}
         (redirect srfi-5-std-taglet "")
         (redirect srfi-5-license-taglet "copyright")))

This package contains a copy of the @seclink[srfi-5-std-taglet]{
SRFI 5 specification document}.

For @hyperlink[srfi-license-history-url]{historical
 reasons}, the SRFI 5 specification document has a
@seclink[srfi-5-license-taglet]{restrictive license} and is
not included in the main Racket distribution.

Racket provides a free implementation of SRFI 5 in
@racketmodname[srfi/5] and free alternative documentation
for SRFI 5's variant of @racket[let] in
@secref["srfi-5" #:doc '(lib "srfi/scribblings/srfi.scrbl")].
The implementation and documentation are distributed under
the same @racket-license-link{license} as Racket: only the
original specification document is restrictively licensed.
