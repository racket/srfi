#!/usr/bin/env racket
#lang at-exp racket

(provide (all-defined-out))

(module+ main
  (require racket/cmdline)
  (parameterize ()
    (define work-dir #f)
    (define checkout? #f)
    (define rewrite? #f)
    (command-line
     #:usage-help
     ""
     "Replaces SRFI standard documents and auxiliary files by checking"
     "out the upstream Git repositories and applying Racket-specific"
     "transformations to the HTML files."
     ""
     "Existing files will be OVERWRITTEN as needed with NO WARNING."
     #:help-labels
     "---------------------------- actions ------------------------------"
     "     (If no action is provided, this script exits immediately.)"
     #:once-any
     [("--sync") "Checkout and rewrite"
      (set! checkout? #t)
      (set! rewrite? #t)]
     [("--checkout") "Checkout; don't rewrite"
      (set! checkout? #t)]
     [("--rewrite") "Rewrite; don't checkout (needs --work-dir)"
      (set! rewrite? #t)]
     #:help-labels
     "------------------------- configuration ---------------------------"
     #:once-each
     [("--work-dir") path "Keep upstream files in <path> (default: temporary)"
      (set! work-dir path)]
     [("--owner") github-owner "Use instead of \"scheme-requests-for-implementation\""
      (current-owner github-owner)]
     [("--ref") git-ref "Use instead of 'head"
      (current-ref git-ref)]
     #:help-labels
     "------------------------------ misc -------------------------------"
     #:args ()
     (if (or checkout? rewrite?)
         (srfi-sync-all #:work-dir work-dir
                        #:checkout? checkout?
                        #:rewrite? rewrite?)
         (eprintf "warning: no action specified\n")))))

(require xml
         net/url-string
         net/git-checkout
         "srfi-doc/srfi/scribblings/util.rkt"
         racket/runtime-path)

(define current-owner
  (make-parameter "scheme-requests-for-implementation"))
(define current-ref
  (make-parameter 'head))

(define (srfi-sync-all #:work-dir [work-dir #f]
                       #:checkout? [checkout? #t]
                       #:rewrite? [rewrite? #t]
                       #:owner [owner (current-owner)]
                       #:ref [ref (current-ref)])
  (when rewrite?
    (unless (or checkout? work-dir)
      (error 'srfi-sync-all "rewrite without checkout requires work-dir")))
  (parameterize ([current-owner owner]
                 [current-ref ref])
    (define ((go tmp))
      (when checkout?
        (srfi-checkout-all tmp))
      (when rewrite?
        (srfi-rewrite-all tmp)))
    (cond
      [work-dir
       (make-directory* work-dir)
       ((go work-dir))]
      [else
       (define tmp (make-temporary-file "tmp-srfi-sync~a" 'directory))
       (dynamic-wind void
                     (λ ()
                       (call-with-continuation-barrier (go tmp)))
                     (λ ()
                       (delete-directory/files tmp)))])))

(define the-srfis
  `(,@'() 1 2 4 5 6 7 8 9
          11 13 14 16 17 19
          23 25 26 27 28 29
          30 31 34 35 38 39
          40 41 42 43 45 48
          54 57 59
          60 61 62 63 64 66 #;67 69 ;; skip 67 for now
          71 74 78
          86 87
          98))

(define subdir-srfis
  '(41 67))

(define aux-file-extensions
  '(#".jpg" #".png" #".css"))

(define-runtime-path free:srfi-std/
  "srfi-doc/srfi/scribblings/srfi-std/")
(define-runtime-path nonfree:srfi-std/
  "srfi-doc-nonfree/srfi/scribblings/srfi-std/")

(define (srfi-n n)
  (string-append "srfi-" (number->string n)))

(define (srfi-checkout n dest-dir)
  (print-section-header "srfi-checkout: " 'sub n)
  (define repo-pth
    (string-append (current-owner) "/" (srfi-n n)))
  (list (string-append "https://github.com/" repo-pth)
        (git-checkout "github.com" repo-pth #:ref (current-ref)
                      #:dest-dir dest-dir)))

(define (srfi-checkout-all work-dir)
  (print-section-header "srfi-checkout-all")
  (call-with-output-file* (build-path work-dir "Sources.rktd")
    #:exists 'truncate/replace
    (λ (out)
      (pretty-write
       (for/list ([n (in-list the-srfis)])
         (cons n (srfi-checkout n (build-path work-dir (srfi-n n)))))
       out))))

(define (print-section-header label [level 'top] [extra ""])
  (define sep-len
    (match level
      ['top 40]
      ['sub 20]))
  (printf "~a\n;; ~a~a\n" (make-string sep-len #\;) label extra))

(define (srfi-rewrite-all work-dir)
  (print-section-header "srfi-rewrite-all")
  (define sources
    (file->value (build-path work-dir "Sources.rktd")))
  (for ([n (in-list the-srfis)])
    (match-define (list repo commit) (dict-ref sources n))
    (srfi-rewrite n work-dir #:repo repo #:commit commit)))

;; a srfi is a (cons/c bytes? document?)
;; `read-xml` doesn't handle DOCTYPE, and some HTML
;; files can have non-XML-compatible DOCTYPE forms,
;; so we handle passing the DOCTYPE along unmodified.

(define (srfi-rewrite n work-dir #:repo [repo "UNKNOWN"] #:commit [commit "UNKNOWN"])
  (define s-n
    (srfi-n n))
  (define html
    (path-replace-extension s-n #".html"))
  (define src-dir
    (build-path work-dir s-n))
  (define dest-dir
    (build-path (if (= 5 n)
                    nonfree:srfi-std/
                    free:srfi-std/)
                (if (memv n subdir-srfis)
                    s-n
                    'same)))
  (define transformed-srfi
    (srfi-transform n #:repo repo #:commit commit
                    (call-with-input-file* (build-path src-dir html)
                      read-srfi)))
  (make-directory* dest-dir)
  (when (memv n subdir-srfis)
    (for ([file (in-list (directory-list src-dir))]
          #:when (member (path-get-extension file)
                         aux-file-extensions))
      (copy-file (build-path src-dir file)
                 (build-path dest-dir file)
                 'exists-ok)))
  (call-with-output-file* (build-path dest-dir html)
    #:exists 'truncate/replace
    (λ (out)
      (write-srfi transformed-srfi out))))

(define (write-srfi a-srfi out)
  (parameterize ([empty-tag-shorthand (cons 'wbr html-empty-tags)])
    (match-define (cons dtd doc) a-srfi)
    (write-bytes dtd out)
    (write-xml doc out)
    (newline out)))

(define (read-srfi in)
  (parameterize ([read-comments #t]
                 [xml-count-bytes #f])
    (port-count-lines! in)
    (match-define (list (? bytes? dtd))
      (regexp-match #rx"^<!(?i:DOCTYPE HTML)[^\n>]*>\n" in))
    (cons dtd (read-xml in))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pure functions

;; srfi-transform : (->* [exact-positive-integer? (cons/c bytes? document?]
;;                       [#:repo string? #:commit string?]
;;                       (cons/c bytes? document?))
;; Applies Racket-specific transformations to the parsed representation
;; of a SRFI html document, which should come from the specified upstream
;; repository and commit for the given SRFI number.
;; See the helper functions below for more details about the transformations.
(define (srfi-transform n a-srfi #:repo [repo "UNKNOWN"] #:commit [commit "UNKNOWN"])
  (match-let*
      ([(cons dtd (document (prolog misc1 #f misc2) html-struct misc3))
        a-srfi]
       [html-xexpr
        (parameterize ([xexpr-drop-empty-attributes #f]
                       [permissive-xexprs #f])
          (xml->xexpr html-struct))]
       [`(html ,attrs ,@in-html)
        (relocate-prolog (cons (comment
                                (provenance-comment-string repo commit))
                               (append misc1 misc2))
                         html-xexpr)]
       [html-xexpr
        `(html ,attrs
               ,@(map (match-lambda
                        [`(head ,attrs ,@in-head)
                         `(head ,attrs ,@(transform-in-head n in-head))]
                        [`(body ,attrs ,@in-body)
                         `(body ,attrs ,@(transform-in-body n in-body))]
                        [other
                         other])
                      in-html))]
       [html-struct
        (xexpr->xml html-xexpr)])
    (cons dtd (document (prolog '() #f '()) html-struct misc3))))

;; relocate-prolog: (-> (listof (or/c comment? p-i?)) xexpr/c xexpr/c)
;; Browsers expect to find <meta charset="utf-8"/>
;; within the first 1024 bytes of a document,
;; but comments in the prolog---especially with our comment
;; giving the provenance of the document---it might not come soon enough.
;; So, we move <meta charset="utf-8"/> to be the first child of the
;; <head> and we move the prolog, beginning with our comment,
;; to immediately follow.
(define (relocate-prolog prolog html)
  (match-define (list* 'html
                       html-attrs
                       before-head ...
                       `(head ,head-attrs ,@in-head)
                       after-head)
    html)
  ;; Remove the existing charset declaration,
  ;; plus whitespace following it.
  ;; Invariant: there must be an existing <meta charset="utf-8"/>.
  (define in-head/filtered
    (let loop ([in-head in-head])
      (match in-head
        [(cons '(meta ([charset "utf-8"])) in-head)
         (dropf in-head string?)]
        [(cons this in-head)
         (cons this (loop in-head))])))
  `(html ,html-attrs
         ,@before-head
         (head ,head-attrs
               "\n   "
               (meta ([charset "utf-8"]))
               "\n"
               ,@(add-between prolog "\n")
               ,@in-head/filtered)
         ,@after-head))

;; transform-in-head: (-> exact-positive-integer? (listof xexpr/c) (listof xexpr/c))
;; Transforms the children of the <head> element for the given SRFI number.
;; The Racket documentation needs to be usable offline and when
;; accessed directly from the filesystem, without a server.
;; Thus, we can't use root-relative urls like "/favicon.png"
;; (and, by convention, Racket docs don't set their favicon anyway,
;; inheriting it from the site where they are hosted, instead).
;; Furthermore, the upstream "srfi.css" uses Google Fonts,
;; which won't work offline. We already have web fonts for the Racket
;; documentation, so we provide an alternative stylesheet that
;; uses those, instead.
(define (transform-in-head n xs)
  (map (match-lambda
         [`(link ,(list-no-order '[rel "stylesheet"] _ ...) ,@_)
          `(link ([rel "stylesheet"]
                  [type "text/css"]
                  [href ,(cond
                           [(= 5 n)
                            "../../srfi/srfi-std/racket-srfi.css"]
                           [(member n subdir-srfis)
                            "../racket-srfi.css"]
                           [else
                            "racket-srfi.css"])]))]
         [`(link ,(list-no-order '[rel "icon"] _ ...) ,@_)
          ;; Racket documentation pages don't have a fixed favicon,
          ;; so we can reuse this position for doc-site.js
          `(script ([type "text/javascript"]
                    [src ,(string-append
                           (if (member n subdir-srfis)
                               "../"
                               "")
                           "../../doc-site.js")]))]
         [other
          other])
       xs))

;; transform-in-body: (-> exact-positive-integer? (listof xexpr/c) (listof xexpr/c))
;; Transforms the children of the <body> element for the given SRFI number.
;; Tasks:
;;   - In the <h1>, replace the SRFI logo with the text "SRFI ".
;;     In addition to making things work offline (which we could do by distributing
;;     the logo file), there are issues for non-graphical user agents: see
;;     <https://github.com/scheme-requests-for-implementation/srfi-29/pull/1#issue-1052449297>.
;;   - Rewrite URLs: see `transform-urls-in-body`.
;;   - Add notes about PLT-specific extensions: see `inject-plt-extensions`.
;;   - Add a note after the SRFI title and author explaining how
;;     this copy of the SRFI is distributed: see `make-racket-srfi-note`.
(define (transform-in-body n xs)
  (match-define (list* pre ...
                       (list* 'h1
                              h1-attrs
                              h1-pre ...
                              `(a ([href "https://srfi.schemers.org/"])
                                  (img ,_ ,@_))
                              h1-post)
                       post)
    xs)
  (match-define-values [(list between-h1-and-author ...)
                        (list author after-author ...)]
    (splitf-at post (match-lambda
                      [`(p ,_ ,@_)
                       #f]
                      [_
                       #t])))
  `(,@pre
    (h1 ,h1-attrs ,@h1-pre "SRFI " ,@h1-post)
    ,@between-h1-and-author
    ,author
    "\n"
    ,(make-racket-srfi-note n)
    ,@(inject-plt-extensions
       n (transform-urls-in-body n after-author))))
          
;; inject-plt-extensions: (-> exact-positive-integer? (listof xexpr/c) (listof xexpr/c))
;; Adds notes about PLT-specific extensions.
;; Currently, the only such note is about SRFI 19's `string->date`.
(define (inject-plt-extensions n xs)
  (cond
    [(not (= 19 n))
     xs]
    [else
     (map (match-lambda
            [(list* 'dl
                    dl-attrs
                    pre ...
                    (and the-dt
                         `(dt ,(list-no-order '[id "string->date"] _ ...)  ,@_))
                    post)
             (match post
               [(list* post-pre ... `(dd ,dd-attrs ,@dd-body) post-post)
                `(dl ,dl-attrs
                     ,@pre
                     ,the-dt
                     ,@post-pre
                     (dd ,dd-attrs
                         ,@dd-body
                         @p[([class "srfi-plt-extension"])]{
                                   @b{PLT-specific extension:} The @code{~?} wildcard is
                                   specific to the PLT implementation of @code{string->date}:
                                   it parses 1 and 2 digit years like @code{~y} and 3 and 4
                                   digit years like @code{~Y}.})
                     ,@post-post)])]
            [`(table ,(and attrs (list-no-order
                                  '[summary "STRING->DATE conversion specifiers"]
                                  _ ...))
                     ,@rows)
             (define-values [rows-before rows-after]
               ;; the last three rows are footers (but there are strings, too)
               (let loop ([rows (reverse rows)]
                          [footer-count 0]
                          [rows-after '()])
                 (match rows
                   [(cons (and this (cons 'tr _)) rows)
                    (if (= 2 footer-count)
                        (values (reverse rows)
                                (cons this rows-after))
                        (loop rows
                              (add1 footer-count)
                              (cons this rows-after)))]
                   [(cons this rows)
                    (loop rows footer-count (cons this rows-after))])))
             `(table
               ,attrs
               ,@rows-before
               (tr ([class "srfi-plt-extension"])
                   (td ([width "6%"])
                       (code () "~?"))
                   (td ([width "23%"])
                       "char-numeric?")
                   (td ([width "50%"])
                       "2-digit or 4-digit year "
                       "(PLT-specific extension)")
                   (td ([width "23%"])
                       (code () "date-year")))
               "\n"
               ,@rows-after)]
            [other
             other])
          xs)]))

;; transform-urls-in-body: (-> exact-positive-integer? (listof xexpr/c) (listof xexpr/c))
;; Recursively walks the given list of xexprs for SRFI N,
;; calling `transform-url-string` on the value of each
;; `href` or `src` attribute.
(define (transform-urls-in-body n xs)
  (define transform-child
    (match-lambda
      [(list* name attrs xs)
       (list* name
              (map (match-lambda
                     [(list (and name (or 'href 'src)) u)
                      (list name (transform-url-string n u))]
                     [other
                      other])
                   attrs)
              (map transform-child xs))]
      [other
       other]))
  (map transform-child xs))

;; transform-url-string: (-> exact-positive-integer? string? string?)
;; Returns a replacement for the given URL string relative to
;; SRFI N in the Racket documentation.
;; The URL is transformed if it is a relative URL or if it explicitly
;; links to "srfi.schemers.org": otherwise, it is returned unmodified.
;; The transformation accounts for several factors:
;;   - The Racket documentation must work when accessed from the filesystem,
;;     without a server, so we cannot use root-relative URLs.
;;   - The layout of URLs in the Racket documentation is different than
;;     at "srfi.schemers.org", e.g. most SRFIs are not in subdirectories.
;;   - We do not distribute auxiliary files like sample implementations
;;     and test suites, so we must change relative URLs for such files
;;     to absolute URLs at "srfi.schemers.org".
;;   - When a "srfi.schemers.org" URL refers to a file we DO distribute,
;;     we should rewrite the link to a relative URL to our copy.
(define (transform-url-string n url-str)
  (match (string->url url-str)
    [(struct* url ([scheme (or #f "http" "https")]
                   [host (or #f "srfi.schemers.org")]
                   [path-absolute? path-absolute?]
                   [path (list (path/param raw-path-elems '())
                               ...)]
                   [query query]
                   [fragment fragment]))
     (define absolute-path-elems
       (normalize-url-elements n raw-path-elems
                               #:path-absolute? path-absolute?))
     ;; We will construct either a relative URL to a locally-installed
     ;; file or, if there isn't one, an absolute URL to the file on the web.
     (define (rebuild-url absolute? path-elems)
       (url (and absolute? "https")
            #f
            (and absolute? "srfi.schemers.org")
            #f
            absolute?
            (map (lambda (elem)
                   (path/param elem '()))
                 path-elems)
            query
            fragment))
     (url->string
      (cond
        [(find-relative-url-elements n absolute-path-elems)
         => (λ (relative-path-elems)
              (rebuild-url #f relative-path-elems))]
        [else
         (rebuild-url #t absolute-path-elems)]))]
    [_
     url-str]))

;; normalize-url-elements: (-> exact-positive-integer?
;;                             (listof string?)
;;                             #:path-absolute? boolean?
;;                             (listof string?))
;; Converts a list of path elements linked from the specified SRFI
;; to an equivalent absolute path from the root of "srfi.schemers.org".
;; If the original path referred to a directory, with or without
;; a trailing /, or to an "index.html" file, the resulting list
;; will consistently end with "".
;; This implementation is not fully general---it would be nice to have a
;; `simplify-url` analagous to `simplify-path`, as discussed in [1]---but
;; it handles all of the ways that such URLs are actually written in the
;; relevant SRFI documents.
;; [1]: https://github.com/racket/web-server/issues/59#issuecomment-476854415
(define (normalize-url-elements n elems #:path-absolute? path-absolute?)
  (let* ([elems
          (match elems
            [(cons 'up (and elems (list (? string?) ...)))
             #:when (not path-absolute?)
             elems]
            [(list (? string?) ...)
             (if path-absolute?
                 elems
                 (cons (srfi-n n) elems))])]
         [elems
          (match elems
            [(list elems ... (or "" "index.html"))
             `(,@elems "")]
            [(list dir)
             #:when (not (path-get-extension dir))
             (list dir "")]
            [_
             elems])])
    elems))

;; find-relative-url-elements: (-> exact-positive-integer?
;;                                 (listof string?)
;;                                 (or/c #f (listof string?)))
;; Given a SRFI number and a normalized list of path elements
;; from the root of "srfi.schemers.org" (see `normalize-url-elements`),
;; returns a list of path elements to the installed location of the
;; corresponding file in the Racket documentation from the installed
;; location of the specified SRFI document.
;; If the given path elements do not refer to a file that will be installed
;; with the Racket documentation, returns #f.
;; More precisely, if we distribute documentation for SRFI X,
;; we also treat '("srfi-X" "") as though it were '("srfi-X" "srfi-X.html"),
;; even though the former is the landing page rather than the specification
;; document, because it seems more useful to go to the locally installed file:
;; we handle that transformation here so that, if we DON'T distribute SRFI X, we
;; can preserve the distinction, since we'd be linking to a non-local file anyway.
(define (find-relative-url-elements from-n absolute-elems)
  (match absolute-elems
    [(list (and dir (pregexp #px"^srfi-(\\d+)$"
                             (list _ (app string->number target-n))))
           file)
     #:when (and (memv target-n the-srfis)
                 (or (not (= 5 target-n))
                     (= 5 from-n))
                 (or (equal? file "")
                     (equal? file (string-append dir ".html"))
                     (and (memv target-n subdir-srfis)
                          (member (path-get-extension file)
                                  aux-file-extensions))))
     (let ([file (if (equal? "" file)
                     (string-append dir ".html")
                     file)])
       (cond
         [(= from-n target-n)
          (if (equal? file (string-append dir ".html"))
              null
              (list file))]
         [else
          `(,@(cond
                [(memv from-n subdir-srfis)
                 '(up)]
                [(= 5 from-n)
                 '(up up "srfi" "srfi-std")]
                [else
                 '()])
            ,@(if (memv target-n subdir-srfis)
                  (list dir)
                  '())
            ,file)]))]
    [_
     #f]))

;; make-racket-srfi-note: (-> exact-positive-integer? xexpr/c)
;; Generates a note for SRFI N explaining how Racket distributes
;; this copy and linking to the upstream document.
(define (make-racket-srfi-note n)
  (define n-str (number->string n))
  (define upstream-url
    (format "https://srfi.schemers.org/srfi-~a/srfi-~a.html" n n))
  (define-syntax-rule (splice-when expr form0 form ...)
    (if expr
        `(form0 form ...)
        '()))
  ;; like @margin-note{}
  `(blockquote ([class "refpara racket-srfi-note"])
               (blockquote ([class "refcolumn"])
                           (blockquote ([class "refcontent"])
                                       @p{
 This copy of the SRFI @,n-str specification document
 is distributed as part of the Racket package
 @(a ([href ,(if (memv n subdir-srfis)
                 "../../index.html"
                 "../index.html")])
     (span ([class "stt"])
           ,(if (= 5 n)
                "srfi-doc-nonfree"
                "srfi-doc")))@,@splice-when[(= 5 n)]{,
  which is not included in the main Racket distribution}.
}
                                       @,@(splice-when (= 5 n) @p{
 @; like what @deprecated{} generates:
 @(b ([style "background-color: yellow;"]) "NOTE:")
 For @(a ([href ,srfi-license-history-url]) "historical reasons"),
 the SRFI 5 specification document has a
 @(a ([href "#copyright"]) "restrictive license").
 Racket provides a free implementation of SRFI 5 with
 @(a ([href "../../srfi/srfi-5.html"]) "free documentation"):
 only this specification document is restrictively licensed.
 })
                                       @p{
 The canonical source of this document is
 @(a ([href ,upstream-url]) ,upstream-url).
 }))))

(define (provenance-comment-string repo commit)
  ;; we assume the GitHub owner will be no longer than
  ;; "scheme-requests-for-implementation"
  (define (with-box-padding repo/commit)
    (define spaces-needed
      (- 63 (string-length repo/commit)))
    (string-append repo/commit (make-string spaces-needed #\space) "┃"))
  @string-append-immutable["\n"]{
 ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
 ┃ This file was automatically transformed for Racket.                      ┃
 ┃                                                                          ┃
 ┃   Source: @with-box-padding[repo]
 ┃   Commit: @with-box-padding[commit]
 ┃                                                                          ┃
 ┃ Please DO NOT edit this file by hand.                                    ┃
 ┃ Whenever possible, contribute changes upstream.                          ┃
 ┃ For Racket-specific changes, edit the transformation script:             ┃
 ┃                                                                          ┃
 ┃   "srfi-sync.rkt" in https://github.com/racket/srfi                      ┃
 ┃                                                                          ┃
 ┃ and regenerate all of these files.                                       ┃
 ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
 @""})
