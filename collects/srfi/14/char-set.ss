
;; Since Matthew implemented this module, tests are in
;;  plt/collects/tests/mzscheme/char-set.ss.

(module char-set mzscheme
  (require (lib "integer-set.ss")
	   (all-except (lib "contract.ss") union)
	   (rename (lib "contract.ss") union/c union))

  ;; Data defn ----------------------------------------
    
  (define-struct char-set (set/thunk))

  (define (fold-set op init l)
    (if (null? l)
	init
	(fold-set op (op init (car l)) (cdr l))))

  (define (char-set-set cs)
    (let ([v (char-set-set/thunk cs)])
      (if (procedure? v)
	  (let ([v2 (v)])
	    (set-char-set-set/thunk! cs v2)
	    v2)
	  v)))

  ;; General procedures ----------------------------------------
  
  (define char-set=
    (case-lambda
     [() #t]
     [(cs) #t]
     [(cs1 cs2) (equal? (integer-set-contents (char-set-set cs1))
			(integer-set-contents (char-set-set cs2)))]
     [(cs1 . rest) (fold-set (lambda (v cs) (and v (char-set= cs1 cs))) #t rest)]))
  
  (define char-set<=
    (case-lambda
     [() #t]
     [(cs) #t]
     [(cs1 cs2) (subset? (char-set-set cs1) (char-set-set cs2))]
     [(cs1 . rest) (and (fold-set (lambda (cs1 cs) (and cs1 (char-set<= cs1 cs) cs)) cs1 rest) #t)]))
  
  (define char-set-hash
    (case-lambda
     [(cs)
      (abs (equal-hash-code (char-set-set cs)))]
     [(cs bound)
      (modulo (char-set-hash cs) bound)]))

  ;; Iterating over character sets ----------------------------------------

  ;; A cursor is (cons num (list-of (cons start-num end-num)))
  ;;  where the first num indicates how far we are into the
  ;;; first range of the cdr of the cursor.

  (define (char-set-cursor cs)
    (cons 0 (integer-set-contents (char-set-set cs))))

  (define (char-set-ref cs c)
    (integer->char (+ (car c) (caadr c))))

  (define (char-set-cursor-next cs c)
    (let ([d (- (cdadr c) (caadr c))])
      (if (= d (car c))
	  (cons 0 (cddr c))
	  (cons (add1 (car c)) (cdr c)))))

  (define (end-of-char-set? c)
    (null? (cdr c)))

  (define (char-set-fold/done kons knil cs done?)
    (let loop ([v knil][l (integer-set-contents (char-set-set cs))])
      (if (null? l)
	  v
	  (let ([end (cdar l)])
	    (let iloop ([v v][i (caar l)])
	      (if (i . > . end)
		  (loop v (cdr l))
		  (let ([v (kons (integer->char i) v)])
		    (if (done? v)
			v
			(iloop v (add1 i))))))))))

  (define (char-set-fold kons knil cs)
    (char-set-fold/done kons knil cs (lambda (x) #f)))

  (define char-set-unfold
    (case-lambda
     [(f p g seed) (char-set-unfold f p g seed char-set:empty)]
     [(f p g seed base-cs)
      ;; Implementation taken directly from SRFI-14:
      (let lp ((seed seed) (cs base-cs))
        (if (p seed) 
	    cs ; P says we are done.
            (lp (g seed) ; Loop on (G SEED).
                (char-set-adjoin! cs (f seed)))))]))

  (define (char-set-unfold! f p g seed base-cs)
    (char-set-unfold f p g seed base-cs))
  
  (define (char-set-for-each proc cs)
    (char-set-fold (lambda (c v) (proc c)) (void) cs))

  (define (char-set-map proc cs)
    ;; Note: no order defined on cs traversal, so it doesn't
    ;;  matter that we build up the list backward
    (char-set-fold (lambda (c v) (cons (proc c) v)) null cs))


  ;; Creating character sets ----------------------------------------

  (define (char-set-copy cs) 
    ;; Our char sets are purely functional:
    cs)

  (define mk-char-set
    (let ([char-set (lambda more
		      (list->char-set more char-set:empty))])
      char-set))

  (define list->char-set 
    (case-lambda
     [(l) (list->char-set l char-set:empty)]
     [(l cs) (fold-set char-set-adjoin cs l)]))
  (define (list->char-set! l cs)
    (list->char-set l cs))

  (define string->char-set
    (case-lambda
     [(s) (string->char-set s char-set:empty)]
     [(s cs) (list->char-set (string->list s) cs)]))
  (define (string->char-set! s cs)
    (string->char-set s cs))

  (define char-set-filter
    (case-lambda
     [(pred cs) (char-set-filter pred cs char-set:empty)]
     [(pred cs base-cs)
      (char-set-fold (lambda (c v) (if (pred c)
				       (char-set-adjoin v c)
				       v))
		     base-cs
		     cs)]))
  (define (char-set-filter! pred cs base-cs)
    (char-set-filter pred cs base-cs))

  (define ucs-range->char-set
    (case-lambda
     [(lower upper) (ucs-range->char-set lower upper #f char-set:empty)]
     [(lower upper error?) (ucs-range->char-set lower upper error? char-set:empty)]
     [(lower upper error? cs)
      (when (or (lower . < . 0)
		(upper . > . #x200000))
	(error 'ucs-range->char-set "invalid range: [~a, ~a)" lower upper))
      (char-set-union cs
		      (if (or (upper . <= . #xD800)
			      (lower . > . #E0000))
			  ;; No holes
			  (make-char-set (make-integer-set (list (cons lower (sub1 upper)))))
			  ;; Spans the hole:
			  (make-char-set (make-integer-set (list (cons lower #xD7FF)
								 (cons #E000 (sub1 upper)))))))]))
  (define (ucs-range->char-set! lower upper error? base-cs)
    (ucs-range->char-set lower upper error? base-cs))

  (define (->char-set x)
    (cond
     [(char? x) (let ([v (char->integer x)])
		  (make-char-set (make-integer-set (list (cons v v)))))]
     [(string? x) (string->char-set x)]
     [(char-set? x) x]
     [else (raise-type-error '->char-set "character, string, or char-set" x)]))

  ;; Querying character sets ----------------------------------------

  (define (char-set-size cs)
    (let loop ([l (integer-set-contents (char-set-set cs))][c 0])
      (if (null? l)
	  c
	  (loop (cdr l) (+ c 1 (- (cdar l) (caar l)))))))

  (define (char-set-count pred cs)
    (char-set-fold (lambda (c v)
		     (+ v (if (pred c) 1 0)))
		   0
		   cs))

  (define (char-set->list cs)
    (char-set-fold cons null cs))
    
  (define (char-set->string cs)
    (list->string (char-set->list cs)))

  (define (char-set-contains? cs char)
    (member? (char->integer char) (char-set-set cs)))
  
  (define (char-set-every pred cs)
    (char-set-fold/done (lambda (c v)
			  (and v
			       (pred c)))
			#t
			cs
			not))

  (define (char-set-any pred cs)
    (char-set-fold/done (lambda (c v)
			  (or v
			      (pred c)))
			#f
			cs
			values))
  
  ;; Character-set algebra ----------------------------------------

  (define char-set-adjoin 
    (case-lambda
     [(cs char1) 
      (let ([v (char->integer char1)])
	(make-char-set (union (char-set-set cs)
			      (make-integer-set (list (cons v v))))))]
     [(cs . more)
      (fold-set char-set-adjoin cs more)]))

  (define char-set-delete 
    (case-lambda
     [(cs char1) 
      (let ([v (char->integer char1)])
	(make-char-set (difference (char-set-set cs)
				   (make-integer-set (list (cons v v))))))]
     [(cs . more)
      (fold-set char-set-delete cs more)]))

  (define (char-set-complement cs)
    (make-char-set
     (union (complement (char-set-set cs) #x0 #xD7FF)
	    (complement (char-set-set cs) #xE000 #x1FFFFF))))

  (define-syntax define-set-op
    (syntax-rules ()
      [(_ char-set-op set-op)
       (define char-set-op 
	 (case-lambda
	  [(cs1 cs2)
	   (make-char-set (set-op (char-set-set cs1) (char-set-set cs2)))]
	  [(cs1 . more)
	   (fold-set char-set-op cs1 more)]))]))

  (define-set-op char-set-union union)
  (define-set-op char-set-intersection intersect)
  (define-set-op char-set-difference difference)
  (define-set-op char-set-xor xor)

  (define char-set-diff+intersection
    (case-lambda
     [(cs1 cs2)
      (let-values ([(cs1^cs2 cs1-cs2 cs2-cs1) (split (char-set-set cs1) (char-set-set cs2))])
	(values (make-char-set cs1-cs2)
		(make-char-set cs1^cs2)))]
     [(cs1 cs2 . more)
      (let-values ([(d i) (char-set-diff+intersection cs1 cs2)])
	(values (apply char-set-difference d more)
		(apply char-set-intersection i more)))]))

  (define char-set-adjoin! char-set-adjoin)
  (define char-set-delete! char-set-delete)
  (define char-set-complement! char-set-complement)
  (define char-set-union! char-set-union)
  (define char-set-intersection! char-set-intersection)
  (define char-set-difference! char-set-difference)
  (define char-set-xor! char-set-xor)
  (define char-set-diff+intersection! char-set-diff+intersection)

  ;; ----------------------------------------

  ;; MzScheme provides a rough map to unicode:
  (define unicode (make-known-char-range-list))

  (define (make-standard-set pred?)
    (make-char-set
     (lambda ()
       (make-integer-set
	(let loop ([l unicode])
	  (cond
	   [(null? l) null]
	   [(caddar l) 
	    ;; Every char in this range has the same properites
	    (if (pred? (integer->char (caar l)))
		(cons (cons (caar l) (cadar l)) (loop (cdr l)))
		(loop (cdr l)))]
	   [else
	    ;; Check char-by-char:
	    (let ([end (cadar l)])
	      (let no-loop ([v (caar l)])
		(cond
		 [(v . > . end)
		  ;; None in this range
		  (loop (cdr l))]
		 [(pred? (integer->char v))
		  ;; Found a char in this range
		  (let yes-loop ([v2 (add1 v)])
		    (cond
		     [(v2 . > . end)
		      ;; Went to end
		      (cons (cons v (sub1 v2)) (loop (cdr l)))]
		     [(pred? (integer->char v2))
		      (yes-loop (add1 v2))]
		     [else
		      ;; Found end of sub-range; treat the rest
		      ;;  of this range as a new range
		      (cons (cons v (sub1 v2))
			    (loop (cons (list v2 end #f) (cdr l))))]))]
		 [else
		  ;; Still looking for a char in this range
		  (no-loop (add1 v))])))]))))))

  (define char-set:lower-case
    (make-standard-set char-lower-case?))
  (define char-set:upper-case
    (make-standard-set char-upper-case?))
  (define char-set:title-case
    (make-standard-set char-title-case?))
  (define char-set:letter
    (make-standard-set char-alphabetic?))
  (define char-set:digit
    (make-standard-set char-numeric?))
  (define char-set:letter+digit
    (char-set-union char-set:letter char-set:digit))
  (define char-set:graphic
    (make-standard-set char-graphic?))
  (define char-set:whitespace
    (make-standard-set char-whitespace?))
  (define char-set:printing
    (char-set-union char-set:whitespace char-set:graphic))
  (define char-set:iso-control
    (make-standard-set char-iso-control?))
  (define char-set:punctuation
    (make-standard-set char-punctuation?))
  (define char-set:symbol
    (make-standard-set char-symbolic?))
  (define char-set:blank
    (make-standard-set char-blank?))
  (define char-set:ascii
    (make-char-set (make-integer-set '((0 . 127)))))
  (define char-set:hex-digit
    (make-char-set (make-integer-set '((48 . 57) (65 . 70) (97 . 102)))))
  (define char-set:empty
    (make-char-set (make-integer-set '())))
  (define char-set:full
    (make-char-set (make-integer-set '((#x0 . #xD7FF) (#xE000 . #x1FFFFF)))))

  

  ;; Contracts and provides ----------------------------------------

  (define char-sets/c 
    (case-> (char-set? char-set? . -> . any)
	    ((char-set?) (listof char-set?) . ->* . any)))
  (define char-sets+/c
    (case-> (char-set? char-set? . -> . any)
	    ((char-set? char-set?) (listof char-set?) . ->* . any)))

  (define char-set-filter/c
    (((char? . -> . any) char-set?) (char-set?) . opt-> . char-set?))

  (define ei/c (and/c number? integer? exact?))

  (define char-set-char/c
    ((char-set?) (listof char?) . ->* . (char-set?)))

  (provide
   char-set?)
  (provide/contract
   [char-set= char-sets/c]
   [char-set<= char-sets/c]
   [char-set-hash (char-set? . -> . any)]
   [char-set-cursor (char-set? . -> . any)]
   [char-set-ref (char-set? any/c . -> . char?)]
   [char-set-cursor-next (char-set? any/c . -> . any)]
   [end-of-char-set? (any/c . -> . boolean?)]
   [char-set-fold ((char? any/c . -> . any) any/c char-set? . -> . any)]
   [char-set-unfold (((any/c . -> . char?) (any/c . -> . any) (any/c . -> . any) any/c) (char-set?) . opt-> . char-set?)]
   [char-set-unfold! ((any/c . -> . char?) (any/c . -> . any) (any/c . -> . any) any/c char-set? . -> . char-set?)]
   [char-set-for-each ((char? . -> . any) char-set? . -> . any)]
   [char-set-map ((char? . -> . any) char-set? . -> . any)]
   [char-set-copy (char-set? . -> . char-set?)]
   [rename mk-char-set char-set (() (listof char?) . ->* . (char-set?))]
   [list->char-set (((listof char?)) (char-set?) . opt-> . char-set?)]
   [list->char-set! ((listof char?) char-set? . -> . char-set?)]
   [string->char-set ((string?) (char-set?) . opt-> . char-set?)]
   [string->char-set! (string? char-set? . -> . char-set?)]
   [char-set-filter char-set-filter/c]
   [char-set-filter! char-set-filter/c]
   [ucs-range->char-set ((ei/c ei/c) (any/c char-set?) . opt-> . char-set?)]
   [ucs-range->char-set! (ei/c ei/c any/c char-set? . -> . char-set?)]
   [->char-set ((union/c string? char? char-set?) . -> . char-set?)]
   [char-set->list (char-set? . -> . any)]
   [char-set->string (char-set? . -> . any)]
   [char-set-size (char-set? . -> . any)]
   [char-set-count ((char? . -> . any) char-set? . -> . any)]
   [char-set-contains? (char-set? char? . -> . any)]
   [char-set-every ((char? . -> . any) char-set? . -> . any)]
   [char-set-any ((char? . -> . any) char-set? . -> . any)]
   [char-set-adjoin char-set-char/c]
   [char-set-adjoin! char-set-char/c]
   [char-set-delete char-set-char/c]
   [char-set-delete! char-set-char/c]
   [char-set-complement (char-set? . -> . char-set?)]
   [char-set-complement! (char-set? . -> . char-set?)]
   [char-set-union char-sets/c]
   [char-set-union! char-sets/c]
   [char-set-intersection char-sets/c]
   [char-set-intersection! char-sets/c]
   [char-set-difference char-sets+/c]
   [char-set-difference! char-sets+/c]
   [char-set-xor char-sets/c]
   [char-set-xor! char-sets/c]
   [char-set-diff+intersection char-sets+/c]
   [char-set-diff+intersection! char-sets+/c])
  (provide
   char-set:lower-case
   char-set:upper-case
   char-set:title-case
   char-set:letter
   char-set:digit
   char-set:letter+digit
   char-set:graphic
   char-set:printing
   char-set:whitespace
   char-set:iso-control
   char-set:punctuation
   char-set:symbol
   char-set:hex-digit
   char-set:blank
   char-set:ascii
   char-set:empty
   char-set:full))
