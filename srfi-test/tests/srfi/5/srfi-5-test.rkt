#lang racket/base

(require rackunit
         syntax/macro-testing
         (rename-in srfi/5
                    [let s:let])
         (rename-in racket/base
                    [let standard-let]))

(provide srfi-5-tests)

(define srfi-5-tests
  (test-suite
   "srfi/5 let tests"
   (test-case
    "no loop"
    (check-equal? (s:let () 5)
                  5
                  "no bindings")
    (check-equal? (s:let ([x 5]) x)
                  5
                  "one binding: treated normally")
    (check-equal? (s:let ([x 40][y 2]) (+ x y))
                  42
                  "two bindings: treated normally")
    (check-equal? (s:let ([x 1] . [y 2])
                         (cons x y))
                  '(1 2)
                  "rest binding w/ 1 value")
    (check-equal? (s:let ([x 1] y 2)
                         (cons x y))
                  '(1 2)
                  "rest binding w/ 1 value (no dot)")
    (check-equal? (s:let ([x 1] . [y 2 3])
                         (cons x y))
                  '(1 2 3)
                  "rest binding w/ multiple values")
    (check-equal? (s:let ([x 1] y 2 3)
                         (cons x y))
                  '(1 2 3)
                  "rest binding w/ multiple values (no dot)")
    (check-exn exn:fail:syntax?
               (λ () (convert-syntax-error (s:let ([x 1 2]) x)))
               "malformed binding pair is an error"))

   (test-case
    "\"named let\"-style loop"
    (check-equal? (s:let loop () 1)
                  1
                  "loop w/ no bindings is ok")
    (check-equal? (s:let loop ([continue? #t])
                         (if continue?
                             (cons continue? (loop #f))
                             (list continue?)))
                  '(#t #f)
                  "loop w/ 1 normal arg")
    (check-equal? (s:let loop ([continue? #t]
                               [x 1])
                         (if continue?
                             (cons x (loop #f 2))
                             (list x)))
                  '(1 2)
                  "loop w/ 2 normal args")
    (check-equal? (s:let loop ([continue? 0]
                               . [args 'a])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding w/ 1 initial value")
    (check-equal? (s:let loop ([continue? 0] args 'a)
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding w/ 1 initial value (no dot)")
    (check-equal? (s:let loop ([continue? 0]
                               . [args 'a 'a1 'a2])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a a1 a2) (b) (c d))
                  "rest binding w/ multiple initial values")
    (check-equal? (s:let loop ([continue? 0] args 'a 'a1 'a2)
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a a1 a2) (b) (c d))
                  "rest binding w/ multiple initial values (no dot)")
    (check-equal? (s:let loop (x)
                         x)
                  '()
                  "rest binding alone w/ 0 initial values")
    (check-equal? (s:let loop (x 1)
                         x)
                  '(1)
                  "rest binding alone w/ 1 initial value")
    (check-equal? (s:let loop (x 1 2)
                         x)
                  '(1 2)
                  "rest binding alone w/ multiple initial values")
    (check-equal? (s:let a (b (+ 1))
                         b)
                  '(1)
                  ;; "confusable" here means that the expression (+ 1)
                  ;; could be confused for a binding pair
                  "rest binding alone w/ confusable initial value")
    (check-exn (λ (e)
                 (and (exn:fail:syntax? e)
                      (regexp-match? #rx"^x: unbound" (exn-message e))))
               (λ ()
                 (convert-syntax-error
                  (s:let a (b [x 1]) 2)))
               "rest binding alone: correct error when confused w/ define")
    (check-pred procedure?
                (s:let loop (args 1 2 3)
                       loop)
                "rest binding alone: does bind procedure")
    (check-exn exn:fail:syntax?
               (λ () (convert-syntax-error (s:let loop ([x 1 2]) x)))
               "malformed binding pair is an error"))

   (test-case
    "\"define\"-style loop"
    (check-equal? (s:let (loop) 1)
                  1
                  "loop w/ no bindings is ok")
    (check-equal? (s:let (loop [continue? #t])
                         (if continue?
                             (cons continue? (loop #f))
                             (list continue?)))
                  '(#t #f)
                  "loop w/ 1 normal arg")
    (check-equal? (s:let (loop [continue? #t]
                               [x 1])
                         (if continue?
                             (cons x (loop #f 2))
                             (list x)))
                  '(1 2)
                  "loop w/ 2 normal args")
    (check-equal? (s:let (loop [continue? 0] . [args])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '(() (b) (c d))
                  "rest binding w/ 0 initial values")
    (check-equal? (s:let (loop [continue? 0] args)
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '(() (b) (c d))
                  "rest binding w/ 0 initial values (no dot)")
    (check-equal? (s:let (loop [continue? 0]
                               . [args 'a])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding w/ 1 initial value")
    (check-equal? (s:let (loop [continue? 0] args 'a)
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding w/ 1 initial value (no dot)")
    (check-equal? (s:let (loop [continue? 0] args 'a 'a1 'a2)
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a a1 a2) (b) (c d))
                  "rest binding w/ multiple initial values (no dot)")
    (check-equal? (s:let (loop . [args])
                         (case args
                           [(()) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '(() (b) (c d))
                  "rest binding alone w/ 0 initial values")
    (check-equal? (s:let (loop args)
                         (case args
                           [(()) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '(() (b) (c d))
                  "rest binding alone w/ 0 initial values (no dot)")
    (check-equal? (s:let (loop . [args 'a])
                         (case args
                           [((a)) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding alone w/ 1 initial value")
    (check-equal? (s:let (loop args 'a)
                         (case args
                           [((a)) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding alone w/ 1 initial value (no dot)")
    (check-equal? (s:let (loop . [args 'a1 'a2 'a3])
                         (case args
                           [((a1 a2 a3)) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '((a1 a2 a3) (b) (c d))
                  "rest binding alone w/ multiple initial values")
    (check-equal? (s:let (loop args 'a1 'a2 'a3)
                         (case args
                           [((a1 a2 a3)) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '((a1 a2 a3) (b) (c d))
                  "rest binding alone w/ multiple initial values (no dot)"))
   (test-case
    "shaddowing"
    ;; See: https://srfi-email.schemers.org/srfi-5/msg/18712014/
    (check-equal? (s:let x ((x 1)) x)
                  1
                  "let-style: loop id can be shaddowed")
    (check-equal? (s:let (x (x 1)) x)
                  1
                  "define-style: loop id can be shaddowed")
    (check-eq? (object-name (s:let f () f))
               'f
               "let-style object name")
    (check-eq? (object-name (s:let (g) g))
               'g
               "define-style object name")
    (check-exn #rx"duplicate"
               (λ () (convert-syntax-error (s:let loop ((x 1) x) #t)))
               "let-style: check duplicate ids")
    (check-exn #rx"duplicate"
               (λ () (convert-syntax-error (s:let (loop (x 1) x) #t)))
               "define-style: check duplicate ids")
    (check-exn #rx"duplicate"
               (λ () (convert-syntax-error (s:let ((x 1) (x 2)) #t)))
               "non-loop: check duplicate ids"))
   (test-case
    "ambiguous \"define\"-style loop: preserve compatible behavior"
    ;; See: https://srfi-email.schemers.org/srfi-5/msg/18709896/
    (check-pred procedure?
                (s:let (ambiguous (+ 1) (- 2) (list 5))
                  ambiguous)
                "ambiguous is not a rest binding")
    (check-equal? (s:let (ambiguous (+ 1) (- 2) (abs -7))
                    (list + - abs))
                  '(1 2 -7)
                  "binds identifiers"))))
