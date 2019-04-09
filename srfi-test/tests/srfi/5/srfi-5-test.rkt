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
    (check-equal? (s:let ([x 1] . [y 2 3])
                         (cons x y))
                  '(1 2 3)
                  "rest binding w/ multiple values")
    (check-exn exn:fail:syntax?
               (λ () (convert-syntax-error (s:let ([x 1 2]) x)))
               "rest binding alone is an error"))

   (test-case
    "let-style loop"
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
    (check-equal? (s:let loop ([continue? 0]
                               . [args 'a 'a1 'a2])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a a1 a2) (b) (c d))
                  "rest binding w/ multiple initial values")
    (check-exn exn:fail:syntax?
               (λ () (convert-syntax-error (s:let loop ([x 1 2]) x)))
               "rest binding alone is an error"))

   (test-case
    "define-style loop"
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
    (check-equal? (s:let (loop [continue? 0]
                               . [args 'a])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "rest binding w/ 1 initial value")
    (check-equal? (s:let (loop [continue? 0]
                               . [args 'a 'a1 'a2])
                         (case continue?
                           [(0) (cons args (loop 1 'b))]
                           [(1) (cons args (loop 2 'c 'd))]
                           [else (list args)]))
                  '((a a1 a2) (b) (c d))
                  "rest binding w/ multiple initial values")
    (check-equal? (s:let (loop . [args 'a])
                         (case args
                           [((a)) (cons args (loop 'b))]
                           [((b)) (cons args (loop 'c 'd))]
                           [else (list args)]))
                  '((a) (b) (c d))
                  "define-style loop can have only rest arg"))

   (check-exn exn:fail:syntax?
              (λ ()
                (convert-syntax-error
                 (s:let a (b [x 1]) x)))
              "combining let- and define- style loop names is an error")))
