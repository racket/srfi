;; module loader for SRFI-19
(module |19| (lib "m.ss" "srfi" "19")
  (require (lib "time.ss" "srfi" "19"))
  (provide (all-from (lib "time.ss" "srfi" "19"))))
