;; module loader for SRFI-13
(module |13| mzscheme
  (require (lib "string.ss" "srfi" "13"))
  (provide (all-from-except (lib "string.ss" "srfi" "13")
			    s:string-copy!)
	   (rename s:string-copy! string-copy!)))
