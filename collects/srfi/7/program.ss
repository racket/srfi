;;;
;;; <program.ss> ---- SRFI-7 program
;;; Time-stamp: <03/02/24 11:20:35 solsona>
;;;
;;; This was ported from the SRFI-7 reference implementation by
;;; Richard Kelsey, for more information please visit:
;;;
;;;   http://srfi.schemers.org/srfi-7/

(module program mzscheme
  (require (lib "0.ss" "srfi"))
  (provide program)
  
  (define-syntax program
    (syntax-rules (requires files code feature-cond)
		  ((program)
		   (begin))
		  ((program (requires feature-id ...)
			    more ...)
		   (begin (cond-expand ((and feature-id ...) 'okay))
			  (program more ...)))
		  ((program (files filename ...)
			    more ...)
		   (begin (load filename) ...
			  (program more ...)))
		  ((program (code stuff ...)
			    more ...)
		   (begin stuff ...
			  (program more ...)))
		  ((program (feature-cond (requirement stuff ...) ...)
			    more ...)
		   (begin (cond-expand (requirement (program stuff ...)) ...)
			  (program more ...)))))

  )

;;; program.ss ends here