;; module loader for SRFI-1
(module |1| mzscheme
  (require (all-except (lib "list.ss" "srfi" "1")
		       append! reverse!
		       map for-each
		       member
		       assoc)
	   (rename (lib "list.ss" "srfi" "1") s:append! append!)
	   (rename (lib "list.ss" "srfi" "1") s:reverse! reverse!)
	   (rename (lib "list.ss" "srfi" "1") s:map map)
	   (rename (lib "list.ss" "srfi" "1") s:for-each for-each)
	   (rename (lib "list.ss" "srfi" "1") s:member member)
	   (rename (lib "list.ss" "srfi" "1") s:assoc assoc))
  
  (provide (all-from-except (lib "list.ss" "srfi" "1")
		       s:append! s:reverse!
		       s:map s:for-each
		       s:member
		       s:assoc)
	   (rename s:append! append!)
	   (rename s:reverse! reverse!)
	   (rename s:map map)
	   (rename s:for-each for-each)
	   (rename s:member member)
	   (rename s:assoc assoc)))
			    
