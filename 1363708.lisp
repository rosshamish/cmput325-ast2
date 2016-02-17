(defun fl-interp (E P)
  (cond 
	((atom E) E)
    (t (let ( (f (car E))  (arg (cdr E)) )
	   (cond 
            ; handle built-in functions
            ((eq f 'if) nil) ; todo if
            ((eq f 'null) (null (fl-interp arg P)))
            ((eq f 'atom) (atom (fl-interp arg P)))
            ((eq f 'eq) nil) ; todo eq
            ((eq f 'first)  (car (fl-interp (car arg) P)))
            ((eq f 'rest) nil) ; todo rest
            ((eq f 'cons) nil) ; todo cons
            ((eq f 'equal) nil) ; todo equal
            ((eq f 'number) nil) ; todo number
            ((eq f '+) nil) ; todo +
            ((eq f '-) nil) ; todo -
            ((eq f '*) nil) ; todo *
            ((eq f '>) nil) ; todo >
            ((eq f '<) nil) ; todo <
            ((eq f '=) nil) ; todo =
            ((eq f 'and) nil) ; todo and
            ((eq f 'or) nil) ; todo or
            ((eq f 'not) nil) ; todo not
            
	        ; if f is a user-defined function,
            ; then evaluate the arguments 
            ; and apply f to the evaluated arguments 
            ; (applicative order reduction) 
            ; todo

            ; otherwise f is undefined; in this case,
            ; E is returned as if it is quoted in lisp
            (t E))))))