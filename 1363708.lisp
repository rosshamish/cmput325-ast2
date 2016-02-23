; utility functions needed:
; - find leftmost innermost function in arg
; - find function f with arity==length(arg) in P
; - apply function f

; function arity takes a FL function definition as a list and returns its arity
(defun arity (f)
  (cond
    ((equal '= (car f)) -1)
    ('t (+ 1 (arity (cdr f))))))

; function xname takes a FL function definition as a list and returns its name
; as a list including its parameters list and the = sign
(defun xname (f)
  (cond
    ((equal '= (car f)) (list '=))
    ('t (cons (car f) (xname (cdr f))))))

; function xbody takes a FL function definition as a list and returns its body
; as a list
(defun xbody (f)
  (cond
    ((equal '= (car f)) (cdr f))
    ('t (xbody (cdr f)))))

; function get-mapping returns the value in the context C
; pointed to by the given name. The name should contain
; the function name (or variable name) as well as the parameters,
; as well as the = sign (if appropriate)
(defun get-mapping (name C)
  (cond
    ((equal (car C) name))))

; (defun fl-interp-impl (E P C)
;   (cond )  
; )


(defun fl-interp (E P)
  (cond 
	((atom E) E)
      (t (let ( (f (car E))  (arg (cdr E)) )
	   (cond 
            ; handle built-in functions
            ((eq f 'if) (if (fl-interp (car arg) P)
                            (fl-interp (cadr arg) P)
                            (fl-interp (caddr arg) P)))
            ((eq f 'null) (null (fl-interp (car arg) P)))
            ((eq f 'atom) (atom (fl-interp (car arg) P)))
            ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f 'first)  (car (fl-interp (car arg) P)))
            ((eq f 'rest) (cdr (fl-interp (car arg) P)))
            ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f 'number) (numberp (fl-interp (car arg) P)))
            ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
            ((eq f 'and) (if (and (fl-interp (car arg) P) (fl-interp (cadr arg) P))
                             'T
                             nil))
            ((eq f 'or) (if (or (fl-interp (car arg) P) (fl-interp (cadr arg) P))
                            'T
                            nil))
            ((eq f 'not) (not (fl-interp (car arg) P)))
            
	      ; if f is a user-defined function,
            ; then evaluate the arguments 
            ; and apply f to the evaluated arguments 
            ; (applicative order reduction) 
            ; todo

            ; otherwise f is undefined; in this case,
            ; E is returned as if it is quoted in lisp
            (t E))))))
