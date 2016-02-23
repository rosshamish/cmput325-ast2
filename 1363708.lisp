; utility functions needed:
; - find leftmost innermost function in arg
; - find function f with arity==length(arg) in P
; - apply function f

; Function my-count returns the length of list L.
(defun my-count (L)
    (if (null L)
        0
        (+ 1 (my-count (cdr L)))))

; function arity-use takes an FL function usage as a list and returns its arity
(defun arity-use (f)
  (- (my-count f) 1))

; function arity-def takes a FL function definition as a list and returns its arity
(defun arity-def (f)
  (cond
    ((null f) nil)
    ((equal '= (car f)) -1)
    ('t (+ 1 (arity-def (cdr f))))))

; function xname takes a FL function definition as a list and returns its name
; as a list including its parameters list and the = sign
(defun xname (f)
  (cond
    ((equal '= (car f)) (list '=))
    ('t (cons (car f) (xname (cdr f))))))

; function xname-arity takes a FL function definition as a list and returns
; a 2-element list containing (name arity)
(defun xname-arity (f)
  (cons (car f) (list (arity-def f))))

; function xname-arity-use takes a FL function usage as a list and returns
; a 2-element list containing (name arity)
(defun xname-arity-use (f)
  (cons (car f) (list (arity-use f))))

; function xbody takes a FL function definition as a list and returns its body
; as a list
(defun xbody (f)
  (cond
    ((equal '= (car f)) (cond
                          ((atom (cdr f)) (cdr f))
                          ('t (cadr f))))
    ('t (xbody (cdr f)))))

; function get-body takes an FL function usage as a list and returns the body
; of the function as defined in P
(defun get-body (f P)
  (cond
    ((null P) nil)
    ((equal (xname-arity-use f) (xname-arity (car P))) (xbody (car P)))))

; function is-user-def takes an FL expression (function usage) as a list and returns whether
; it is user-defined or not
(defun is-user-def (E P)
  (equal (xname-arity-use E) (xname-arity (car P))))

(defun fl-interp-impl (E P C)
  (cond 
	((atom E) E)
      (t (let ( (f (car E))  (arg (cdr E)) )
	   (cond 
            ; handle built-in functions
            ((eq f 'if) (if (fl-interp-impl (car arg) P C)
                            (fl-interp-impl (cadr arg) P C)
                            (fl-interp-impl (caddr arg) P C)))
            ((eq f 'null) (null (fl-interp-impl (car arg) P C)))
            ((eq f 'atom) (atom (fl-interp-impl (car arg) P C)))
            ((eq f 'eq) (eq (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f 'first)  (car (fl-interp-impl (car arg) P C)))
            ((eq f 'rest) (cdr (fl-interp-impl (car arg) P C)))
            ((eq f 'cons) (cons (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f 'equal) (equal (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f 'number) (numberp (fl-interp-impl (car arg) P C)))
            ((eq f '+) (+ (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f '-) (- (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f '*) (* (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f '>) (> (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f '<) (< (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f '=) (= (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C)))
            ((eq f 'and) (if (and (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C))
                             'T
                             nil))
            ((eq f 'or) (if (or (fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C))
                            'T
                            nil))
            ((eq f 'not) (not (fl-interp-impl (car arg) P C)))
            
            ; if f is a user-defined function,
            ; then evaluate the arguments 
            ; and apply f to the evaluated arguments 
            ; (applicative order reduction)
            ; ((exists-in-context E C))
            ((is-user-def E P) (fl-interp-impl (get-body E P) P 
                                               (append C (mapcar #'(lambda (a) (fl-interp-impl a P C)) arg))))

            ; otherwise f is undefined; in this case,
            ; E is returned as if it is quoted in lisp
            (t E))))))

; (defun extend-context (arg C P)
;   (append C (mapcar #'(lambda (a) (fl-interp-impl a P C)) arg)))

(defun fl-interp (E P)
  (fl-interp-impl E P ()))
  