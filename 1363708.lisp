; CMPUT 325 Wi16 - NON-PROCEDURAL PROG LANGUAGES
; Assignment 2: An Interpreter for a Simple Functional Language
; February 24, 2016

; The interpreter is written in Common Lisp, tested on the Steel Bank Common Lisp
; implementation.

; As described in the specification, the interpreter is implemented based on
; Applicative Order Reduction (AOR). That is, evaluation is carried out as a
; series of reductions, where expressions are reduced to normal form and then
; evaluated. Arguments are evaluated before a function is applied, and functions
; are reduced starting with the leftmost innermost function.

; This interpreter uses a context. This context maps names, say X, to values,
; say 3. When a function is called, its parameters (names) are mapped to the evaluated
; values of its arguments (values), and the context is extended to include these
; names and values. 

; Name collisions between function parameter lists are avoided by prepending when extending the context. 
; That is, when new name-value pairs are added to the context, they are prepended to the list.
; That way, when retrieving a value for a name, the most recent (read: most local) value for that
; name will be returned.

; Functions are defined by (a) their name, and (b) their arity. Arity is defined as the
; number of parameters expected by the function. In this implementation, functions are correctly
; distinguished by both (a) and (b).

; Functions are documented at their definitions in this file. All code is in this file.
; The main entry point is fl-interp.

; Utility functions are tested in file testutil.lisp (not included in assignment submission).

; Interpreter as a whole is tested in file test.lisp (not included in assignment submission).
; Attribution for test.lisp:
; - Author: Noah Weninger
; - Published: February 4, 2016
; - Accessed: February 8, 2016

; Utility Functions:
; my-count
; arity-use
; arity-def
; xname
; xname-arity
; xname-arity-use
; xbody
; get-body
; get-def
; get-params
; is-user-def
; extend-context
; exists-in-context
; evaluate-in-context

; Main Functions:
; fl-interp-impl
; fl-interp


; function my-count returns the length of list L.
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
    ((equal (xname-arity-use f) (xname-arity (car P))) (xbody (car P)))
    ('t (get-body f (cdr P)))))

; function get-def takes an FL function usage as a list and returns the definition
; in program P
(defun get-def (f P)
  (cond
    ((null P) nil)
    ((equal (xname-arity (car P)) (xname-arity-use f)) (car P))
    ('t (get-def f (cdr P)))))

; function get-params takes an FL function usage as a list and returns the parameter names
; as a list, as defined in program P
(defun get-params-impl (def-without-name)
  (cond
    ((equal '= (car def-without-name)) nil)
    ('t (cons (car def-without-name) (get-params-impl (cdr def-without-name))))))
(defun get-params (f P)
  (get-params-impl (cdr (get-def f P))))

; function is-user-def takes an FL expression (function usage) as a list and returns whether
; it is user-defined or not
(defun is-user-def (E P)
  (cond
    ((null P) nil)
    ((equal (xname-arity-use E) (xname-arity (car P))) 't)
    ('t (is-user-def E (cdr P)))))

; function extend-context takes an FL expression (function usage) as a list, a program P,
; a context C, a list of concrete arguments to the function, and an interpreter function
; to call on the arguments to evaluate them.
; It extends the context C and returns the new extended context.
; The context is a list of dotted pairs like ((name . value) (name . value))
(defun extend-context (E P C args func)
  (append (mapcar #'(lambda (param-i arg-i) (cons param-i (funcall func arg-i P C))) (get-params E P) args) C))

; function exists-in-context returns a boolean: whether the given expression exists
; in the given context or not.
(defun exists-in-context (E C)
  (cond
    ((null C) nil)
    ((equal E (caar C)) 't)
    ('t (exists-in-context E (cdr C)))))

; function evaluate-in-context returns the value of the given expression 
; in the given context.
(defun evaluate-in-context (E C)
  (cond
    ((null C) nil)
    ((equal E (caar C)) (cdar C))
    ('t (evaluate-in-context E (cdr C)))))

; function fl-interp-impl implements function fl-interp by keeping track
; of an additional parameter, the context C. The context is a list of name-value
; pairs, where the car of each element is the name, and the cdr of each element
; is the value.
(defun fl-interp-impl (E P C)
  (cond 
	((atom E) (cond
                ((exists-in-context E C) (evaluate-in-context E C))
                ('t E)))
      (t (let ( (f (car E))  (arg (cdr E)) )
	   (cond 
            ; handle built-in functions
            ((eq f 'if) (cond
                          ((fl-interp-impl (car arg) P C) (fl-interp-impl (cadr arg) P C))
                          ('t (fl-interp-impl (caddr arg) P C))))
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
            ((exists-in-context f C) (evaluate-in-context f C))
            ((is-user-def E P) (fl-interp-impl (get-body E P) P (extend-context E P C arg #'fl-interp-impl)))

            ; otherwise f is undefined; in this case,
            ; E is returned as if it is quoted in lisp
            (t E))))))

; function fl-interp is the entry-point into the interpreter.
; It calls fl-interp-impl with an empty initial context.
(defun fl-interp (E P)
  (fl-interp-impl E P nil))
  