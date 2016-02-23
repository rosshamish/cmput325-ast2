(load "1363708.lisp")
; (trace get-body)

(defun test (fn idx tests)
  (and tests
       (let ((res (apply fn (caar tests)))
             (ans (cadar tests)))
         (if (equal res ans)
           (format t "~d OK~%" idx)
           (format t "~d FAIL: expected ~A, got ~A~%" idx ans res))
         (test fn (1+ idx) (cdr tests)))))

(test 'arity-def 13
      '((((reduce = 1 2 3)) 0)
        (((reduce A B C = 1 2 3)) 3)))

(test 'arity-use 13
      '((((reduce 1 2 3)) 3)
        (((reduce (1 2) 3)) 2)))

(test 'xname 18
      '((((reduce = 1 2 3)) (reduce =))
        (((reduce X Y Z = 1 2 3)) (reduce X Y Z =))))

(test 'xname-arity 18
      '((((reduce = 1 2 3)) (reduce 0))
        (((reduce X Y Z = 1 2 3)) (reduce 3))))

(test 'xbody 18
      '((((reduce = (+ 2 3))) (+ 2 3))
        (((reduce X Y Z = 2)) 2)))

(test 'get-body 18
      '((((reduce) ((reduce = 4))) 4)
        (((reduce 4 5 6) ((reduce X Y Z = (+ X 1)))) (+ X 1))))

(test 'get-def 18
      '((((reduce) ((reduce = 4))) (reduce = 4))
        (((reduce 4 5 6) ((reduce X Y Z = (+ X 1)))) (reduce X Y Z = (+ X 1)))))

(test 'get-params 18
      '((((reduce) ((reduce = 4))) ())
        (((reduce 4 5 6) ((reduce X Y Z = (+ X 1)))) (X Y Z))))

