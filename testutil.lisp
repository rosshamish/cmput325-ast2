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
      '((((reduce = 1 2 3)) (1 2 3))
        (((reduce X Y Z = 1 2 3)) (1 2 3))))

(test 'get-body 18
      '((((reduce) ((reduce = 4 5 6))) (4 5 6))
        (((reduce X Y Z) ((reduce 4 5 6 = 7 8 9))) (7 8 9))))
