(load "1363708.lisp")
(trace fl-interp)

(defun test (fn idx tests)
  (and tests
       (let ((res (apply fn (caar tests)))
             (ans (cadar tests)))
         (if (equal res ans)
           (format t "~d OK~%" idx)
           (format t "~d FAIL: expected ~A, got ~A~%" idx ans res))
         (test fn (1+ idx) (cdr tests)))))

(test 'fl-interp 14
      '((((rest (1 2 (3))) nil) (2 (3)))
        (((rest (p 1 2 (3))) nil) (1 2 (3)))
        (((first (rest (1 (2 3)))) nil) (2 3))
        (((eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil) nil)
        (((if (> 1 0) (+ 1 2) (+ 2 3)) nil) 3)
        (((if (> 1 0) (if (eq 1 2) 3 4) 5) nil) 4)
        (((cons (first (1 2 3))  (cons a nil)) nil) (1 a))
        (((and (or T  nil) (> 3 4)) nil) nil)
        (((eq (1 2 3) (1 2 3)) nil) nil)
        (((equal (1 2 3) (1 2 3)) nil) t)
        (((+ 1 2) nil) 3)
        (((f (f 2)) ( (f X =  (* X X)))) 16)
        (((a (+ 1 2)) ( (a X = (+ X 1)))) 4)
        (((b (+ 1 2)) ( (b X = (+ X 1)))) 4)
        (((reverse (1 2 3)) ( (reverse X =  (if (null X) nil (append (reverse (rest X)) (cons (first X) nil)))) (append X Y = (if (null X) Y (cons (first X) (append (rest X) Y)))))) (3 2 1))
        (((fib 10) ((fib n = (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 89)
        (((f 1) ((f n = (cons n (2 3 4 5 6))))) (1 2 3 4 5 6))
        (((count (1 2 3)) ( (count L = (if (null L) 0 (+ 1 (count (rest L))))))) 3)
        (((f (g 2) (g 1)) ( (f X Y = (+ X Y)) (g X = (+ 1 X)))) 5)
        (((f) ((f = 5))) 5)
        (((and 1 2) nil) t)
        (((and nil 2) nil) nil)
        (((or nil 2) nil) t)
        (((+ (f 1) (f 1 2)) ((f x = x) (f x y = (+ x y)))) 4)
        ; The following two should not terminate under applicative order reduction
        ;(((f 0 (g 1)) ((g X = (+ X (g (+ X 1)))) (f X Y = (if (eq X 0) 0 Y)))) 0)
        ;(((h (g 5)) ((g X = (g (g X))) (h X = a))) a)
        ))
