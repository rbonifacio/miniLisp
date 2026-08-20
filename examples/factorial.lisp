; recursive factorial
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(display "5! = ")
(print (fact 5))

; closures
(define (make-adder n) (lambda (x) (+ x n)))
(define add5 (make-adder 5))
(print (add5 10))

; lists
(print (list 1 2 3 4 5))
(print (cons 0 '(1 2 3)))
