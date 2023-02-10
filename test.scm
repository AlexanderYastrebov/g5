(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (print (if (equal? expect expr) "PASS" "FAIL")
              *tests-run* "/"
              (set! *tests-pased* (+ *tests-passed* 1)))))))

(test 1 2)
(test 2 2)
