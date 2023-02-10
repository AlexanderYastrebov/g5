(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (print (if (equal? expect expr) "[PASS]" "[FAIL]")
              *tests-run* "/"
              (set! *tests-passed* (+ *tests-passed* 1)))))))

(test 4 (+ 2 2))

(test 'yes (if (> 3 2) 'yes 'no))

(define a 123)
(define-syntax reta
  (syntax-rules ()
    ((reta) a)))

(test 123 
      (let ((a 0))
        (reta)))

(define let-inner 1)
(let () (define let-inner 2) (test 2 let-inner))
(test 1 let-inner)
