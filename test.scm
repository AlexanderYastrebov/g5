(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (print (if (equal? expect expr) "[PASS]" "[FAIL]")
              *tests-run* "/"
              (set! *tests-passed* (+ *tests-passed* 1))
              name)))))

(test "add" 4 (+ 2 2))

(test "if" 'yes (if (> 3 2) 'yes 'no))

(define a 123)
(define-syntax reta
  (syntax-rules ()
    ((reta) a)))

(test "macro scoping" 123 
      (let ((a 0))
        (reta)))

(define let-inner 1)
(let () (define let-inner 2) (test "let-inner-inner" 2 let-inner))
(test "let-inner" 1 let-inner)

(test "cond" 'yes (cond
             ((> 3 2) 'yes)
             ((< 3 2) 'no)))

(define plus
  (case-lambda 
    (() 0)
    ((x) x)
    ((x y) (+ x y))
    ((x y z) (+ (+ x y) z))
    (args (apply + args))))


(test "case-lambda" 6 (plus 1 2 3))


(test "call/cc 1" 231 (call/cc (lambda (f) 231)))
(test "call/cc 2" 123 (call/cc (lambda (f) (f 123))))
