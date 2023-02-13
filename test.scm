(define *tests-run* 0)
(define *tests-passed* 0)

(define (test name expect expr)
   (set! *tests-run* (+ *tests-run* 1))
   (if (equal? expect expr)
     (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "[PASS] "))
     (begin
       (print "[FAIL]")
       (print "Expected:" expect)
       (print "     Got:" expr)))
     (print *tests-passed* "/" *tests-run* name))

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
(test "call/cc 2" 123 (call-with-current-continuation (lambda (f) (f 123))))

(test "call/cc 3" -3
    (call-with-current-continuation
      (lambda (exit)
        (for-each (lambda (x)
                    (if (negative? x)
                        (exit x)))
                  '(54 0 37 -3 245 19))
        #t)))

(test "quasiquote" '(1 3 1) `(1 ,(+ 1 2) 1))
(test "quasiquote 2" '(list 3 4) `(list ,(+ 1 2) 4))

(test "vector->list" (vector->list '#(1 2 3)) '(1 2 3))
(test "list->vector" (list->vector '(1 2 3)) '#(1 2 3))

(test "letrec-syntax" 1234
      (letrec-syntax ((test1 (syntax-rules () ((test1) (test))))
                      (test (syntax-rules () ((test) 1234))))
      (test1)))
