(set! a 1)
(set! scope '())
(define (myfn a)
    (set! scope (save-scope)))

(myfn 2)

(with-scope scope
            (display a))


(define-syntax print
  (syntax-rules ()
    ((_ x)
     ((lambda ()
       (display x)
       (newline))))))

(newline)
(display "test")
(newline)
(print "test of macros")


;(define-syntax let
;  (syntax-rules ()
;    ((let ((name val) ...) body1 body2 ...)
;     ((lambda (name ...) body1 body2 ...)
;      val ...))))
