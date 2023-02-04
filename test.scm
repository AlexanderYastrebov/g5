(set! a 1)
(set! scope '())
(define (myfn a)
    (set-scope! scope))

(myfn 2)

(with-scope scope
            (display a))






;(define-syntax let
;  (syntax-rules ()
;    ((let ((name val) ...) body1 body2 ...)
;     ((lambda (name ...) body1 body2 ...)
;      val ...))))
