(set! a 1)
(set! scope '())
(define (myfn a)
    (define-scope scope))

(myfn 2)

(with-scope scope
            (display a))
