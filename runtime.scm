(define (newline) (display #\newline))

(define-syntax let
  (syntax-rules ()
    ((let ((dst src) ...) body ...)
     ((lambda (dst ...) body ...) src ...))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))
