(define (newline) (display #\newline))

(define-syntax let
  (syntax-rules ()
    ((_ ((dst src) ...) body ...)
     ((lambda (dst ...) body ...) src ...))))
