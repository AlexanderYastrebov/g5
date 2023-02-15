(define call-with-current-continuation call/cc)
(define (null? x) (eqv? x '()))
(define (zero? z) (= z 0))
(define (positive? x) (>= x 0))
(define (negative? x) (< x 0))
(define (>= x y) (not (< x y)))
(define (<= x y) (not (< x y)))
(define exact? number?)
(define (inexact? z) #f)
(define (abs x) (if (negative? x) (- x) x))

(define (char=? a b) (= (char->integer a) (char->integer b)))
(define (char<? a b) (< (char->integer a) (char->integer b)))
(define (char>? a b) (> (char->integer a) (char->integer b)))
(define (char=<? a b) (=< (char->integer a) (char->integer b)))
(define (char=>? a b) (=> (char->integer a) (char->integer b)))

(define (string=<? a b) (not (string>? a b)))
(define (string=>? a b) (not (string<? a b)))

(define (string-ci<? a b)  (string<? (string-downcase a) (string-downcase b)))
(define (string-ci>? a b)  (string>? (string-downcase a) (string-downcase b)))
(define (string-ci<=? a b) (string<=? (string-downcase a) (string-downcase b)))
(define (string-ci>=? a b) (string>=? (string-downcase a) (string-downcase b)))
(define (string-ci=? a b)  (string=? (string-downcase a) (string-downcase b)))

(define (write-char ch . x)
  (if (not (char? ch))
    (error "write-char takes a char as the argument")
    (apply display `(,ch ,@x))))

(define (newline) (display #\newline))

(define (list . x) x)

(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))

(define (for-each f . x)
  (define (for-each1 f x)
    (if (not (null? x))
      (begin
        (f (car x))
        (for-each1 f (cdr x)))))
  (for-each1 (lambda (x) (for-each1 f x) x) x))

(define (print . x)
  (for-each (lambda (x) (display x) (display #\space)) x)
  (newline))

(define (error reason . args)
    (display "Error: ")
    (display reason)
    (apply print args)
    (exit 1))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))
  
(define-syntax letrec 
  (syntax-rules () 
    ((_ ((var init) ...) body ...)
     (let () 
       (define var init) ... 
       (let () body ...))))) 

 (define-syntax let
   (syntax-rules ()
     ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag) val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax do
  (syntax-rules ()
    ((_ ((var init step) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step) ...))))))
       (loop init ...)))
    ((do "step" x) x)
    ((do "step" x y) y)))

(define (force object) (object))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
          (result #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result))))))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (make-promise (lambda () expression)))))

(define (length list)
  (define (lengthl list . count)
    (if (null? list)
      (car count)
      (lengthl (cdr list)
              (if (null? count)
                1
                (+ (car count) 1)))))
  (lengthl list 0))

(define (max . x)
  (define (maxl x)
    (if (= (length x) 1)
      x
     (if (> (car (cdr x)) (car x))
       (maxl (cdr x))
       (maxl (cons (car x) (cdr (cdr x)))))))
  (car (maxl x)))

(define (reverse l)
  (if (null? l)
    '()
    (append (reverse (cdr l)) (list (car l)))))

(define (list-tail x k)
                  (if (zero? k)
                    x
                    (list-tail (cdr x) (- k 1))))

;(define (memf f? x l)
;  (cond
;    ((null? l) #f)
;    ((f? (car l) x) l)
;    (member x (cdr l))))
;
;(define (member x l) (memf equal? x l))
;(define (memv x l) (memf eqv? x l))
;(define (memq x l) (memf eq? x l))
;;;; Defined in lists.scm

(define (assf f? thing alist)
   (if (null? alist)
       #f
       (if (f? (car (car alist)) thing)
           (car alist)
           (assoc thing (cdr alist)))))

; (define (assoc x l) (assf equal? x l)) ;;;; also defined in lists.scm
(define (assv x l) (assf eqv? x l))
(define (assq x l) (assf eq? x l))

(define (append . l)
  (define (append2 l1 l2)
    (if (null? l1)
      l2
      (cons (car l1) (append2 (cdr l1) l2))))
  (case (length l)
    ((1) (car l))
    ((2) (append2 (car l) (car (cdr l))))
    (else (append2 (car l) (apply append (cdr l))))))

(define (list-ref l n)
  (if (zero? n)
    (car n)
    (list-ref (cdr l) (- n 1))))

(define (vector-fill! vector fill)
  (define (fill-n vector fill n)
    (if (>= n 0)
      (begin
        (vector-set! vector n fill)
        (fill-n vector fill (- n 1)))))
  (fill-n vector fill (- (vector-length vector) 1)))


(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((_ ((unquote x) . xs))          (cons x (quasiquote xs)))
    ((_ ((unquote-splicing x) . xs)) (append x (quasiquote xs)))
    ((_ (x . xs))                    (cons (quasiquote x) (quasiquote xs)))
    ((_ (unquote x)) x)
    ((_ x) (quote x))))


(define (get-environment-variable k)
  (define (get-var k env)
    (if (null? env)
      #f
      (if (string=? (car (car env)) k)
        (cdr (car env))
        (get-var k (cdr env)))))
  (get-var k (get-environment-variables)))

(define (caar   x) (car (car x)))
(define (cadr   x) (car (cdr x)))
(define (cdar   x) (cdr (car x)))
(define (cddr   x) (cdr (cdr x)))

(define (caaar  x) (caar (car x)))
(define (caadr  x) (caar (cdr x)))
(define (cadar  x) (cadr (car x)))
(define (caddr  x) (cadr (cdr x)))
(define (cdaar  x) (cdar (car x)))
(define (cdadr  x) (cdar (cdr x)))
(define (cddar  x) (cddr (car x)))
(define (cdddr  x) (cddr (cdr x)))

(define (caaaar x) (caaar (car x)))
(define (caaadr x) (caaar (cdr x)))
(define (caadar x) (caadr (car x)))
(define (caaddr x) (caadr (cdr x)))
(define (cadaar x) (cadar (car x)))
(define (cadadr x) (cadar (cdr x)))
(define (caddar x) (caddr (car x)))
(define (cadddr x) (caddr (cdr x)))
(define (cdaaar x) (cdaar (car x)))
(define (cdaadr x) (cdaar (cdr x)))
(define (cdadar x) (cdadr (car x)))
(define (cdaddr x) (cdadr (cdr x)))
(define (cddaar x) (cddar (car x)))
(define (cddadr x) (cddar (cdr x)))
(define (cdddar x) (cdddr (car x)))
(define (cddddr x) (cdddr (cdr x)))

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

(define (string-fill! str ch)
  (do ((n (- (string-length str) 1) (- n 1)))
       ((= n -1) str)
    (string-set! str n ch)))


(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))
