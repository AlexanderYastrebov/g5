(set! scope '())
(set! a 1)

(define (f)
  (set! a 2)
  (set! scope (save-scope)))

(f)
(display (with-scope scope a))
