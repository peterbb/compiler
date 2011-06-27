
(define gensym-counter 0)

(define gensym
  (lambda ()
    (set! gensym-counter (+ 1 gensym-counter))
    gensym-counter))


(define gensym-list
  (lambda (n)
    (if (= n 0)
	'()
	(cons (gensym)
	      (gensym-list (- n 1))))))