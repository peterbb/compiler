; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE



(define gensym-counter 0)

(define gensym2
  (lambda ()
    (set! gensym-counter (+ 1 gensym-counter))
    gensym-counter))


(define gensym-list
  (lambda (n)
    (if (= n 0)
	'()
	(cons (gensym2)
	      (gensym-list (- n 1))))))
