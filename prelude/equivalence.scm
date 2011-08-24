; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (eq? x y)
  (%eq? x y))

(define (eqv? x y)
  (cond ((eq? x y) #t)
	((and (char? x) (char? y))
	 (char=? x y))
	((and (number? x) (number? y))
	 (= x y))
	(else #f)))

(define (equal? x y)
  (cond ((eqv? x y) #t)
	((pair? x y)
	 (and (equal? (car x) (car y))
	      (equal? (cdr x) (cdr y))))
	(else #f)))



