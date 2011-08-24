; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (append . args)
  (cond ((null? args)
	 '())
	((null? (cdr args))
	 (car args))
	(else
	 (append2 (car args) (apply append (cdr args))))))

(define (append2 x y)
  (if (null? x)
      y
      (append (cdr x) (cons (car x) y))))

(define (list? x)
  (or (null? x)
      (and (pair? x)
	   (list? (cdr x)))))

(define (length x)
  (cond ((null? x) 0)
	((pair? x) (+ 1 (length (cdr x))))
	(else (error "length"))))

(define (make-ass* name =)
  (make-mem* name
	     (lambda (x y)
	       (if (not (pair? y))
		   (error (string-append name ": not pair."))
		   (= x (car y))))))

(define (make-mem* name =)
  (define (loop elem list)
    (cond ((null? list) #f)
	  ((not (pair? list))
	   (error (string-append name ": inproper list.") list))
	  ((= elem (car list))
	   (car list))
	  (else
	   (loop elem (cdr list)))))
  loop)

(define assq (make-ass* "assq" eq?))
(define assv (make-ass* "assv" eqv?))
(define assoc (make-ass* "assoc" equal?))

(define memq (make-mem* "memq" eq?))
(define memv (make-mem* "memv" eqv?))
(define member (make-mem* "member" equal?))

(define (reverse l)
  (define (loop l r)
    (if (null? l)
	r
	(loop (cdr l) (cons (car l) r))))
  (loop l '()))

(define (list-ref l n)
  (if (zero? n)
      (car l)
      (list-ref (cdr l) (- n 1))))

(define (list-tail l n)
  (if (zero? n)
      l
      (list-tail (cdr l) (- n 1))))


;;; Generic

(define (map0 proc l)
  (if (null? l)
      '()
      (cons (proc (car l))
	    (map0 proc (cdr l)))))

(define (map proc . args)
  (if (null? (car args))
      '()
      (cons (apply0 proc (map0 car args))
	    (apply0 map 
		   (cons proc (map0 cdr args))))))


(define (for-each proc . l*)
  (if (null? (car l*))
      #f
      (begin (apply proc (map car l*))
	     (for-each proc (map cdr l*)))))
