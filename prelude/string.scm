; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (string? s)
  (%string? s))

(define (%assert-string caller obj)
  (if (not (string? obj))
      (error (string-append caller " expected string, got something else"))))

(define (string-length s)
  (%assert-string "string-length" s)
  (%string-length s))

(define (string=? s1 s2)
  (define (loop i max)
    (or (>= i max)
	(and (char=? (string-ref s1 i)
		     (string-ref s2 i))
	     (loop (+ i 1) max))))
  (%assert-string "string=?" s1)
  (%assert-string "string=?" s2)
  (and (= (string-length s1)
	  (string-length s2))
       (loop 0 (string-length s1))))

(define (string-ref s i)
  (%assert-string "string-ref" s)
  (%assert-number "string-ref" i)
  (if (and (>= i 0)
	   (< i (string-length s)))
      (%string-ref s i)
      (error "string-ref: index out of bounds")))

(define (string-set! s i v)
  (%assert-string "string-set!" s)
  (%assert-number "string-set!" i)
  (if (and (>= i 0)
	   (< i (%string-length s)))
      (%string-set! s i v)
      (error "string-set!: index out of bound")))

(define (map-string proc string)
  (define (loop i max)
    (if (< i max)
	(begin (string-set! string i
			    (proc (string-ref string i)))
	       (loop (+ i 1) max))
	string))
  (%assert-string "map-string" string)
  (loop 0 (string-length string)))

(define (make-string length)
  (%assert-number "make-string" length)
  (let ((s (%make-string length)))
    (map-string (lambda (c) #\?) s)))


(define (string-append . args)
  (cond ((null? args) "")
	((null? (cdr args)) (car args))
	(else
	 (string-append2
	  (car args)
	  (apply string-append (cdr args))))))

(define (string-append2 s1 s2)
  (define (loop src src-i dst dst-i count)
    (define (loop src-i dst-i i)
      (if (< i count)
	  (begin
	    (string-set! dst dst-i
			 (string-ref src src-i))
	    (loop (+ 1 src-i) (+ 1 dst-i) (+ 1 i)))))
    (loop src-i dst-i 0))

  (%assert-string "string-append" s1)
  (%assert-string "string-append" s2)
  (let* ((length-1 (string-length s1))
	 (length-2 (string-length s2))
	 (s (make-string (+ length-1 length-2))))
    (loop s1 0 s 0 length-1)
    (loop s2 0 s length-1 length-2)
    s))

(define (string-copy s)
  (string-append s ""))

(define (list->string c*)
  (let* ((len (length c*))
	 (s (make-string len)))
    (define (loop i c*)
      (if (< i len)
	  (begin (string-set! s i (car c*))
		 (loop (+ 1 i) (cdr c*)))
	  s))
    (loop 0 c*)))
