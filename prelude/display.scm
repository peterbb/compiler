; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (newline)
  (display-char #\newline))

(define (display x)
  (cond ((char? x) (display-char x))
	((boolean? x) (display-boolean x))
	((string? x) (display-string x))
	((null? x) (display-string "()"))
	((pair? x) (print-list display x))
	((number? x) (display-number x))
	((symbol? x) (display-symbol x))
	(else (display-string "#<unkown>"))))

(define pp display)

(define (display-char x)
  (if (char? x)
      (%write-char x)
      (error "display-char")))

(define (display-string s)
  (define (loop i max)
    (if (< i max)
	(begin (display-char (string-ref s i))
	       (loop (+ i 1) max))))
  (%assert-string "write-string" s)
  (loop 0 (string-length s)))

(define (display-boolean x)
  (if x
      (display "#t")
      (display "#f")))

(define (display-digit c)
  (cond ((= c 0) (display #\0))
	((= c 1) (display #\1))
	((= c 2) (display #\2))
	((= c 3) (display #\3))
	((= c 4) (display #\4))
	((= c 5) (display #\5))
	((= c 6) (display #\6))
	((= c 7) (display #\7))
	((= c 8) (display #\8))
	((= c 9) (display #\9))
	(else (error "display-digit: can't print:" c))))

(define (display-number x)
  (cond ((< x 0)
	 (display #\-)
	 (display-number (* -1 x)))
	((and (<= 0 x) (<= x 9))
	 (display-digit x))
	(else
	 (display-number (quotient x 10))
	 (display-digit (remainder x 10)))))
	 
(define (display-symbol x)
  (%assert-symbol "display-symbol" x)
  (display-string (symbol->string x)))



(define (write x)
  (cond ((char? x) (write-char x))
	((boolean? x) (display-boolean x))
	((string? x) (write-string x))
	((null? x) (display-string "()"))
	((symbol? x) (display-symbol x))
	((pair? x) (print-list write x))
	((number? x) (display-number x))
	(else
	 (display-string "#<unknown>"))))

(define (write-char x)
  (%assert-char "write-char" x)
  (display-char #\#)
  (display-char #\\)
  (cond ((char=? x #\space)
	 (display-string "space"))
	((char=? x #\newline)
	 (display-string "newline"))
	(else
	 (display-char x))))

(define (write-string s)
  (%assert-string "write-string" s)
  (display-char #\")
  (display-string s)
  (display-char #\"))

(define (print-list proc list)
  (display "(")
  (proc (car list))
  (print-rest-list proc (cdr list)))

(define (print-rest-list proc list)
  (cond ((null? list)
	 (display ")"))
	((pair? list)
	 (display " ")
	 (proc (car list))
	 (print-rest-list proc (cdr list)))
	(else
	 (display " . ")
	 (proc list)
	 (display ")"))))
