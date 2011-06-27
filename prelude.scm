; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE


;;; Generic equality.
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

;;; Numbers.
;; Predicates
(define (number? x)
  (%number? x))

(define (%assert-number caller obj)
  (if (not (number? obj))
      (error (string-append caller "expected number, got:") obj)))

(define (%assert-number2 caller obj1 obj2)
  (%assert-number caller obj1)
  (%assert-number caller obj2))

(define (= x y)
  (%assert-number2 "=" x y)
  (%eq? x y))

(define (zero? n)
  (%assert-number "zero?" n)
  (= n 0))

(define (< x y)
  (%assert-number2 "<" x y)
  (> y x))

(define (<= x y)
  (%assert-number2 "<=" x y)
  (or (= x y)
      (< x y)))

(define (> x y)
  (%assert-number2 ">" x y)
  (%> x y))

(define (>= x y)
  (%assert-number2 ">=" x y)
  (or (= x y)
      (> x y)))

;; Operations on numbers
(define (+ x y)
  (%assert-number2 "+" x y)
  (%+ x y))

(define (- x y)
  (%assert-number2 "-" x y)
  (%- x y))

(define (* x y)
  (%assert-number2 "*" x y)
  (%* x y))

(define (quotient x y)
  (%assert-number2 "quotient" x y)
  (if (= y 0)
      (error "quotient: division by zero")
      (%quotient x y)))

(define (remainder x y)
  (%assert-number2 "quotient" x y)
  (if (= y 0)
      (error "remainder: division by zero")
      (%remainder x y)))
;;; Booleans

(define (boolean? x)
  (or (eq? x #f) (eq? x #t)))

(define (not x) (eq? x #f))

;;; Pairs

(define (null? x) (eq? x '()))

(define (pair? x) (%pair? x))

(define (cons x y) (%cons x y))

(define (list . x) x)

(define (%assert-pair caller obj)
  (if (not (pair? obj))
      (error (string-append caller " expected pair, got:" obj))))

(define (car x)
  (%assert-pair "car" x)
  (%car x))

(define (cdr x)
  (%assert-pair "cdr" x)
  (%cdr x))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (set-car! x y) 
  (%assert-pair "set-car!" x)
  (%set-car! x y))

(define (set-cdr! x y)
  (%assert-pair "set-cdr!" x)
  (%set-cdr! x y))

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


;;; Characters

(define (char? x) (%char? x))

(define (%assert-char caller obj)
  (if (not (char? obj))
      (error (string-append caller " expected character, got:") obj)))

(define (char->integer x)
  (%assert-char "char->integer" x)
  (%char->integer x))

(define (integer->char x)
  (%assert-number "integer->char" x)
  (%integer->char x))

(define (char->number c)
  (%assert-char "char->number" c)
  (if (and (char<=? #\0 c) (char<=? c #\9))
      (- (char->integer c) (char->integer #\0))
      (error "char->number given non-numeric character")))

(define (%make-char-pred name op)
  (lambda (x y)
    (%assert-char name x)
    (%assert-char name y)
    (op (char->integer x) (char->integer y))))

(define char=? (%make-char-pred "char=?" =))
(define char<? (%make-char-pred "char<?" <))
(define char<=? (%make-char-pred "char<=?" <=))
(define char>? (%make-char-pred "char>?" >))
(define char>=? (%make-char-pred "char>=?" >=))

(define (char-numeric? c)
  (%assert-char "char-numeric?" c)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (char-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)))

;;; Strings

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

(define (string-append s1 s2)

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

;;; Display and write.

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
	((null? x) (write-string "()"))
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

;;; Reader

(define %peek-valid? #f)
(define %peeked-char #\?)

(define (eof-object? o) (%eof-object? o))

(define (read-char)
  (if %peek-valid?
      (begin (set! %peek-valid? #f)
	     %peeked-char)
      (%read-char)))

(define (peek-char)
  (if %peek-valid?
      %peeked-char
      (begin (set! %peek-valid? #t)
	     (set! %peeked-char (%read-char))
	     %peeked-char)))


;;; The readere.
(define (read)
  (let ((c (peek-char)))
    (cond ((eof-object? c) c)
	  ((char-whitespace? c)
	   (read-char)
	   (read))
	  ((char=? c #\;)
	   (%skip-comment))
	  ((char-numeric? c) (%read-number))
	  ((or (char=? c #\+)
	       (char=? c #\-))
	   (%dispatch-symbol/number))
	  ((char=? c #\#) (%read-hashed))
	  ((char=? c #\() (%read-list))
	  ((char=? c #\") (%read-string))
	  ((char=? c #\)) (error "read: ')' not legal start"))
	  (else
	   (%read-symbol (read-char))))))

(define (%skip-comment)
  (let ((c (peek-char)))
    (cond ((eof-object? c) c)
	  ((char=? c #\newline)
	   (read))
	  (else
	   (read-char)
	   (%skip-comment)))))

;;; Does not detect leading +/-.
(define (%read-number)
  (define (loop val)
    (let ((c (peek-char)))
      (cond ((eof-object? c) val)
	    ((char-numeric? c)
	     (loop (+ (* 10 val)
		      (char->number (read-char)))))
	    ((char=? #\/ c)
	     (error "read: does not support rationals"))
	    ((char=? #\. c)
	     (error "read: does not support floats"))
	    (else val))))
  (loop 0))

(define (%dispatch-symbol/number)
  (let ((sign (read-char))
	(lookahead (peek-char)))
    (cond ((eof-object? lookahead)
	   (%read-symbol sign))
	  ((char-numeric? lookahead)
	   (if (char=? sign #\-)
	       (- 0 (%read-number))
	       (%read-number)))
	  (else
	   (%read-symbol sign)))))

(define (%match-char c)
  (if (not (char=? c (read-char)))
      (error "match-char:" c)))

(define (%read-hashed)
  (%match-char #\#)
  (let ((c (read-char)))
    (cond ((eof-object? c)
	   (error "read: reached eof while after reading '#'"))
	  ((char=? c #\t)
	   #t)
	  ((char=? c #\f)
	   #f)
	  ((char=? c #\\)
	   (%read-character))
	  (else
	   (error "read: unknown hashed")))))

(define (char-atmosphere? c)
  (or (eof-object? c)
      (memv c '(#\space #\newline #\; #\( #\) #\"))))

(define (%read-character)

  (define (read-until-atmosphere)
    (let ((c (peek-char)))
      (if (char-atmosphere? c)
	  '()
	  (cons (read-char)
		(read-until-atmosphere)))))

  (define *char-table*
    '(((#\n #\e #\w #\l #\i #\n #\e) #\newline)
      ((#\s #\p #\a #\c #\e) #\space)))

  (let* ((c (read-until-atmosphere))
	 (len (length c)))
    (if (= len 1)
	(car c)
	(let ((b (assoc c *char-table*)))
	  (if b
	      (cdr b)
	      (error "read: unknown character"))))))

(define (%read-string)
  (define (loop c*)
    (let ((c (read-char)))
      (cond ((char=? c #\")
	     (list->string (reverse c*)))
	    ((char=? c #\\)
	     (error "read: escape in string not implemented"))
	    (else
	     (loop (cons c c*))))))
  (%match-char #\")
  (loop '()))

(define (%read-symbol c)
  (define (list->symbol c) (string->symbol (list->string c)))
  (define (symbol-ender? c)
    (memv c '(#\space #\newline #\) #\( #\;)))
  (define (loop c*)
    (let ((c (peek-char)))
      (cond ((eof-object? c)
	     (list->symbol (reverse c*)))
	    ((symbol-ender? c)
	     (list->symbol (reverse c*)))
	    (else
	     (read-char)
	     (loop (cons c c*))))))
  (loop (list c)))


(define (skip-white)
  (if (char-whitespace? (peek-char))
      (begin (read-char) (skip-white))))

(define (%read-list)
  (define (read-rest)
    (skip-white)
    (let ((c (peek-char)))
      (cond ((eof-object? c)
	     (error "read: eof inside list"))
	    ((char=? c #\.)
	     (read-dotted))
	    ((char=? c #\) )
	     (%match-char #\) )
	     '())
	    (else
	     (let ((o (read)))
	       (if (eof-object? o)
		   (error "read: eof-object inside list")
		   (cons o (read-rest))))))))

  (define (read-dotted)
    (%match-char #\.)
    (let ((o (read)))
      (if (eof-object? o)
	  (error "read: eof inside list")
	  (begin (skip-white)
		 (%match-char #\))
		 o))))
	       

  (%match-char #\()
  (let ((head (read)))
    (if (eof-object? head)
	(error "read: eof while reading list")
	(cons head (read-rest)))))
	 
  
    
	   
      


;;; Symbols
(define (symbol? x) (%symbol? x))

(define (%assert-symbol caller obj)
  (if (not (symbol? obj))
      (error (string-append caller " expected symbol, got something else"))))

(define (symbol->string x)
  (%assert-symbol "symbol->string" x)
  (string-copy (%symbol->string x)))

(define (string->symbol x)
  (%assert-string "string->symbol" x)
  (%intern (string-copy x)))
  
;;; Error
(define (error msg . args)
  (define (loop args)
    (if (pair? args)
	(begin (write (car args))
	       (display " ")
	       (loop (cdr args)))))
  (newline)
  (display "error: ")
  (display msg)
  (loop args)
  (newline)
  (%halt))

;;; Generic

(define (map proc list)
  (if (null? list)
      '()
      (cons (proc (car list))
	    (map proc (cdr list)))))


;;; Continuations

(define (values . vals)
  (call-with-current-continuation
   (lambda (k)
     (apply k vals))))

;(define (call-with-values producer reciever)
;  (call-with-current-continuation
;   (lambda (k)
;     ((producer)

(define (apply proc args)
  (if (list? args)
      (%apply proc
	      (+ 1 (length args)) ;+1 = continuation
	      args)
      (error "apply: argument list:" args)))

(define (call-with-current-continuation proc)
  (%call-with-current-continuation proc))

;;; Formater

(define (sprintf fmt . args)
  (error "sprintf not implemented"))

(define (printf fmt . args)
  (error "printf not implemented"))
