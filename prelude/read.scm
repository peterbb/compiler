; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

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
	 
  
