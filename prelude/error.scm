; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

;;; Error
(define error (lambda (msg . args)
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
  (%halt)))
