; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE
      
;;; Continuations

(define (values . vals)
  (error "values not implemented"))

;(define (call-with-values producer reciever)
;  (call-with-current-continuation
;   (lambda (k)
;     ((producer)

(define (call-with-values source trunk)
  (error "call-with-values not implemented"))

(define (apply0 proc args)
  ;(display (length args)) (newline)
  (if (list? args)
      (%apply proc
	      (+ 1 (length args))
	      args)
      (error "apply0: argument list:" args)))
  

(define (apply proc arg1 . argn)
  (define (collect-args args)
    (if (null? (cdr args))
	(if (list? (car args))
	    (car args)
	    (error "apply: last element must be proper list"))
	(cons (car args)
	      (collect-args (cdr args)))))
  (apply0 proc (collect-args (cons arg1 argn))))

(define (call-with-current-continuation proc)
  (%call-with-current-continuation proc))

(define (open-input-file filename)
  (error "open-input-file not implemented" filename))

(define (close-input-port port)
  (error "close-input-port not implemented" port))

(define (command-line-arguments)
  (error "command-line-arguments not implemented"))

(define (for-each x y)
  (error "for each not implemented"))

(define (number->string n)
  (error "number->string not implemented"))

(define (string->list n)
  (error "string->list not implemented"))
