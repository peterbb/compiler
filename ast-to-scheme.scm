; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define ast->scheme/with-globals
  (lambda (exp globals)
    (if (null? globals)
	(ast->scheme (list exp))
	(cons (list 'define
		    (ast->scheme:variable (car globals))
		    '(quote undefined))
	      (ast->scheme/with-globals exp (cdr globals))))))


(define ast->scheme
  (lambda (exp)
    (%map ast->scheme:command exp)))

(define ast->scheme:command
  (lambda (exp)
    (if (definition? exp)
	(ast->scheme:definition exp)
	(ast->scheme:expression exp))))

(define ast->scheme:expression
  (lambda (exp)
    (cond ((variable? exp)
	   (ast->scheme:variable exp))
	  ((symbol? exp) exp);hax
	  ((constant? exp)
	   (ast->scheme:constant exp))
	  ((if? exp)
	   (ast->scheme:if exp))
	  ((lambda? exp)
	   (ast->scheme:lambda exp))
	  ((activation-record? exp)
	   (ast->scheme:activation-record exp))
	  ((application? exp)
	   (ast->scheme:application exp))
	  ((assignment? exp)
	   (ast->scheme:assignment exp))
	  ((assignment/k? exp)
	   (ast->scheme:assignment/k exp))
	  ((definition/k? exp)
	   (ast->scheme:definition/k exp))
	  ((halt-continuation? exp)
	   (ast->scheme:halt-continuation exp))
	  (else
	   (error "ast->scheme:expression:" exp)))))

(define ast->scheme:variable
  (lambda (var)
    (define (->symbol tag number original)
      (string->symbol (string-append tag
				     "-"
				     (number->string number)
				     ":"
				     (symbol->string original))))
    (cond ((global-variable? var)
	   (->symbol "global"
		     (global-variable-name var)
		     (global-variable-original var)))
	  ((local-variable? var)
	   (->symbol "local"
		     (local-variable-name var)
		     (local-variable-original var)))
	  ((builtin-variable? var) ;;; Assume it is in call-position.
      	   (list 'lambda '(k . args)
		 (list 'k
		       (list 'apply (builtin-variable-name var) 'args))))
	  ((symbol? var) var)
	  (else
	   (error "ast->scheme:variable: got" var)))))

(define ast->scheme:constant
  (lambda (const)
    (list 'quote (constant-value const))))

(define ast->scheme:if
  (lambda (exp)
    (list 'if
	  (ast->scheme:expression (if-predicate exp))
	  (ast->scheme:expression (if-consequence exp))
	  (ast->scheme:expression (if-alternative exp)))))

(define ast->scheme:lambda
  (lambda (exp)
    (list 'lambda (%map ast->scheme:variable (lambda-variable* exp))
	  (ast->scheme:expression (lambda-body exp)))))

(define ast->scheme:activation-record
  (lambda (exp)
    (append (list 'lambda
		  (list (ast->scheme:variable (activation-record-variable exp)))
		  (ast->scheme:expression (activation-record-body exp))))))

(define ast->scheme:application
  (lambda (exp)
    (cons (ast->scheme:expression (application-procedure exp))
	  (%map ast->scheme:expression
	       (application-arguments exp)))))

(define ast->scheme:assignment
  (lambda (exp)
    (list 'set!
	  (ast->scheme:variable (assignment-variable exp))
	  (ast->scheme:expression (assignment-expression exp)))))

(define ast->scheme:assignment/k
  (lambda (exp)
    (list 'begin
	  (list 'set!
		(ast->scheme:variable (assignment/k-variable exp))
		(ast->scheme:expression (assignment/k-expression exp)))
	  (list (ast->scheme:expression (assignment/k-kont exp))
		'(quote undefined)))))

(define ast->scheme:halt-continuation
  (lambda (exp)
    '(lambda (v) (printf ";exit with: ~a~%" v) (exit))))

(define ast->scheme:definition
  (lambda (exp)
    (list 'define
	  (ast->scheme:variable (definition-variable exp))
	  (ast->scheme:expression (definition-expression exp)))))
		  

(define ast->scheme:definition/k
  (lambda (exp)
    (list 'begin
	  (list 'set!
		(ast->scheme:variable (definition/k-variable exp))
		(ast->scheme:expression (definition/k-expression exp)))
	  (list (ast->scheme:expression (definition/k-kont exp))
		'(quote defined)))))
