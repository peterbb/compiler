; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define beta-reduce
  (lambda (ast)
    (cond ((constant? ast) ast)
	  ((halt-continuation? ast) ast)
	  ((variable? ast) ast)
	  ((if? ast) (beta-reduce-if ast))
	  ((application? ast) (beta-reduce-app ast))
	  ((lambda? ast) (beta-reduce-lambda ast))
	  ((assignment/k? ast) (beta-reduce-set! ast))
	  ((definition/k? ast) (beta-reduce-def ast))
	  ((activation-record? ast) (beta-reduce-ar ast))
	  (else
	   (error "beta-reduce: unkown ast:" ast)))))

(define beta-reduce-if
  (lambda (ast)
    (let ((pred (if-predicate ast))
	  (con (if-consequence ast))
	  (alt (if-alternative ast)))
      (make-if
       (beta-reduce pred)
       (beta-reduce con)
       (beta-reduce alt)))))


(define beta-reduce-app
  (lambda (ast)
    (let ((op (beta-reduce (application-procedure ast)))
	  (args (map beta-reduce (application-arguments ast))))
      (if (activation-record? op)
	  (apply-beta-reduce-ar op args)
	  (make-application op args)))))

(define apply-beta-reduce-ar
  (lambda (op args)
    (if (not (= 1 (length args)))
	(error "apply-beta-reduce-ar: too many arguments" (length args)))
    (beta-reduce (substitute (activation-record-body op)
			     (activation-record-variable op)
			     (car args)))))
	  
(define beta-reduce-lambda
  (lambda (ast)
    (make-lambda
     (lambda-variable* ast)
     (beta-reduce (lambda-body ast)))))

(define beta-reduce-set!
  (lambda (ast)
    (make-assignment/k
     (assignment/k-variable ast)
     (beta-reduce (assignment/k-expression ast))
     (beta-reduce (assignment/k-kont ast)))))

(define beta-reduce-def
  (lambda (ast)
    (make-definition/k
     (definition/k-variable ast)
     (beta-reduce (definition/k-expression ast))
     (beta-reduce (definition/k-kont ast)))))

(define beta-reduce-ar
  (lambda (ast)
    (make-activation-record
     (activation-record-variable ast)
     (beta-reduce (activation-record-body ast)))))
		

;;; substitute[P, x, R] == P [x := R].
(define substitute
  (lambda (exp var sub)
    (cond ((constant? exp) exp)
	  ((halt-continuation? exp) exp)
	  ((variable? exp) (substitute-var exp var sub))
	  ((if? exp) (substitute-if exp var sub))
	  ((application? exp) (substitute-app exp var sub))
	  ((lambda? exp) (substitute-lambda exp var sub))
	  ((activation-record? exp) (substitute-ar exp var sub))
	  ((assignment/k? exp) (substitute-set! exp var sub))
	  ((definition/k? exp) (substitute-def exp var sub))
	  (else
	   (error "substitute: unknown ast:" exp)))))
	   

(define substitute-var
  (lambda (exp var sub)
    (if (var=? var exp)
	sub
	exp)))

(define substitute-if
  (lambda (exp var sub)
    (make-if (substitute (if-predicate exp) var sub)
	     (substitute (if-consequence exp) var sub)
	     (substitute (if-alternative exp) var sub))))

(define substitute-app
  (lambda (exp var sub)
    (make-application
     (substitute (application-procedure exp) var sub)
     (%map (lambda (e) (substitute e var sub))
	  (application-arguments exp)))))

(define substitute-lambda
  (lambda (exp var sub)
    (make-lambda
     (lambda-variable* exp)
     (substitute (lambda-body exp) var sub))))

(define substitute-ar
  (lambda (exp var sub)
    (make-activation-record
     (activation-record-variable exp)
     (substitute (activation-record-body exp) var sub))))

(define substitute-set!
  (lambda (exp var sub)
    (if (var=? var (assignment/k-variable exp))
	(error "substitute-assignment: cant substitute mutated variable" var))
    (make-assignment/k
     (assignment/k-variable exp)
     (substitute (assignment/k-expression exp) var sub)
     (substitute (assignment/k-kont exp) var sub))))

(define substitute-def
  (lambda (exp var sub)
    (if (var=? var (definition/k-variable exp))
	(error "substitute-definition: cant substitute defined variable" var))
    (make-definition/k
     (definition/k-variable exp)
     (substitute (definition/k-expression exp) var sub)
     (substitute (definition/k-kont exp) var sub))))
