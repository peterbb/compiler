; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE


;;; Util
(define (tagged-list? list tag)
  (and (pair? list)
       (eq? tag (car list))))

(define (assert bool msg)
  (if (not bool)
      (error "assert:" msg)))

;;; ================================ ;;;
;;; Variables

(define (global->builtin! var)
  (assert (global-variable? var) "global->builtin!")
  (assert (not (global-variable-defined? var)) "globa->builtin! is defined")
  (let ((real-name (global-variable-original var)))
    (set-car! var 'builtin)
    (set-car! (cdr var) real-name)
    (set-cdr! (cdr var) '())))

(define (make-local-variable name)
  (list 'local name 'comp))

(define (make-local-variable/original name original)
  (list 'local name original))

(define (local-variable? e)
  (tagged-list? e 'local))

(define (local-variable-name e)
  (assert (local-variable? e) "local-variable-name")
  (list-ref e 1))

(define (local-variable-original e)
  (assert (local-variable? e) "local-variable-original")
  (list-ref e 2))

;; -------

(define (make-vararg-variable name)
  (list 'vararg name 'comp))

(define (make-vararg-variable/original name original)
  (list 'vararg name original))

(define (vararg-variable? e)
  (tagged-list? e 'vararg))

(define (vararg-variable-name e)
  (assert (vararg-variable? e) "vararg-variable-name")
  (list-ref e 1))

(define (vararg-variable-original e)
  (assert (vararg-variable? e) "vararg-variable-original")
  (list-ref e 2))

;; -------

(define (make-global-variable name)
  (list 'global name 'comp))

(define (make-global-variable/original name original defined?)
  (list 'global name original defined?))

(define (global-variable? e)
  (tagged-list? e 'global))

(define (global-variable-name e)
  (assert (global-variable? e) "global-variable-name")
  (list-ref e 1))

(define (global-variable-original e)
  (assert (global-variable? e) "global-variable-original")
  (list-ref e 2))

(define (global-variable-defined! e)
  (assert (global-variable? e) "global-variable-defined!")
  (set-car! (cdddr e) #t))

(define (global-variable-defined? e)
  (assert (global-variable? e) "global-variable-defined?")
  (list-ref e 3))

;; -------

(define (make-builtin-variable name)
  (list 'builtin name))

(define (builtin-variable? e)
  (tagged-list? e 'builtin))

(define (builtin-variable-name e)
  (assert (builtin-variable? e) "builtin-variable-name")
  (list-ref e 1))

;; -----
(define (variable? e)
  (or (local-variable? e)
      (global-variable? e)
      (vararg-variable? e)
      (builtin-variable? e)))

(define (variable-name e)
  (cond ((local-variable? e)
	 (local-variable-name e))
	((global-variable? e)
	 (global-variable-name e))
	((builtin-variable? e)
	 (builtin-variable-name e))
	((vararg-variable? e)
	 (vararg-variable-name e))
	(else
	 (error "variable-name: unrecognized type:" e))))


;;;;================================;;;
;;; Constants

(define (make-constant value)
  (list 'const value))

(define (constant? e)
  (tagged-list? e 'const))

(define (constant-value e)
  (assert (constant? e) "constant-value")
  (list-ref e 1))

;;; ==================================
;;; if

(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (if? e)
  (tagged-list? e 'if))

(define (if-predicate e)
  (assert (if? e) "if-predicate")
  (list-ref e 1))

(define (if-consequence e)
  (assert (if? e) "if-consequence")
  (list-ref e 2))

(define (if-alternative e)
  (assert (if? e) "if-alternative")
  (list-ref e 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lambda
(define (make-lambda var* body debug-name)
  (define (fixed-arity? var*)
    (or (null? var*)
	(and (local-variable? (car var*))
	     (fixed-arity? (cdr var*)))))
  (list 'lambda  var* body (fixed-arity? var*) debug-name))

(define (lambda? e)
  (tagged-list? e 'lambda))

(define (lambda-variable* e)
  (assert (lambda? e) "lambda-variable*")
  (list-ref e 1))

(define (lambda-body e)
  (assert (lambda? e) "lambda-body")
  (list-ref e 2))

(define (lambda-fixed-arity? e)
  (assert (lambda? e) "lambda-fixed-arity?")
  (list-ref e 3))

(define (lambda-debug-name e)
  (assert (lambda? e) "lambda-debug-name")
  (list-ref e 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;; Application

(define (make-application procedure arguments)
  (cons '@
   (cons procedure arguments)))

;(define (make-application* procedure . arguments)
;  (make-application procedure arguments))

(define (application? e)
  (tagged-list? e '@))

(define (application-procedure e)
  (assert (application? e) "application-procedure")
  (list-ref e 1))

(define (application-arguments e)
  (assert (application? e) "application-arguments")
  (cddr e))


;;; ==========================0
(define (make-definition variable expression)
  (assert (or (symbol? variable) (variable? variable))
	  "make-definition")
  (list 'define variable expression))

(define (definition? e)
  (tagged-list? e 'define))

(define (definition-variable e)
  (assert (definition? e) "definition-variable")
  (list-ref e 1))

(define (definition-expression e)
  (assert (definition? e) "definition-expression")
  (list-ref e 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-definition/k variable expression kont)
  (assert (variable? variable) "make-definition/k")
  (list 'define/k variable expression kont))

(define (definition/k? e)
  (tagged-list? e 'define/k))

(define (definition/k-variable e)
  (assert (definition/k? e) "definition/k-variable")
  (list-ref e 1))

(define (definition/k-expression e)
  (assert (definition/k? e) "definition/k-expression")
  (list-ref e 2))

(define (definition/k-kont e)
  (assert (definition/k? e) "definition/k-kont")
  (list-ref e 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-assignment variable expression)
  (list 'set! variable expression))

(define (assignment? e)
  (tagged-list? e 'set!))

(define (assignment-variable e)
  (assert (assignment? e) "assignment-variable")
  (list-ref e 1))

(define (assignment-expression e)
  (assert (assignment? e) "assignment-variable")
  (list-ref e 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-assignment/k variable expression kont)
  (list 'set!/k variable expression kont))

(define (assignment/k? e)
  (tagged-list? e 'set!/k))

(define (assignment/k-variable e)
  (assert (assignment/k? e) "assignment/k-variable")
  (list-ref e 1))

(define (assignment/k-expression e)
  (assert (assignment/k? e) "assignment/k-expression")
  (list-ref e 2))

(define (assignment/k-kont e)
  (assert (assignment/k? e) "assignment/k-kont")
  (list-ref e 3))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-record-type ast:ar
;  (make-ast:ar var exp)
;  ast:ar?
;  (var ast:ar:var set-ast:ar:var!)
;  (exp ast:ar:exp set-ast:ar:exp!))

(define make-activation-record
  (lambda (variable body)
    (list 'ar variable body)))

(define activation-record?
  (lambda (e) (tagged-list? e 'ar)))

(define activation-record-variable
  (lambda (e) (list-ref e 1)))

(define activation-record-body
  (lambda (e) (list-ref e 2)))


;;;;;;;;;;;;;;;;;;;;;;;,
(define make-halt-continuation
  (lambda () '(halt)))

(define halt-continuation?
  (lambda (e) (tagged-list? e 'halt)))

;;;;;;;;;;;;;;;;;;;

(define (ast:command? ast)
  (or (definition? ast)
      (definition/k? ast)
      (expression? ast)))

(define (expression? ast)
  (or (constant? ast)
      (variable? ast)
      (if? ast)
      (lambda? ast)
      (application? ast)
      (assignment? ast)
      (assignment/k? ast)
      (halt-continuation? ast)))


(define var=? 
  (lambda (x y)
    (and (variable? x)
	 (variable? y)
	 (eq? (car x) (car y))
	 (equal? (cadr x) (cadr y)))))
	     
