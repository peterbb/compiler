; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE


(define *global-variables* '())

(define (push-global! name var)
  (let ((binding (cons name var)))
    (set! *global-variables*
	  (cons binding *global-variables*))
    var))

(define (lookup-global-variable! name)
  (let ((binding (assq name *global-variables*)))
    (if binding
	(cdr binding)
	(push-global! name (make-global-variable/original (gensym) name #f)))))

(define (define-global-variable! name)
  (let ((binding (assq name *global-variables*)))
    (cond ((not binding)
	   (push-global! name (make-global-variable/original (gensym) name #t)))
	  (else
	   (global-variable-defined! (cdr binding))
	   (cdr binding)))))
	   

;;; Toplevel parser.
(define (parse cmd*)
  (if (and (pair? cmd*)
	   (list? cmd*))
      (let ((res (parse-command* cmd*)))
	(for-each (lambda (binding)
		    (if (not (global-variable-defined? (cdr binding)))
			(global->builtin! (cdr binding))))
		  *global-variables*)
	res)
      (error "parse: must be list")))

(define (parse-command* cmd*)
  (if (null? cmd*)
      '()
      (let ((cmd (car cmd*))
	    (cmd* (cdr cmd*)))
	(cond ((p:define? cmd)
	       (cons (parse-define cmd)
		     (parse-command* cmd*)))
	      ((p:load? cmd)
	       (parse-command*
		(append (io:read-file (cadr cmd))
			cmd*)))
	      (else
	       (cons (parse-exp cmd '())
		     (parse-command* cmd*)))))))

(define (p:load? e) (tagged-list? e 'load))
(define (p:begin? cmd) (tagged-list? cmd 'begin))
(define (p:define? e) (tagged-list? e 'define))

(define (parse-define cmd)
  (let* ((name (p:define-name cmd))
	 (expr (p:define-expr cmd))
	 (new-name (define-global-variable! name)))
    (make-definition new-name (parse-exp expr '()))))

(define (p:define-name e)
  (if (symbol? (cadr e))
      (if (= 3 (length e))
	  (cadr e)
	  (error "define statement: malformed:" e))
      (if (symbol? (caadr e))
	  (caadr e)
	  (error "define statement: expected symbol:" e))))

(define (p:define-expr e)
  (if (symbol? (cadr e))
      (caddr e)
      (append (list 'lambda (cdadr e))
	      (cddr e))))

;;; Parse expression-dispatch
(define (parse-exp exp lex-env)
  (cond ((p:variable? exp) (parse-variable exp lex-env))
	((p:const? exp) (parse-const exp))
	((p:if? exp) (parse-if exp lex-env))
	((p:lambda? exp) (parse-lambda exp lex-env))
	((p:set!? exp) (parse-set! exp lex-env))
	((p:lambda/cc? exp) (parse-lambda/cc exp lex-env))
	;; syntax begin
	((p:let? exp)
	 (parse-exp (let->exp exp) lex-env))
	((p:let*? exp)
	 (parse-exp (let*->exp exp) lex-env))
	((p:cond? exp)
	 (parse-exp (cond->exp exp) lex-env))
	((p:and? exp)
	 (parse-exp (and->exp exp) lex-env))
	((p:or? exp)
	 (parse-exp (or->exp exp) lex-env))
	((p:begin? exp)
	 (parse-exp (begin->exp exp) lex-env))
	((p:quasiquote? exp)
	 (error "parse-exp: not implemented: quasiquote" exp))
	;; error checking
	((p:define? exp)
	 (error "parse-exp: define may not appear here" exp))
	;; normal again...
	((pair? exp) (parse-app exp lex-env))
	(else 
	 (error "parse-exp: unrecognized:" exp))))

;; ... Variable
(define (p:variable? e) (symbol? e))

(define (parse-variable exp lex-env)
  (let ((binding (assq exp lex-env)))
    (if binding
	(cdr binding)
	(lookup-global-variable! exp))))

;; ... Constants
(define (p:const? exp)
  (or (tagged-list? exp 'quote)
      (number? exp)
      (boolean? exp)
      (char? exp)
      (string? exp)))

(define (parse-const exp)
  (if (tagged-list? exp 'quote)
      (if (= 2 (length exp))
	  (make-constant (cadr exp))
	  (error "parse-const: malformed quote:" exp))
      (make-constant exp)))

;; ... if
(define (p:if? e)
  (and (tagged-list? e 'if)
       (if (or (= 3 (length e))
	       (= 4 (length e)))
	   #t
	   (error "malformed if-expression"))))

(define (p:else-part e)
  (if (= (length e) 4) (list-ref e 3) #f))

(define (parse-if exp lex-env)
  (make-if (parse-exp (list-ref exp 1) lex-env)
	   (parse-exp (list-ref exp 2) lex-env)
	   (parse-exp (p:else-part exp) lex-env)))
	   
;; -- lambda
(define (p:lambda? e) (tagged-list? e 'lambda))

(define (parse-lambda-list l)
  (cond ((null? l) '())
	((symbol? l)
	 (list (cons l (make-vararg-variable/original (gensym) l))))
	((and (pair? l)
	      (symbol? (car l)))
	 (cons (cons (car l) (make-local-variable/original (gensym) (car l)))
	       (parse-lambda-list (cdr l))))
	(else
	 (error "parse-lambda-list: malformed:" l))))

(define (parse-lambda e lex-env)
  (let ((name:var-bindings (parse-lambda-list (cadr e)))
	(body (fix-internal-defines (cddr e))))
    (make-lambda (map cdr name:var-bindings) 
		 (parse-exp body 
			    (append name:var-bindings lex-env)))))

(define (fix-internal-defines e*)
  (let* ((internal-defines (p:get-internal-defines e*))
	 (real-body (p:skip-internal-defines e*))
	 (internal-names (map p:define-name internal-defines))
	 (internal-exprs (map p:define-expr internal-defines)))
    (if (null? internal-defines)
	(if (null? (cdr real-body))
	    (car real-body)
	    (cons 'begin real-body))
	(make-internal-define internal-names
			      internal-exprs
			      real-body))))

(define (make-internal-define names exprs body)
  (let ((bindings (map (lambda (v) (list v #f)) names))
	(assignments (map (lambda (v e)
			      (list 'set! v e))
			    names exprs)))
    (append (list 'let bindings)
	    (append assignments body))))
       

(define (p:get-internal-defines e*)
  (cond ((null? e*)
	 (error "parse-lambda: not expressions after defines"))
	((p:define? (car e*))
	 (cons (car e*)
	       (p:get-internal-defines (cdr e*))))
	(else '())))

(define (p:skip-internal-defines e*)
  (cond ((null? e*)
	 (error "p:lambda-skip-defs"))
	((p:define? (car e*))
	 (p:skip-internal-defines (cdr e*)))
	(else e*)))	   
	  

(define (p:lambda/cc? exp)
  (tagged-list? exp 'lambda/cc))
;; -- set!
(define (p:set!? e)
  (and (tagged-list? e 'set!)
       (if (= 3 (length e))
	   #t
	   (error "parse-set!: wrong length" e))))

(define (parse-set! exp lex-env)
  (let ((binding (assq (cadr exp) lex-env)))
    (if binding
	(make-assignment (cdr binding)
			 (parse-exp (caddr exp) lex-env))
	(make-assignment (lookup-global-variable! (cadr exp))
			 (parse-exp (caddr exp) lex-env)))))

;; -- application
(define (parse-app exp* lex-env)
  (if (not (list? exp*))
      (error "parse-app: inproper argument list" exp*))
  (make-application (parse-exp (car exp*) lex-env)
		    (map (lambda (e)
			   (parse-exp e lex-env))
			 (cdr exp*))))
;; -- let
(define (p:let? e) (tagged-list? e 'let))
(define (p:let-var x) (car x))
(define (p:let-exp x) (cadr x))

(define (let->exp exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (let ((vars (map p:let-var bindings))
	  (exps (map p:let-exp bindings)))
      (cons (cons 'lambda
		  (cons vars body))
	    exps))))

;; -- let*
(define (p:let*? e) (tagged-list? e 'let*))

(define (let*->exp e)
  (if (null? (cadr e))
      (cons 'let (cdr e))
      (let* ((binding-list (cadr e))
	     (body (cddr e))
	     (b (car binding-list))
	     (b* (cdr binding-list)))
	(list 'let (list b)
	      (append (list 'let* b*)
		      body)))))
	
;; -- cond

(define (p:cond? e) (tagged-list? e 'cond))

(define (cond->exp e)
  (cond->exp0 (cdr e)))

(define (cond->exp0 e)
  (if (null? e)
      '(quote undefined)
      (let ((pred (car (car e)))
	    (seq (cdr (car e)))
	    (alt (cond->exp0 (cdr e))))
	(if (eq? pred 'else)
	    (cons 'begin seq)
	    (list 'if 
		  pred
		  (cons 'begin seq)
		  alt)))))


;; -- and
(define (p:and? e) (tagged-list? e 'and))

(define (and->exp e)
  (and->exp0 (cdr e)))

;;- what is this doing here?
(define (gensym-symbol)
  (string->symbol (string-append "tmp"
				 (number->string (gensym)))))
(define (and->exp0 e)
  (cond ((null? e) #t)
	((null? (cdr e)) (car e))
	(else
	 (let ((e (car e))
	       (e* (cdr e)))
	   (list 'if e
		 (cons 'and e*)
		 #f)))))

;; -- or
(define (p:or? e) (tagged-list? e 'or))

(define (or->exp e)
  (or->exp0 (cdr e)))

(define (or->exp0 e)
  (cond ((null? e) #f)
	((null? (cdr e)) (car e))
	(else
	 (let ((var (gensym-symbol))
	       (e (car e))
	       (e* (cdr e)))
	   (list 'let (list (list var e))
		 (list 'if var var (cons 'or e*)))))))

;; -- begin
(define (begin->exp exp)
  (define (join e1 e2)
    (list '(lambda (x y) (y))
	  e1
	  (list 'lambda '() e2)))
  (define (begin->exp exp)
    (if (null? (cdr exp))
	(car exp)
	(join (car exp)
	      (begin->exp (cdr exp)))))
  (begin->exp (cdr exp)))
  
;; -- quasi-quote
(define (p:quasiquote? e) (tagged-list? e 'quasiquote))
