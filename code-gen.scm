; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE



(define *delayed* '())

(define (add-delayed! code)
  (set! *delayed* (append code *delayed*)))

(define (generate-code ast globals)
  (let ((main-code (ast->code ast (llvm:gensym "%dummy") '())))
    (append *builtin-code*
	    (generate-global-declarations globals)
	    (list "define void @main() {"
		  "%env = call %t_obj @get-nil()")
	    main-code
	    (list "unreachable"
		  "}")
	    *delayed*)))
  

(define (generate-global-declarations var*)
  (cond ((null? var*)
	 '())
	((global-variable? (car var*))
	 (cons
	  (string-append (llvm:declare-global (number->string (global-variable-name (car var*))))
			 ";" (symbol->string (global-variable-original (car var*))))
	  (generate-global-declarations (cdr var*))))
	(else
	 (generate-global-declarations (cdr var*)))))

(define (ast->code ast target lex-env)
  (cond ((local-variable? ast)
	 (ast->code:local-variable ast target lex-env))
	((vararg-variable? ast)
	 (ast->code:vararg-variable ast target lex-env))
	((global-variable? ast)
	 (ast->code:global-variable ast target lex-env))
	((builtin-variable? ast)
	 (ast->code:builtin-variable ast target lex-env))
	((constant? ast)
	 (ast->code:constant ast target lex-env))
	((if? ast)
	 (ast->code:if ast target lex-env))
	((application? ast)
	 (ast->code:application ast target lex-env))
	((lambda? ast)
	 (ast->code:lambda ast target lex-env))
	((activation-record? ast)
	 (ast->code:ar ast target lex-env))
	((definition/k? ast)
	 (ast->code:definition ast target lex-env))
	((assignment/k? ast)
	 (ast->code:assignment ast target lex-env))
	((halt-continuation? ast)
	 (ast->code:halt-continuation ast target lex-env))
	(else
	 (error "ast->code: ast is: " ast))))

(define (block-contains-variable? var block)
  (if (null? block)
      #f
      (or (var=? var (car block))
	  (block-contains-variable? var (cdr block)))))
;;; cdr down the environment and find the block
;;; which contains the variable.
;;; XXX: Code is given in reverse.
(define (gen-code:find-block var env-reg lex-env k)
  (define (loop code block-reg lex-env)
    (cond ((null? lex-env)
	   (error "gen-code:find-block: variable not found:" var))
	  ((block-contains-variable? var (car lex-env))
	   (let ((tmp (llvm:gensym "%block")))
	     (k (if (local-variable? var)
		    (cons (llvm:get-car tmp block-reg)
			  code)
		    code)
		(if (local-variable? var)
		    tmp
		    block-reg)
		(car lex-env))))
	  (else
	   (let ((tmp (llvm:gensym "%block")))
	     (loop (cons (llvm:get-cdr tmp block-reg) code)
		   tmp
		   (cdr lex-env))))))
  (loop '() env-reg lex-env))

(define (gen-code:find-local-binding var env-reg lex-env)
  (define (find-binding code block-reg block-env)
    (if (var=? var (car block-env))
	(values (reverse code)
		block-reg)
	(let ((tmp (llvm:gensym "%block")))
	  (find-binding (cons (llvm:get-cdr tmp block-reg) code)
			tmp 
			(cdr block-env)))))

  (gen-code:find-block var env-reg lex-env find-binding))

(define (ast->code:local-variable var target lex-env)
  (call-with-values (lambda () (gen-code:find-local-binding var "%env" lex-env))
    (lambda (code reg)
      (append code
	      (list (llvm:get-car target reg))))))

(define (gen-code:find-vararg-binding var env-reg lex-env)
  (define (find-binding code block-reg block-env)
    (if (var=? var (cadr block-env))
	(values #f (reverse code) block-reg)
	(let ((tmp (llvm:gensym "%block")))
	  (find-binding (cons (llvm:get-cdr tmp block-reg) code)
			tmp
			(cdr block-env)))))
  
  (gen-code:find-block var env-reg lex-env
		       (lambda (code.rev block-reg block-env)
			 (if (var=? var (car block-env))
			     (values #t (reverse code.rev) block-reg)
			     (let ((tmp (llvm:gensym "%block")))
			       (find-binding (cons (llvm:get-car tmp block-reg) code.rev)
					     tmp
					     block-env))))))

(define (ast->code:vararg-variable var target lex-env)
  (call-with-values (lambda () (gen-code:find-vararg-binding var "%env" lex-env))
    (lambda (in-car? code reg)
      (if in-car?
	  (append code (list (llvm:get-car target reg)))
	  (append code (list (llvm:get-cdr target reg)))))))

(define (ast->code:global-variable var target lex-env)
  (list
   (llvm:load-global target (number->string (global-variable-name var)))))

(define (ast->code:builtin-variable var target lex-env)
  (error "ast->code:builtin-variable: may only appear in function-position: " var))

(define (ast->code:constant const target lex-env)
  (define (constant->code value target)
    (cond ((eq? value #t)
	   (list
	    (string-append target " = call %t_obj @get-true()")))
	  ((eq? value #f)
	   (list
	    (string-append target " = call %t_obj @get-false()")))
	  ((null? value)
	   (list
	    (string-append target " = call %t_obj @get-nil()")))
	  ((number? value)
	   (list
	    (string-append target
			   " = call %t_obj @i64-to-number(i64 "
			   (number->string value)
			   ")")))
	  ((char? value)
	   (list (string-append target " = call %t_obj @i64-to-char(i64 " 
				(number->string (char->integer value))
				")")))
	  ((string? value)
	   (compile-string value target))
	  ((symbol? value)
	   (compile-symbol value target))
	  ((pair? value)
	   (let ((a (car value))
		 (b (cdr value))
		 (a-reg (llvm:gensym "%a"))
		 (b-reg (llvm:gensym "%b")))
	     (append (constant->code a a-reg)
		     (constant->code b b-reg)
		     (list
		      (llvm:cons target a-reg b-reg)))))
	  (else
	   (error "ast->code:cosntant: not supported (yet?):" value const))))
  (constant->code (constant-value const) target))

(define (compile-string value target)

  (define (encode-char i)
    (sprintf "%t_obj ~a" (+ 1 (* (char->integer i) 8))))

  (define (encode-string s*)
    (cond ((null? s*) "")
	  ((null? (cdr s*))
	   (encode-char (car s*)))
	  (else
	   (string-append (encode-char (car s*)) ", " (encode-string (cdr s*))))))

  (let ((label (llvm:gensym "@string"))
	(tmp (llvm:gensym "%raw-string-ptr"))
	(length (string-length value)))
    (add-delayed!
     (list
      (sprintf "~a = global {%t_obj, [~a x %t_obj]} { %t_obj ~a, [ ~a x %t_obj ] [ ~a ]}"
	       label length (* 4 length) length
	       (encode-string (string->list value)))))
    (list
     (sprintf "~a = ptrtoint {%t_obj, [~a x %t_obj]}* ~a to %t_obj" tmp length label)
     (sprintf "~a = or %t_obj ~a, 6" target tmp))))
    
(define (compile-symbol value target)
  (let ((tmp (llvm:gensym "%sym-as-str")))
    (append (compile-string (symbol->string value) tmp)
	    (list (sprintf "~a = call %t_obj @intern(%t_obj ~a)" target tmp)))))

(define (ast->code:if ast target lex-env)
  (let ((pred-reg (llvm:gensym "%if-pred-"))
	(pred-reg.i1 (llvm:gensym "%if-pred.i1-"))
	(true-label (llvm:gensym "true-branch-"))
	(false-label (llvm:gensym "false-branch-")))
    (let ((code.pred (ast->code (if-predicate ast) pred-reg lex-env))
	  (code.cons (ast->code (if-consequence ast) target lex-env))
	  (code.alt  (ast->code (if-alternative ast) target lex-env)))
      (append code.pred
	      (list (sprintf "~a = icmp ne %t_obj ~a, ~a" pred-reg.i1 pred-reg (llvm:false))
		    (sprintf "br i1 ~a, label %~a, label %~a" pred-reg.i1 true-label false-label)
		    (sprintf "~a:" true-label))
	      code.cons
	      (list "unreachable"
		    (sprintf "~a:" false-label))
	      code.alt))))

(define (ast->code:application ast target lex-env)
  (define (make-args-value-code reg* exp*)
    (map (lambda (reg ast)
	   (ast->code ast reg lex-env))
	 reg* exp*))
  (let* ((op (application-procedure ast))
	 (args (application-arguments ast))
	 (arity (length args))
	 (reg* (llvm:gensym-list arity "%arg-value"))
	 (args-value-code (make-args-value-code reg* args)))
    (append (apply append args-value-code)
	    (if (builtin-variable? op)
		(ast->code:builtin-application op reg*)
		(ast->code:generic-application op reg* arity lex-env)))))

(define (ast->code:builtin-application op args)
  (let ((name (symbol->string (builtin-variable-name op))))
    (llvm:tail-call (sprintf "@\"builtin:~a\"" name) args)))

(define (ast->code:generic-application op args arity lex-env)

  (define (cons-args args prev-arg k)
    (if (null? args)
	(k prev-arg)
	(let ((x (llvm:gensym "%arglist-")))
	  (cons (llvm:cons x (car args) prev-arg)
		(cons-args (cdr args) x k)))))

  (define (do-application arglist-reg)
    (let* ((op-reg (llvm:gensym "%proc-"))
	   (op-code (ast->code op op-reg lex-env)))
      (append op-code
	      (llvm:tail-call "@apply" (list op-reg
					     (number->string (* 4 arity))
					     arglist-reg)))))
  (cons-args (reverse args) (llvm:nil) do-application))



(define (ast->code:lambda ast target lex-env)
  (let ((label (llvm:gensym "@lambda-")))
    (add-delayed!
     (make-lambda-definition label ast lex-env))
    (list 
     (sprintf "~a = call %t_obj @make-closure(%t_code ~a, %t_obj %env)" target label ))))

(define (make-lambda-definition label ast lex-env)

  (define (lambda-arity)
    (if (lambda-fixed-arity? ast)
	(length (lambda-variable* ast))
	(- (length (lambda-variable* ast)) 1)))

  (define (arity-relation)
    (if (lambda-fixed-arity? ast) "eq" "sge"))

  (let* ((args (lambda-variable* ast))
	 (arity (* 4 (lambda-arity)))
	 (arity-rel (arity-relation))
	 (lambda-code (ast->code (lambda-body ast) "%dummy"
				 (cons args lex-env)))
	 (debug-name (symbol->string (lambda-debug-name ast)))
	 (debug-label (llvm:gensym "@debug-string"))
	 (debug-length (+ 1 (string-length debug-name)))
	 (debug-tmp (llvm:gensym "%tmp")))
    (printf "; args=~a~%;rel=~a~%;-----------~%" args arity-rel)
    (append
     (list
      (sprintf "define private protected cc 10 void ~a(%t_obj %env, %t_obj %arity) noreturn {" label)
      (sprintf "%arity-check = icmp ~a %t_obj %arity, ~a" arity-rel arity)
      "br i1 %arity-check, label %arity-check-pass, label %signal-arity-error"
      "arity-check-pass:")
     lambda-code
     (list
      "unreachable"
      "signal-arity-error:"
      (sprintf "~a = bitcast [~a x i8]* ~a to i8*"
	       debug-tmp debug-length debug-label)
      (sprintf "call void @scheme-arity-error(i8* ~a, %t_obj %arity, %t_obj ~a)"
	       debug-tmp arity)
      "unreachable"
      "}"
      (sprintf "~a = constant [~a x i8] c\"~a\\00\""
	       debug-label debug-length debug-name)))))


(define (ast->code:ar ast target lex-env)
  (ast->code:lambda (make-lambda (list (activation-record-variable ast))
				 (activation-record-body ast)
				 'ar)
		    target
		    lex-env))

(define (ast->code:definition ast target lex-env)
  (ast->code:def/ass (definition/k-variable ast)
		     (definition/k-expression ast)
		     (definition/k-kont ast)
		     target
		     lex-env))

(define (ast->code:assignment ast target lex-env)
  (ast->code:def/ass (assignment/k-variable ast)
		     (assignment/k-expression ast)
		     (assignment/k-kont ast)
		     target
		     lex-env))

(define (ast->code:def/ass var expr kont target lex-env)
  (let ((val-reg (llvm:gensym "%v.")))
    (let ((code.expr (ast->code expr val-reg lex-env))
	  (code.kont (ast->code (make-application kont (list (make-constant #f))) "%dummy" lex-env)))
      (cond ((global-variable? var)
	     (append code.expr
		     (list (string-append "store i64 " val-reg ", i64* @glob"
					  (number->string (global-variable-name var))))
		     code.kont))
		   
	    ((local-variable? var)
	     (call-with-values (lambda () (gen-code:find-local-binding var "%env" lex-env))
	       (lambda (code.binding binding.reg)
		 (append code.expr
			 code.binding
			 (list (llvm:set-car! binding.reg val-reg))
			 code.kont))))
	    ((vararg-variable? var)
	     (error "assigmnet/def: assignment to vararg not implemented:" var))
	    (else
	     (error "ast->code:def/ass unknown variable type" var))))))

(define (ast->code:halt-continuation ast target lex-env)
  (let ((tmp (llvm:gensym "%untagged-halt")))
    (list
     (string-append tmp " = ptrtoint %t_closure* @halt-closure to %t_obj")
     (string-append target " = or %t_obj " tmp ", 5"))))
