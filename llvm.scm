; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE



(define (llvm:gensym t)
  (string-append t (number->string (gensym2))))

(define (llvm:gensym-list n tag)
  (if (= 0 n)
      '()
      (cons (llvm:gensym tag)
	    (llvm:gensym-list (- n 1) tag))))

(define (llvm:fixnum->word target obj)
  (string-append target " = call i64 @number-to-i64(%t_obj " obj ")\n"))

(define (llvm:word->fixnum target obj)
  (string-append target " = call %t_obj @i64-to-number(i64 " obj ")\n"))

(define (llvm:number n)
  (number->string (* 4 n)))

(define (llvm:nil)
  "23")

(define (llvm:true)
  "15") ; 1111

(define (llvm:false)
  "7") ; 111

(define (llvm:eof)
  "31") ; 11111 = 31

(define llvm:undefined llvm:false)

(define (llvm:symbol-tag)
  "3")

(define (llvm:string-tag)
  "6")

(define (llvm:select target pred true false)
  (string-append target " = select i1 " pred ", %t_obj " true ", %t_obj " false))

(define (llvm:declare-global name)
  (string-append "@glob" name " = global %t_obj " (llvm:undefined)))

(define (llvm:load-global target name)
  (string-append target " = load %t_obj* @glob" name))


(define (llvm:cons target a b)
  (string-append
   target " = call %t_obj @make-pair(%t_obj " a ", %t_obj " b ")"))

(define (llvm:get-car target source)
  (string-append
   target " = call %t_obj @get-car(%t_obj " source ")"))

(define (llvm:get-cdr target source)
  (string-append
   target " = call %t_obj @get-cdr(%t_obj " source ")"))

(define (llvm:set-car! target value)
  (string-append "call void @set-car(%t_obj " target ", %t_obj " value ")"))

(define (llvm:tail-call op args)
  (let ((formals (llvm:make-param-list op args)))
    (list (string-append "tail call fastcc void " formals " noreturn")
	  "ret void")))

(define (llvm:call target op args)
  (let ((formals (llvm:make-param-list op args)))
    (string-append target " = call %t_obj " formals)))

(define (llvm:make-param-list name args)
  (define (add-type s) (string-append "%t_obj " s))
  (define (add-comma s) (string-append ", " s))
  (let* ((args (map add-type args))
	 (first-arg (car args))
	 (rest-args (cdr args))
	 (rest-args (map add-comma rest-args)))
    (string-append name
		   "("
		   (apply string-append first-arg rest-args)
		   ")")))



(define (llvm:return x)
  (llvm:tail-call "@apply1" (list "%k" x)))

(define (llvm:builtin-header name args)
  (string-append "define private protected fastcc void "
		 (llvm:make-param-list
		  (string-append "@\"builtin:" name "\"")
		  (cons "%k" args))
		 "{"))

(define (llvm:fix-body body)
  (cond ((null? body) '())
	((string? (car body))
	 (cons (car body)
	       (llvm:fix-body (cdr body))))
	(else
	 (llvm:fix-body
	  (append (car body) (cdr body))))))
		  

(define (llvm:declare-builtin name args body)
  (set! *builtin-code*
	(append
	 (list (llvm:builtin-header name args))
	 (llvm:fix-body body)
	 (list "}")
	 *builtin-code*)))

(define *builtin-code* '())

(define (llvm:signal-error e)
  (let ((label (llvm:gensym "@error-msg"))
	(length (+ 2 (string-length e))))
    (set! *builtin-code*
	  (cons (string-append
		 label " = constant ["
		 (number->string length)
		 " x i8] c\"" e "\\0A\\00\"")
		*builtin-code*))
    (list
     (string-append 
        "tail call fastcc void @scheme-error(i8* bitcast(["
                (number->string length)
                " x i8]* " label " to i8*)) noreturn")
        "ret void")))

   
