; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(load "ast.scm")
(load "parse.scm")
(load "gensym.scm")
(load "io.scm")
(load "cps.scm")
(load "beta-reduce.scm")
(load "llvm.scm")
(load "code-gen.scm")
(load "builtin.scm")

;;; "args" is a list of strings, which are the arguments.
;;; The first element in "args" is the name of the file to compile.
;;; The rest of the arguments should be the names of the stages to preform
;;; in the order as they appear.
(define (main args)
  (let* ((file-name (car args))
	 (pipeline (make-pipeline (cdr args)))
	 (code (add-static-prelude (io:read-file file-name)))
	 (ast (parse code)))
    (pipeline ast)))

;;; This code is implicitly appended to the begining of the
;;; compiled file.
(define *static-prelude*
  (list
   '(load "prelude/all.scm")
   ))

(define (add-static-prelude code)
  (append *static-prelude* code))

(define (make-pipeline args)
  (if (null? args)
      (lambda (ast)
	(display "; Done\n"))
      (let ((stage (assoc (car args) *stages*))
	    (rest-stages (make-pipeline (cdr args))))
	(if stage
	    (lambda (ast)
	      (rest-stages ((cdr stage) ast)))
	    (error "unknown argument:" (car args))))))

(define *stages*
  (list	(cons "ast"
	      (lambda (ast)
		(display ";;; AST:\n")
		(pp ast)
		(newline)
		ast))

	(cons "cps"
	      cps-convert)

	(cons "beta"
	      beta-reduce)

	(cons "gen"
	      (lambda (ast)
		(generate-code ast (map cdr *global-variables*))))

	(cons "code"
	      (lambda (code)
		(display ";;; LLVM:\n")
		(for-each print-llvm-line code)
		code))
	))

(define (print-llvm-line line)
  (if (and (not (char=? #\{ (string-ref line (- (string-length line) 1))))
	   (not (string=? line "}"))
	   (not (char=? #\@ (string-ref line 0))))
      (display "    "))
  (display line)
  (newline))

(main (command-line-arguments))
