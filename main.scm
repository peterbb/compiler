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


;;; Pipeline:
;;; parse -> alpha -> cps -> beta

(define (compile filename k)
  (let* ((file-content (io:read-file filename))
	 (with-prelude (cons (list 'load "prelude.scm") file-content))
	 ;(with-prelude file-content)
	 (ast* (parse with-prelude)))
    (k ast* (map cdr *global-variables*))))

(define (make-print header)
  (lambda (k)
    (lambda (ast globals)
      (printf ";;;; ~a~%" header)
      (map pp (ast->scheme/with-globals ast globals))
      (printf "~%")
      (k ast globals))))

(define (make-raw-print* header)
  (lambda (k)
    (lambda (ast globals)
      (printf ";;;; ~a~%" header)
      (pp ast)
      (printf "~%")
      (k ast globals))))

(define (cps-stage k)
  (lambda (ast globals)
    (k (cps-convert ast) globals)))

(define (beta-stage k)
  (lambda (ast globals)
    (k (beta-reduce ast) globals)))

(define (gen-stage k)
  (lambda (ast globals)
    (k (generate-code ast globals)
       globals)))

(define (gen-print k)
  (lambda (code globals)

    (define (display/line x)
      (if (and (not (char=? #\{ (string-ref x (- (string-length x) 1))))
	       (not (string=? x "}"))
	       (not (char=? #\@ (string-ref x 0))))
	  (printf "    "))
      
      (display x)
      (newline))

    (map display/line code)
    (k code globals)))

(define done
  (lambda (ast globals)
    (printf ";;; Bye~%")))

(define (compose-stages s1 s2)
  (lambda (k)
    (s1 (s2 k))))

;;;;;;
(define stages
  (list	(cons "raw-print" (make-raw-print* "ast"))
	(cons "cps" cps-stage)
	(cons "beta" beta-stage)
	(cons "code" gen-stage)
	(cons "code-print" (compose-stages gen-stage gen-print))))

(define (make-pipeline args)
  (if (null? args)
      done
      (let ((stage (assoc (car args) stages)))
	(if stage
	    ((cdr stage) (make-pipeline (cdr args)))
	    (error "unkown argument:" (car args))))))



(let* ((args (command-line-arguments))
       (file (car args))
       (stages (make-pipeline (cdr args))))
  (compile file stages))
