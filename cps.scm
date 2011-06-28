; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE




(define (make-application1 op arg)
  (make-application op (list arg)))

(define cps-convert
  (lambda (exp*)
    (make-application1
     (cps-convert-cmd* exp*)
     (make-halt-continuation))))

(define cps-convert-cmd
  (lambda (cmd)
    (if (definition? cmd)
	(cps-convert-def cmd)
	(cps-convert-exp cmd))))

(define cps-convert-cmd*
  (lambda (cmd*)
    (if (not (list? cmd*))
	(error "cps-convert-cmd*"))
    (cps-convert-F* cmd* cps-convert-cmd cps-convert-cmd*)))
    

;; F[(define v e)] ==>
;; (ar (k)
;;   (F[e]
;;    (ar (t) (define/k v t k))))
(define cps-convert-def
  (lambda (def)
    (let ((k (make-local-variable (gensym)))
	  (t (make-local-variable (gensym)))
	  (e (definition-expression def)))
      (make-activation-record
       k
       (make-application1
	(cps-convert-exp e)
	(make-activation-record
	 t
	 (make-definition/k (definition-variable def) t k)))))))

(define cps-convert-exp
  (lambda (exp)
    (cond ((constant? exp)
	   (cps-convert-const exp))
	  ((variable? exp)
	   (cps-convert-var exp))
	  ((if? exp)
	   (cps-convert-if exp))
	  ((lambda? exp)
	   (cps-convert-lambda exp))
	  ((application? exp)
	   (cps-convert-app exp))
	  ((assignment? exp)
	   (cps-convert-set! exp))
	  (else
	   (error "cps-convert-exp" exp)))))

;; F*[e] ==> F[e]
;; F*[e e*] ==>
;;  (ar (k)
;;    (F[e] (ar (t) (F*[e*] k))))
(define cps-convert-F*
  (lambda (exp* F F*)
    (if (null? (cdr exp*))
	(F (car exp*))
	(let ((t (make-local-variable (gensym)))
	      (k (make-local-variable (gensym))))
	  (make-activation-record 
	   k
	   (make-application1
	    (F (car exp*))
	    (make-activation-record
	     t
	     (make-application1 (F* (cdr exp*)) k))))))))
				 

(define cps-convert-exp*
  (lambda (exp*)
    (cps-convert-F* exp* cps-convert-exp cps-convert-exp*)))

;; F[c] ==> (ar (k) (k c))
(define cps-convert-const
  (lambda (exp)
    (let ((k (make-local-variable (gensym))))
      (make-activation-record
       k
       (make-application1 k exp)))))

(define cps-convert-var cps-convert-const)


(define cps-convert-lambda
  (lambda (exp)
    (let ((k1 (make-local-variable (gensym)))
	  (k2 (make-local-variable (gensym)))
	  (vars (lambda-variable* exp))
	  (body (lambda-body exp)))
      (make-activation-record
       k1
       (make-application1
	k1
	(make-lambda
	 (cons k2 vars)
	 (make-application1 (cps-convert-exp body) k2)
	 (lambda-debug-name exp)))))))

;;; F[(if e1 e2 e3)] ==> ?
(define cps-convert-if
  (lambda (exp)
    (let ((k (make-local-variable (gensym)))
	  (t (make-local-variable (gensym)))
	  (pred (if-predicate exp))
	  (con (if-consequence exp))
	  (alt (if-alternative exp)))
      (make-activation-record
       k
       (make-application1
	(cps-convert-exp pred)
	(make-activation-record
	 t
	 (make-if t
		  (make-application1 (cps-convert-exp con) k)
		  (make-application1 (cps-convert-exp alt) k))))))))


;;; F[(set! x e)] ==> ???
(define cps-convert-set!
  (lambda (exp)
    (let ((k (make-local-variable (gensym)))
	  (t (make-local-variable (gensym)))
	  (e (assignment-expression exp)))

      (make-activation-record
       k
       (make-application1 (cps-convert-exp e)
			  (make-activation-record
			   t
			   (make-assignment/k
			    (assignment-variable exp) t k)))))))

(define cps-convert-app
  (lambda (exp)
    (let ((k (make-local-variable (gensym)))
	  (t (make-local-variable (gensym))))
      (make-activation-record
       k
       (make-application1
	(cps-convert-exp (application-procedure exp))
	 (make-activation-record
	  t
	  (cps-convert-app0 (application-arguments exp)
			    (list k t))))))))



(define cps-convert-app0
  (lambda (exp* vars)
    (if (null? exp*)
	(let ((vars (reverse vars)))
	  (make-application (car vars) (cdr vars)))
	(let ((t (make-local-variable (gensym)))
	      (e (car exp*))
	      (e* (cdr exp*)))
	  (make-application1
	   (cps-convert-exp e) 
	    (make-activation-record
	     t
	     (cps-convert-app0 e* (cons t vars))))))))
