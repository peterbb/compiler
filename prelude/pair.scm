; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (null? x) (eq? x '()))

(define (pair? x) (%pair? x))

(define (cons x y) (%cons x y))

(define (list . x) x)

(define (%assert-pair caller obj)
  (if (not (pair? obj))
      (error (string-append caller " expected pair, got:" obj))))

(define (car x)
  (%assert-pair "car" x)
  (%car x))

(define (cdr x)
  (%assert-pair "cdr" x)
  (%cdr x))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

(define (set-car! x y) 
  (%assert-pair "set-car!" x)
  (%set-car! x y))

(define (set-cdr! x y)
  (%assert-pair "set-cdr!" x)
  (%set-cdr! x y))
