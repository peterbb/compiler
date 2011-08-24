; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE


;; Predicates
(define (number? x)
  (%number? x))

(define (%assert-number caller obj)
  (if (not (number? obj))
      (error (string-append caller "expected number, got:") obj)))

(define (%assert-number2 caller obj1 obj2)
  (%assert-number caller obj1)
  (%assert-number caller obj2))

(define (= x y)
  (%assert-number2 "=" x y)
  (%eq? x y))

(define (zero? n)
  (%assert-number "zero?" n)
  (= n 0))

(define (< x y)
  (%assert-number2 "<" x y)
  (> y x))

(define (<= x y)
  (%assert-number2 "<=" x y)
  (or (= x y)
      (< x y)))

(define (> x y)
  (%assert-number2 ">" x y)
  (%> x y))

(define (>= x y)
  (%assert-number2 ">=" x y)
  (or (= x y)
      (> x y)))

;; Operations on numbers
(define (+ x y)
  (%assert-number2 "+" x y)
  (%+ x y))

(define (- x y)
  (%assert-number2 "-" x y)
  (%- x y))

(define (* x y)
  (%assert-number2 "*" x y)
  (%* x y))

(define (quotient x y)
  (%assert-number2 "quotient" x y)
  (if (= y 0)
      (error "quotient: division by zero")
      (%quotient x y)))

(define (remainder x y)
  (%assert-number2 "quotient" x y)
  (if (= y 0)
      (error "remainder: division by zero")
      (%remainder x y)))
