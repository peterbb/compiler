; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (char? x) (%char? x))

(define (%assert-char caller obj)
  (if (not (char? obj))
      (error (string-append caller " expected character, got:") obj)))

(define (char->integer x)
  (%assert-char "char->integer" x)
  (%char->integer x))

(define (integer->char x)
  (%assert-number "integer->char" x)
  (%integer->char x))

(define (char->number c)
  (%assert-char "char->number" c)
  (if (and (char<=? #\0 c) (char<=? c #\9))
      (- (char->integer c) (char->integer #\0))
      (error "char->number given non-numeric character")))

(define (%make-char-pred name op)
  (lambda (x y)
    (%assert-char name x)
    (%assert-char name y)
    (op (char->integer x) (char->integer y))))

(define char=? (%make-char-pred "char=?" =))
(define char<? (%make-char-pred "char<?" <))
(define char<=? (%make-char-pred "char<=?" <=))
(define char>? (%make-char-pred "char>?" >))
(define char>=? (%make-char-pred "char>=?" >=))

(define (char-numeric? c)
  (%assert-char "char-numeric?" c)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (char-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)))
