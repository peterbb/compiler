; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (boolean? x)
  (or (eq? x #f)
      (eq? x #t)))

(define (not x)
  (eq? x #f))
