; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

(define (symbol? x) (%symbol? x))

(define (%assert-symbol caller obj)
  (if (not (symbol? obj))
      (error (string-append caller " expected symbol, got something else"))))

(define (symbol->string x)
  (%assert-symbol "symbol->string" x)
  (string-copy (%symbol->string x)))

(define (string->symbol x)
  (%assert-string "string->symbol" x)
  (%intern (string-copy x)))
  
