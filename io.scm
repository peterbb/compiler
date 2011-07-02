; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE

 
(define (io:read-file filename)
  (display ";; loading '")
  (display filename)
  (display "'")
  (newline)
  (let ((stream (open-input-file filename)))
    (let ((content (io:read-file-content stream)))
      (close-input-port stream)
      content)))

(define (io:read-file-content stream)
  (define (loop l)
    (let ((o (read stream)))
      (if (eof-object? o)
	  (reverse l)
	  (loop (cons o l)))))
  (loop '()))
