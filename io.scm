 
(define (io:read-file filename)
  (printf ";; [old-lisp] loading '~a'~%" filename)
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