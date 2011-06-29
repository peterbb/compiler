; Copyright (c) 2011, Peter Brottveit Bock
; Released under the BSD 3-Clause License
; See: https://raw.github.com/peterbb/compiler/master/LICENSE




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Equality:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%eq?" '("%a" "%b")
 (list
  "%res.i1 = icmp eq %t_obj %a, %b"
  (llvm:select "%res" "%res.i1" (llvm:true) (llvm:false))
  (llvm:return "%res")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Numbers:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%number?" '("%obj")
 ;;; Check if %obj is a number
 (list
  "%res.i1 = call i1 @is-number(%t_obj %obj)"
  (llvm:select "%res" "%res.i1" (llvm:true) (llvm:false))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%+" '("%a" "%b")
 (list
  (llvm:call "%res" "@add-numbers" '("%a" "%b"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%-" '("%a" "%b")
 (list
  (llvm:fixnum->word "%a.w" "%a")
  (llvm:fixnum->word "%b.w" "%b")
  "%res.w = sub i64 %a.w, %b.w"
  (llvm:word->fixnum "%res" "%res.w")
  (llvm:return "%res")))

(llvm:declare-builtin
 "%*" '("%a" "%b")
 (list
  (llvm:fixnum->word "%a.w" "%a")
  (llvm:fixnum->word "%b.w" "%b")
  "%res.w = mul i64 %a.w, %b.w"
  (llvm:word->fixnum "%res" "%res.w")
  (llvm:return "%res")))

(llvm:declare-builtin
 "%quotient" '("%a" "%b")
 (list
  (llvm:fixnum->word "%a.w" "%a")
  (llvm:fixnum->word "%b.w" "%b")
  "%res.w = sdiv i64 %a.w, %b.w"
  (llvm:word->fixnum "%res" "%res.w")
  (llvm:return "%res")))

(llvm:declare-builtin
 "%remainder" '("%a" "%b")
 (list
  (llvm:fixnum->word "%a.w" "%a")
  (llvm:fixnum->word "%b.w" "%b")
  "%res.w = srem i64 %a.w, %b.w"
  (llvm:word->fixnum "%res" "%res.w")
  (llvm:return "%res")))

(llvm:declare-builtin
 "%>" '("%a" "%b")
 (list
  "%res.i1 = call i1 @gt-numbers(%t_obj %a, %t_obj %b)"
  (llvm:select "%res" "%res.i1" (llvm:true) (llvm:false))
  (llvm:return "%res")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Characters:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%char?" '("%obj")
 (list 
  "%res.i1 = call i1 @is-char(%t_obj %obj)"
  (string-append "%res = select i1 %res.i1, %t_obj "
		 (llvm:true) ", %t_obj " (llvm:false))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%char->integer" '("%char")
 (list
  (llvm:call "%res" "@char-to-num" '("%char"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%integer->char" '("%int")
 (list
  (llvm:call "%res" "@num-to-char" '("%int"))
  (llvm:return "%res")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Pairs:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%pair?" '("%a")
 (list
  "%res.i1 = call i1 @is-pair(%t_obj %a)"
  (llvm:select "%res" "%res.i1" (llvm:true) (llvm:false))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%cons" '("%a" "%b")
 (list
  (llvm:call "%res" "@make-pair" '("%a" "%b"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%car" '("%o")
 (list
  (llvm:call "%res" "@get-car" '("%o"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%cdr" '("%o")
 (list
  (llvm:call "%res" "@get-cdr" '("%o"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%set-car!" '("%p" "%v")
 (list
  "call void @set-car(%t_obj %p, %t_obj %v)"
  (llvm:return (llvm:false))))

(llvm:declare-builtin
 "%set-cdr!" '("%p" "%v")
 (list
  "call void @set-cdr(%t_obj %p, %t_obj %v)"
  (llvm:return (llvm:false))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Symbols:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%symbol?" '("%o")
 (list
  "%tag = and %t_obj %o, 7"
  (string-append "%res.1 = icmp eq %t_obj %tag, " (llvm:symbol-tag))
  (llvm:select "%res" "%res.1" (llvm:true) (llvm:false))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%symbol->string" '("%o")
 (list
  "%untagged = and %t_obj %o, sext(i4 8 to %t_obj)"
  (string-append "%tagged = or %t_obj %untagged, " (llvm:string-tag))
  (llvm:return "%tagged")))

(llvm:declare-builtin
 "%intern" '("%s")
 (list
  (llvm:call "%res" "@intern" '("%s"))
  (llvm:return "%res")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IO:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%write-char" '("%char")
 (list
  "%value = call i64 @char-to-i64(%t_obj %char)"
  "%value.32 = trunc i64 %value to i32"
  "call i32 @putchar(i32 %value.32)"
  (llvm:return (llvm:nil))))

(llvm:declare-builtin
 "%read-char" '()
 (list
  "%value.i32 = call i32 @getchar()"
  "%is-eof = icmp eq i32 %value.i32, -1"
  "%value.i64 = sext i32 %value.i32 to i64"
  "%value.obj = call %t_obj @i64-to-char(i64 %value.i64)"
  (llvm:select "%result" "%is-eof" (llvm:eof) "%value.obj")
  (llvm:return "%result")))

(llvm:declare-builtin
 "%eof-object?" '("%o")
 (list 
  (string-append "%is-eof = icmp eq %t_obj %o, " (llvm:eof))
  (llvm:select "%result" "%is-eof" (llvm:true) (llvm:false))
  (llvm:return "%result")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Strings:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%string?" '("%s")
 (list
  "%res.i1 = call i1 @is-string(%t_obj %s)"
  (llvm:select "%res" "%res.i1" (llvm:true) (llvm:false))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%string-ref" '("%s" "%i")
 (list
  (llvm:call "%res" "@string-ref" '("%s" "%i"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%string-set!" '("%s" "%i" "%v")
 (list
  "call void @string-set(%t_obj %s, %t_obj %i, %t_obj %v)"
  (llvm:return (llvm:false))))

(llvm:declare-builtin
 "%string-length" '("%s")
 (list
  (llvm:call "%res" "@string-length" '("%s"))
  (llvm:return "%res")))

(llvm:declare-builtin
 "%make-string" '("%len")
 (list
  (llvm:call "%res" "@make-string" '("%len"))
  (llvm:return "%res")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Apply:
;;;;-------------------------------------------
(llvm:declare-builtin
 "%apply" '("%proc" "%arity" "%args")
 (list
  (llvm:cons "%args.k" "%k" "%args")
  (llvm:tail-call "@apply" '("%proc" "%arity" "%args.k"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; call-with-current-continuation:
;;;;-------------------------------------------
(llvm:declare-builtin
 "%call-with-current-continuation" '("%proc")
 (list
  (llvm:cons "%args.1" "%k" (llvm:nil))
  (llvm:cons "%args.2" "%k" "%args.1")
  (llvm:tail-call "@apply" (list "%proc" (llvm:number 2) "%args.2"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; %halt:
;;;;-------------------------------------------

(llvm:declare-builtin
 "%halt" '()
 (list "call void @exit(i32 123) noreturn"
       "unreachable"))
