
declare void @exit(i64)
declare i64* @malloc(i64)
declare i64* @calloc(i64, i64)
declare i64 @puts(i8*)
declare i32 @getchar()
declare i32 @putchar(i32)
declare i32 @printf(i8*, ...)
declare i32 @ungetc(i32, i8*)
declare i64* @fdopen(i64, i8*)

@stdin = global i64* null

define void @init-stdin() {
    %file = call i64* @fdopen(i64 0, i8* bitcast([2 x i8]* @stdin-mode to i8*))
    store i64* %file, i64** @stdin
    ret void
}

@stdin-mode = constant [2 x i8] c"w\00"

;;; Tags:
;;; Number:      00
;;; Character : 001
;;; Pair:       010
;;; Symbol:     011           
;;; Closure:    101
;;; String      110
;;; Other:      111
;;;     -   111 #f
;;;     -  1111 #t
;;;     - 10111 ()
;;;     - 11111 EOF

%t_obj = type i64
%t_code = type void(%t_obj, %t_obj)*
%t_pair = type { %t_obj, %t_obj } ; {car, cdr}
%t_closure = type { %t_code, %t_obj } ; { code, env }
%t_string = type { %t_obj, [0 x %t_obj ] }


;;;=================================================================
;;; Numbers
;;;=================================================================
define %t_obj @i64-to-number(i64 %num) {
    %res = shl i64 %num, 2
    ret %t_obj %res
}

define i64 @num-to-i64(%t_obj %num) {
    %res = ashr i64 %num, 2
    ret i64 %res
}

define i1 @is-number(%t_obj %a) {
    %untagged = and %t_obj %a, 3
    %res = icmp eq %t_obj %untagged, 0
    ret i1 %res
}

define %t_obj @add-numbers(%t_obj %a, %t_obj %b) {
    %res = add nsw %t_obj %a, %b
    ret %t_obj %res
}

define %t_obj @sub-numbers(%t_obj %a, %t_obj %b) {
    %res = sub nsw %t_obj %a, %b
    ret %t_obj %res
}

define %t_obj @mul-numbers(%t_obj %a, %t_obj %b) {
    %res = mul nsw %t_obj %a, %b
    ret %t_obj %res
}

define %t_obj @div-numbers(%t_obj %a, %t_obj %b) {
    %res = sdiv %t_obj %a, %b
    ret %t_obj %res
}

define i1 @eq-numbers(%t_obj %a, %t_obj %b) {
    %res = icmp eq i64 %a, %b
    ret i1 %res
}

define i1 @gt-numbers(%t_obj %a, %t_obj %b) {
    %res = icmp sgt i64 %a, %b
    ret i1 %res
}

;;;=================================================================
;;; Characters
;;;=================================================================

define %t_obj @i64-to-char(i64 %val) {
    %untagged = shl i64 %val, 3
    %tagged = or i64 %untagged, 1
    ret %t_obj %tagged
}

define i64 @char-to-i64(%t_obj %char) {
    %res = lshr i64 %char, 3
    ret i64 %res
}

define i1 @is-char(%t_obj %o) {
    %tag = and i64 %o, 7
    %res = icmp eq i64 %tag, 1
    ret i1 %res
}

define %t_obj @char-to-num(%t_obj %char) {
    %res = lshr i64 %char, 1
    ret %t_obj %res
}

define %t_obj @num-to-char(%t_obj %num) {
    %1 = shl i64 %num, 1
    %2 = or i64 %1, 1
    ret %t_obj %2
}


;;;=================================================================
;;; Boolean
;;;=================================================================

define %t_obj @get-true() {
    ret %t_obj 15 ; 1111
}

define %t_obj @get-false() {
    ret %t_obj 7  ; 111
}

;;;=================================================================
;;; nil
;;;=================================================================

define %t_obj @get-nil() {
    ret %t_obj 23 ; 10111
}

;;;=================================================================
;;; pair
;;;=================================================================

define %t_obj @make-pair(%t_obj %a, %t_obj %b) {
    %raw_ptr = call i64* @malloc(i64 16)
    %pair_ptr = bitcast i64* %raw_ptr to %t_pair*

    %car_ptr = getelementptr %t_pair* %pair_ptr, i32 0, i32 0
    %cdr_ptr = getelementptr %t_pair* %pair_ptr, i32 0, i32 1

    store %t_obj %a, %t_obj* %car_ptr
    store %t_obj %b, %t_obj* %cdr_ptr

    %untagged_obj = ptrtoint %t_pair* %pair_ptr to %t_obj
    %tagged_obj = or %t_obj %untagged_obj, 2

    %untagged_tag = and %t_obj %untagged_obj, 7
    %aligned-ok = icmp eq %t_obj %untagged_tag, 0
    br i1 %aligned-ok, label %return, label %signal-error
return:
    ret %t_obj %tagged_obj

signal-error:
    call void @scheme-error(i8* bitcast([28 x i8]* @make-pair-error to i8*)) noreturn
    unreachable
}
@make-pair-error = constant [28 x i8] c"make-pair: alignment error\0A\00"

define i1 @is-pair(%t_obj %o) {
    %tag = and %t_obj %o, 7
    %res = icmp eq %t_obj %tag, 2
    ret i1 %res
}

define %t_obj* @get-car-location(%t_obj %pair) {
    %untagged = and %t_obj %pair, sext(i4 8 to %t_obj)
    %pair_ptr = inttoptr %t_obj %untagged to %t_pair*
    %car_ptr = getelementptr %t_pair* %pair_ptr, i32 0, i32 0
    ret %t_obj* %car_ptr
}

define %t_obj @get-car(%t_obj %pair) {
    %valid = call i1 @is-pair(%t_obj %pair)
    br i1 %valid, label %ok, label %signal-error
ok:
    %car_ptr = call %t_obj* @get-car-location(%t_obj %pair)
    %value = load %t_obj* %car_ptr
    ret %t_obj %value

signal-error:
    call void @scheme-error(i8* bitcast([12 x i8]* @get-car-error to i8*))
    unreachable
}
@get-car-error = constant [12 x i8] c"error %car\0A\00"

define void @set-car(%t_obj %pair, %t_obj %value) {
    %car_ptr = call %t_obj* @get-car-location(%t_obj %pair)
    store %t_obj %value, %t_obj* %car_ptr
    ret void
}

define %t_obj* @get-cdr-location(%t_obj %pair) {
    %untagged = and %t_obj %pair, sext(i4 8 to %t_obj)
    %pair_ptr = inttoptr %t_obj %untagged to %t_pair*
    %cdr_ptr = getelementptr %t_pair* %pair_ptr, i32 0, i32 1
    ret %t_obj* %cdr_ptr
}

define %t_obj @get-cdr(%t_obj %pair) {
    %valid = call i1 @is-pair(%t_obj %pair)
    br i1 %valid, label %ok, label %signal-error
ok:
    %cdr_ptr = call %t_obj* @get-cdr-location(%t_obj %pair)
    %value = load %t_obj* %cdr_ptr
    ret %t_obj %value
signal-error:
    call void @scheme-error(i8* bitcast([12 x i8]* @get-cdr-error to i8*))
    unreachable
}
@get-cdr-error = constant [12 x i8] c"error %cdr\0A\00"

define void @set-cdr(%t_obj %pair, %t_obj %value) {
    %cdr_ptr = call %t_obj* @get-cdr-location(%t_obj %pair)
    store %t_obj %value, %t_obj* %cdr_ptr
    ret void
}

;;;=================================================================
;;; closure
;;;=================================================================
;;; %t_closure = type { %t_code, %t_obj } ; { code, env }

define %t_obj @make-closure(%t_code %code, %t_obj %env) {
    %raw_ptr = call i64* @malloc(i64 12)
    %closure_ptr = bitcast i64* %raw_ptr to %t_closure*

    %code_ptr =  getelementptr %t_closure* %closure_ptr, i32 0, i32 0
    %env_ptr =   getelementptr %t_closure* %closure_ptr, i32 0, i32 1

    store %t_code %code,  %t_code* %code_ptr
    store %t_obj  %env,   %t_obj*  %env_ptr

    %untagged_ptr = ptrtoint %t_closure* %closure_ptr to %t_obj
    %tagged_ptr = or %t_obj %untagged_ptr, 5

    ret %t_obj %tagged_ptr
}

define i1 @is-closure(%t_obj %o) {
    %tag = and %t_obj %o, 7
    %res = icmp eq %t_obj %tag, 5
    ret i1 %res
}

define %t_code @get-code(%t_obj %o) {
    %untagged_o = and %t_obj %o, sext(i4 8 to %t_obj)
    %closure_ptr = inttoptr %t_obj %untagged_o to %t_closure*
    %code_ptr = getelementptr %t_closure* %closure_ptr, i32 0, i32 0
    %code = load %t_code* %code_ptr
    ret %t_code %code
}

define %t_obj @get-env(%t_obj %o) {
    %untagged_o = and %t_obj %o, sext(i4 8 to %t_obj)
    %closure_ptr = inttoptr %t_obj %untagged_o to %t_closure*
    %env_ptr = getelementptr %t_closure* %closure_ptr, i32 0, i32 1
    %env = load %t_obj* %env_ptr
    ret %t_obj %env
}

define cc 10 void @apply(%t_obj %closure, %t_obj %given.arity, %t_obj %args) noreturn {
    %closure-ok = call i1 @is-closure(%t_obj %closure)
    br i1 %closure-ok, label %next, label %not-closure
next:
    %closure.code = call %t_code(%t_obj)* @get-code(%t_obj %closure)
    %closure.env = call %t_obj @get-env(%t_obj %closure)

    %new-env = call %t_obj @make-pair(%t_obj %args, %t_obj %closure.env)
    tail call cc 10 void %closure.code(%t_obj %new-env, %t_obj %given.arity) noreturn
    unreachable

not-closure:
    call void @print-int(i64 %closure)
    call void @scheme-error(i8* bitcast([20 x i8]* @closure-error-msg to i8*))
    unreachable
}
@closure-error-msg = constant[20 x i8] c"error: apply: type\0A\00"


;;;=================================================================
;;; String (really just a tagged vector)
;;;=================================================================
; %t_string = type { %t_obj, [0 x %t_obj ] }

define %t_obj @make-string(%t_obj %num) {
    %elements.1 = call i64 @num-to-i64(%t_obj %num)
    %elements = add %t_obj %elements.1, 1
    %raw_ptr = call i64* @calloc(i64 8, i64 %elements)
    %str_ptr = bitcast i64* %raw_ptr to %t_string*
    %size_ptr = getelementptr %t_string* %str_ptr, i32 0, i32 0
    store %t_obj %num, %t_obj* %size_ptr

    %untagged = ptrtoint i64* %raw_ptr to %t_obj
    %tagged = or %t_obj %untagged, 6
    ret %t_obj %tagged
}

define i1 @is-string(%t_obj %o) {
    %tag = and %t_obj %o, 7
    %res = icmp eq %t_obj %tag, 6
    ret i1 %res
}

define %t_obj @string-length(%t_obj %tagged_obj) {
    %untagged_obj = and %t_obj %tagged_obj, sext(i4 8 to %t_obj)
    %str_ptr = inttoptr %t_obj %untagged_obj to %t_string*
    %size_ptr = getelementptr %t_string* %str_ptr, i32 0, i32 0
    %size = load %t_obj* %size_ptr
    ret %t_obj %size
}

define %t_obj* @string-location(%t_obj %string_obj, %t_obj %scm_num) {
    %untagged_obj = and %t_obj %string_obj, sext(i4 8 to %t_obj)
    %string_ptr = inttoptr %t_obj %untagged_obj to %t_string*
    %index = call i64 @num-to-i64(%t_obj %scm_num)
    %loc = getelementptr %t_string* %string_ptr, i32 0, i32 1, i64 %index
    ret %t_obj* %loc
}

define %t_obj @string-ref(%t_obj %scm_str, %t_obj %scm_num) {
    %loc = call %t_obj* @string-location(%t_obj %scm_str, %t_obj %scm_num)
    %res = load %t_obj* %loc
    ret %t_obj %res
}

define void @string-set(%t_obj %scm_str, %t_obj %scm_num, %t_obj %val) {
    %loc = call %t_obj* @string-location(%t_obj %scm_str, %t_obj %scm_num)
    store %t_obj %val, %t_obj* %loc
    ret void
}

;;;=================================================================
;;; Symbol
;;;=================================================================
;;; Really just a string with a symbol tag. Interned in a linked list.

@symbol-table = global %t_obj 23 ; nil

define %t_obj @intern(%t_obj %string) {
    %table = load %t_obj* @symbol-table
    %result = call %t_obj @intern0(%t_obj %string, %t_obj %table)
    ret %t_obj %result
}

define %t_obj @intern0(%t_obj %string, %t_obj %table) {
    %end-of-table = icmp eq %t_obj %table, 23 ; nil
    br i1 %end-of-table, label %intern-string, label %search

search:
    %pair = call %t_obj @get-car(%t_obj %table)
    %current_string = call %t_obj @get-car(%t_obj %pair)
    %found = call i1 @string-cmp(%t_obj %string, %t_obj %current_string)
    br i1 %found, label %return-found, label %continue-search

continue-search:
    %rest-table = call %t_obj @get-cdr(%t_obj %table)
    %result.1 = tail call %t_obj @intern0(%t_obj %string, %t_obj %rest-table)
    ret %t_obj %result.1

return-found:
    %result.2 = call %t_obj @get-cdr(%t_obj %pair)
    ret %t_obj %result.2

intern-string: ; retag to 011
    %untagged = and %t_obj %string, sext(i4 8 to %t_obj)
    %symbol = or %t_obj %untagged, 3
    %binding = call %t_obj @make-pair(%t_obj %string, %t_obj %symbol)
    %new-table = call %t_obj @make-pair(%t_obj %binding, %t_obj %table)
    store %t_obj %new-table, %t_obj* @symbol-table
    ret %t_obj %symbol
}

define i1 @string-cmp(%t_obj %s1, %t_obj %s2) {
entry:
    %len1 = call %t_obj @string-length(%t_obj %s1)
    %len2 = call %t_obj @string-length(%t_obj %s2)
    %len-eq = icmp eq %t_obj %len1, %len2
    br i1 %len-eq, label %loop, label %return-false

loop:
    %i = phi %t_obj [ 0,  %entry ], [ %next-i, %in-loop ]
    %done = icmp eq %t_obj %i, %len1
    br i1 %done, label %return-true, label %in-loop

in-loop:
    %c1 = call %t_obj @string-ref(%t_obj %s1, %t_obj %i)
    %c2 = call %t_obj @string-ref(%t_obj %s2, %t_obj %i)
    %next-i = call %t_obj @add-numbers(%t_obj %i, %t_obj 4)
    %char-eq = icmp eq %t_obj %c1, %c2
    br i1 %char-eq, label %loop, label %return-false

return-false:
    ret i1 0

return-true:
    ret i1 1
}

;;;=================================================================
;;; The halt closure
;;;=================================================================

define cc 10 void @halt-continuation-proc(%t_obj %env, %t_obj %arity) {
    call i64 @puts(i8* bitcast([9 x i8]* @exit-msg to i8*))
    call void @exit(i64 0) noreturn
    unreachable
}
@halt-closure = constant %t_closure { %t_code @halt-continuation-proc, %t_obj 23 }, align 8
@exit-msg = constant [9 x i8] c"\0A\0A;halt\0A\00"


;;;=================================================================
;;; Signal runtime errors
;;;=================================================================

define void @scheme-error(i8* %msg) noreturn {
    call i64 @puts(i8* %msg)
    call void @exit(i64 1) noreturn
    unreachable
}

;;; Helper function:
define private protected cc 10 void @apply1(%t_obj %k, %t_obj %val) noreturn {
    %nil = call %t_obj @get-nil()
    %args = call %t_obj @make-pair(%t_obj %val, %t_obj %nil)
    tail call cc 10 void @apply(%t_obj %k, %t_obj 4, %t_obj %args) noreturn
    unreachable
}

define void @print-int(i64 %val) {
    %fmt = bitcast [5 x i8]* @print-int-fmt to i8*
   
    call i32(i8*, ...)* @printf(i8* %fmt, i64 %val)
    ret void
}

@print-int-fmt = constant [5 x i8] c"%ld\0A\00"

define void @scheme-arity-error() noreturn {
    call void @scheme-error(i8* bitcast([15 x i8]* @arity-error-msg to i8*))
    unreachable
}
@arity-error-msg = constant [15 x i8] c"\0Aerror: arity\0A\00"
