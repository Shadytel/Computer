; Tim Carstens
; 2011


; Setup the registers
.lit fact       ; R0 (ip)
.lit 0x000      ; R1 (flags)
.lit stack_top  ; R2 (our call stack)
.lit 0x000      ; R3
.lit 0x000      ; R4
.lit 0x000      ; R5
.lit 0x000      ; R6
.lit 0x000      ; R7

; interrupt vector table
;.lit 0x000
;.lit 0x000
;.lit 0x000
;.lit 0x000
;.lit 0x000
;.lit 0x000
;.lit 0x000
;.lit 0x000
;.lit 0x000


; Multiplication: the function
;   r2: stack (ascending)
;   r4: gets clobbered
;   r5: gets clobbered
;   r6: gets clobbered
;   r7: where we store the result
mult:
    mov     r2, r6          ; r6 now points to the top of the stack
    insub   1, r6           ; r6 now points to the return pointer
    mov     (--r6), r7      ; r7 contains the second factor, r6 points to first factor
    mov     r7, r5          ; r5 is now a copy of the second factor
    mov     (--r6), r6      ; r6 contains the first factor
    ; now we engage in multiplication.
    ; if either factor is zero, return zero
    tst r6
    if.jmp zero_factor
    tst r7
    if.jmp zero_factor
    imov    1, r4
test_for_one:               ; if r6 is 1, we're done.
    eq      r4, r6
    if.jmp  mult_done
    add     r5, r7          ; add the factor
    dec     r6              ; decrement the multiplication count
    jmp test_for_one
zero_factor:
    imov    0, r7
mult_done:
    mov     (--r2), ip


.lit 0xfff
.lit 0xfff


; Factorial: the function
;   r2: stack (ascending)
;   r6: gets clobbered
;   r7: where we store the result
;
;   stack contents:
;     [the number to factorialize]
;     [return address]
;
fact:
    ; load the argument into r6
    mov     r2, r6          ; r6 now points to the top of the stack
    insub   2, r6           ; r6 now points to the argument
    mov     (r6), r6        ; r6 now contains the argument
    ; if the argument is less than 2, we are done
    imov    2, r7
    u.lt    r6, r7
    if.jmp arg_less_two
    ; the argument is at-least 2.  we can now compute the factorial
    ; of r6-1, then multiply by r6.
    ; to do this, we need to:
    mov     r6, (r2++)      ; 1. save our r6
    dec     r6              ; 2. dec r6
    mov     r6, (r2++)      ; 3. push the new r6 as an argument on the stack
    imov    fact_return_point, (r2++)   ; 4. push return address
    jmp     fact            ; 5. jump into fact
    fact_return_point:
    dec     r2              ; clean up after our pushed-arg
    mov     (--r2), r6      ; restore our r6
    ; now multiply
    mov     r6, (r2++)      ; push first factor
    mov     r7, (r2++)      ; push second factor
    imov    fact_done_cleanup, (r2++)   ; push return point
    jmp     mult            ; do the multiplication
fact_done_cleanup:
    insub   2, r2           ; cleanup from our call
    jmp     fact_done
arg_less_two:
    mov r6, r7
fact_done:
    mov (--r2), ip


.lit 0xfff
.lit 0xfff


loop:
    jmp loop


stack:
    .lit 0xfff
    .lit 0xfff
    .lit 0x005
    .lit loop
stack_top:

