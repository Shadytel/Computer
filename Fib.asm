; Tim Carstens
; 2011

; Fibonacci calculator
;   r5: one number
;   r6: other number
;   r7: number of iterations
fib:
    tst     r7          ; check to see if we have no iterations left
    if.jmp  skip        ; if not, jump to the end
    mov     r6, r4      ; save the high number
    add     r5, r6      ; compute the new high number
    mov     r4, r5      ; move the high number to the low number
    dec     r7          ; decrement our iteration count
    jmp     fib         ; loop back to do more
    skip:

