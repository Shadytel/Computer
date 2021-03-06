; Tim Carstens
; 2011


; Setup the registers
.lit fib        ; R0 (ip)
.lit 0x000      ; R1 (flags)
.lit 0x000      ; R2
.lit 0x000      ; R3
.lit 0x000      ; R4
.lit 0x001      ; R5
.lit 0x001      ; R6
.lit 0x005      ; R7

; interrupt vector table
.lit 0x000
.lit 0x000
.lit 0x000
.lit 0x000
.lit 0x000
.lit 0x000
.lit 0x000
.lit 0x000
.lit 0x000

; 1 1 2 3 5 8 13
;     1 2 3 4 5

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
    jmp skip

