main:
    ; Print text.
l:  lda @text,x
    beq +n
    jsr $ffd2
    inx
    bne -l
    inc @(+ -l 2)
    jmp -l
n:
