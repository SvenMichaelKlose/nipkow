screen = $1e00
colors = $9600

current_low     = 0
average         = 1
tleft           = 3
old_irq         = 4
tmp             = 6

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
