main = $351

screen = $1e00
colors = $9600

current_low     = 0
current_high    = 1
average         = 2
tleft           = 4
old_irq         = 5
tmp             = 7
do_invert       = 8

    org $120c

    ; Print text.
l:  lda @text,x
    beq +n
    jsr $ffd2
    inx
    bne -l
    inc @(+ -l 2)
    jmp -l
n:
