; Minus half of VIA CA1 status bit test loop cycles and instructions to reinit.
restart_delay = @(+ (half (+ 4 3 3)) 8)
timer = @(- (* 8 audio_longest_pulse) restart_delay)

tape_audio_player:
if @*nipkow-disable-interrupts?*
    ; Disable interrupts.
    sei
    lda #$7f
    sta $911e
    sta $911d
    sta $912e
    sta $912d
end

    ; Start tape motor.
    lda $911c
    and #$fd
    sta $911c

    ; Initialize VIA2 timer 1.
    lda #0
    sta $912b       ; one-shot mode
    lda #<timer
    sta current_low
    ldy #>timer

    ; Play.
f:  lda $9121       ; Reset the VIA2 CA1 status bit.
l:  lda $912d       ; Read the VIA2 CA1 status bit.
    and #2
    beq -l

    lda $9124       ; Read the timer's low byte which is your sample.
    sty $9125       ; Write high byte to restart the timer.

    tax
    lsr             ; Reduce sample from 7 to 4 bits.
    lsr
    lsr
    sta $900e       ; Play it!
if @*nipkow-fx-border?*
    sta $900f       ; Something for the eye.
end

    ; Make sum of samples.
    txa
    clc
    adc average
    sta average
    bcc +n
    inc @(++ average)
    bne -f

n:  dec tleft
    bne -f

    ; Correct time if average pulse length doesn't match our desired value.
    lda @(++ average)   ; average / 256
    cmp #@(- #x40 restart_delay)
    beq +j              ; It's already what we want.
    tax
    bcc +n
    dec current_low
    bne +d
n:  inc current_low
d:  lda current_low
    sta $9124

    ; Divide average by 128 and restart summing up samples.
    txa
j:  asl
    sta average
    lda #0
    rol
    sta @(++ average)
    lda #128
    sta tleft
    bne -f      ; (4)
