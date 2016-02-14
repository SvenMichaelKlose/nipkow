; Minus half of VIA CA1 status bit test loop cycles and instructions to reinit.
restart_delay = @(+ (half (+ 4 3)) 8)
timer = @(- (* 8 (nipkow-longest-pulse)) restart_delay)

tape_audio_player:
if @*nipkow-disable-interrupts?*
    sei
    lda #$7f
    sta $911d
    sta $911e
    sta $912d
    sta $912e
end

    ; Boost digital audio with distorted HF carrier.
    lda #$0f
    sta $900e
    ldx #$7e
    stx $900c
    ldy #0
l:  dey
    bne -l
    lda #$fe
    stx $900c
    stx $900c
    sta $900c
    sta $900c
    stx $900c
    sta $900c

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
    beq -l

    ldx $9124       ; Read the timer's low byte which is your sample.
    sty $9125       ; Write high byte to restart the timer.

    asl
    bmi -l          ; Tape must have been stopped.

    ; Clip sample.
    txa
    bpl +n
    cmp #196
    bcc +s
    lda #0
    beq +n
s:  lda #127

n:  lsr             ; Reduce sample from 7 to 4 bits.
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

n:  dec tleft
    bne -f

    ; Correct time if average pulse length doesn't match our desired value.
    lda @(++ average)   ; average / 256
    cmp #$3f
    beq +done           ; It's already what we want.
    bcc +n
    dec current_low
    lda current_low
    cmp #$ff
    bne +d
    dec current_high
    jmp +d
n:  inc current_low
    bne +d
    inc current_high
d:  lda current_low
    sta $9124
    lda #0
    sta average
    sta @(++ average)

    lda #0
    sta average
    sta @(++ average)
    beq -f
