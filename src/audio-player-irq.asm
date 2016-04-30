irq_break_delay = @(half 3)
irq_delay = 7
irq_handler_delay = 29
restart_delay = @(+ irq_break_delay irq_delay irq_handler_delay)

timer = @(- (* 8 (nipkow-longest-pulse)) restart_delay)

tape_audio_player:
if @*nipkow-disable-interrupts?*
    sei
    lda #$7f
    sta $911e
    sta $912e
    sta $912d
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

    ; Set IRQ vector.
    lda #<play_audio_sample
    sta $314
    lda #>play_audio_sample
    sta $315

    ; Initialise VIA2 Timer 1 (cassette tape read).
    lda #<timer
    sta current_low
    lda #>timer
    sta current_high
    sta $9124
    lda #>timer
    sta $9125

    lda #%00000000  ; One-shot mode.
    sta $912b
    lda #%10000010  ; CA1 IRQ enable (tape pulse)
    sta $912e

    ; Let the IRQ handler do everthing.
    cli
w:  bne -w

play_audio_sample:
    lda $9124       ; Read the timer's low byte which is your sample.
    ldy current_high
    sty $9125       ; Write high byte to restart the timer.

    ; Clip sample.
    pha
    bpl +n
    cmp #196
    bcc +s
    lda #0
    beq +n
s:  lda #127

n:  lsr             ; Reduce sample from 7 to 4 bits.
    lsr
    lsr

if @*nipkow-inversions?*
    tax
    inc do_invert
    lda do_invert
    lsr
    bcc +j
    txa
    jmp +m
j:  lda inversions,x
end

m:  sta $900e       ; Play it!
if @*nipkow-fx-border?*
    sta $900f       ; Something for the eye.
end

    ; Make sum of samples.
    pla
    clc
    adc average
    sta average
    bcc +n
    inc @(++ average)

n:  dec tleft
    bne +done

    ; Correct time if average pulse length doesn't match our desired value.
    lda @(++ average)   ; average / 256
    cmp #$3f
    beq +r              ; It's already what we want.
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

r:  lda #0
    sta average
    sta @(++ average)

done:
    lda #$7f
    sta $912d
    jmp $eb18

inversions:
    15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
