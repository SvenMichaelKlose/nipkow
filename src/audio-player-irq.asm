; Minus half of VIA CA1 status bit test loop cycles and instructions to reinit.
restart_delay = @(+ (half (+ 4 3 3)) 8)
timer = @(- (* 8 audio_longest_pulse) restart_delay)

tape_audio_player:
    sei
    lda #$7f
    sta $911e
    sta $912e
    sta $912d

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
    lda #<timer     ; Cycles to count down.
    sta current_low
    sta $9124
    lda #>timer
    sta $9125

    lda #%00000000  ; One-shot mode.
    sta $912b

    ; Initialize VIA2 tape pulse IRQ.
    lda #%10000010  ; CA1 IRQ enable (tape pulse)
    sta $912e

    ; Let the IRQ handler do everthing.
    cli
w:  bne -w

play_audio_sample:
    lda $9124       ; Read the timer's low byte which is your sample.
    ldy #>timer
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
    bne +done

n:  dec tleft
    bne +done

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

done:
    lda #$7f
    sta $912d
    jmp $eb18
