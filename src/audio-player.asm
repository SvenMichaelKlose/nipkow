current_low = 0
average = 1
tleft = 3

timer = @(* 8 audio_longest_pulse)

tape_audio_player:
    ; Disable interrupts.
    sei
    lda #$7f
    sta $911e
    sta $911d
    sta $912e
    sta $912d

    ; Start tape motor.
    lda $911c
    and #$fd
    sta $911c

    ; Initialize VIA2 timer 1.
    lda #0
    sta $912b   ; one-shot mode
    lda #<timer
    sta current_low
    ldy #>timer

    ; Play.
f:  lda $9121   ; (4) Reset the VIA2 CA1 status bit.
l:  lda $912d   ; (4) Read the VIA2 CA1 status bit.
    lsr         ; (2) Shift to test bit 2.
    lsr         ; (2)
    bcc -l      ; (2/3) Nothing happened yet. Try again…

    lda $9124   ; (4) Read the timer's low byte which is your sample.
    sty $9125   ; (4) Write high byte to restart the timer.
    tax
    lsr         ; (2) Reduce sample from 7 to 4 bits.
    lsr         ; (2)
    lsr         ; (2)
    sta $900e   ; (4) Play it!
    sta $900f   ; (4) Something for the eye.

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
    tax
    cmp #$40
    beq +j              ; It's already what we want.
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
