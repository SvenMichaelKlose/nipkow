; nipkow – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

current_low = 0
average = 1
tleft = 3
tmp = 4

desired_average = $40
average_loop_cycles = @(half (+ 4 2 2 3))
sure_delay = 12
reset_delay = @(+ average_loop_cycles sure_delay)
timer = @(- (* 8 audio_longest_pulse) 0)

tape_audio_player:
    sei         ; Disable interrupts.
    lda #$7f
    sta $911e
    sta $911d
    sta $912e
    sta $912d

    ; Initialize VIC.
    lda #17             ; (horizontal origin)
    sta $9000
    lda #50             ; (vertical origin)
    sta $9001
    lda #@(+ 128 16)    ; (16 screen rows)
    sta $9002
    lda #@(* 16 2);     ; (16 screen columns)
    sta $9003
    lda #$fd            ; (character set at $1000)
    sta $9005
    lda #8              : (black screen and border)
    sta $900f

    ; Set colors to white.
    ldx #0
    stx tleft
    stx average
    stx @(++ average)
    lda #white
l:  sta colors,x
    sta @(+ 203 colors),x
    dex
    bne -l

    ; Generate other half of luminance chars.
    ldx #63
    ldy #64
l:  lda luminances,x
    sta $1400,x
    eor #$ff
    sta $1400,y
    iny
    dex
    bpl -l

    ; Start tape motor.
    lda $911c
    and #$fd
    sta $911c

    ; Initialize VIA2 timer 1.
    lda #%00000000
    sta $912b
    lda #<timer
    sta current_low
    ldy #>timer
    sty $9125

play_audio:
    ; Wait for end of pulse.
    lda $9121   ; Reset the VIA2 CA1 status bit.
l:  lda $912d   ; (4) Read the VIA2 CA1 status bit.
    lsr         ; (2) Shift to test bit 2.
    lsr         ; (2)
    bcc -l      ; (2/3) Nothing happened yet. Try again…

    ; Get sample.
    ldx $9124   ; (4) Read the timer's low byte which is your sample.
    lda $9125   ; (4) Read the timer's high byte.
    sty $9125   ; (4) Write high byte to restart the timer and acknowledge interrupt.
    bmi framesync

    ; Downsample and play.
    lda downsamples,x
    sta $900e

    ; Make sum of samples.
    txa
    clc
    adc average
    sta average
    bcc +n
    inc @(++ average)
n:

    ; Check if sum is complete.
    dec tleft
    bne play_video      ; No, continue with video…

    ; Correct time if average pulse length doesn't match our desired value.
    lda @(++ average)   ; average / 256
    tax
    cmp #desired_average
    beq +j              ; It's already what we want…
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

play_video:
    ; Wait for end of pulse.
    lda $9121   ; Reset the VIA2 CA1 status bit.
l:  lda $912d   ; (4) Read the VIA2 CA1 status bit.
    lsr         ; (2) Shift to test bit 2.
    lsr         ; (2)
    bcc -l      ; (2/3) Nothing happened yet. Try again…

    ; Get sample.
    ldx $9124   ; (4) Read the timer's low byte which is your sample.
    lda $9125   ; (4) Read the timer's high byte.
    sty $9125   ; (4) Write high byte to restart the timer.
    bmi framesync
    lda downsamples,x
    tax

    ; Invert every second luminance value.
    lda @(++ +p)
    lsr
    bcc +p
    lda inversions,x
    tax

    ; Draw luminance pixel.
p:  stx $1e00       ; Save as luminance char.
    inc @(++ -p)    ; Step to next pixel.

    jmp play_audio  ; Back to audio…

    ; Start new frame.
framesync:
    lda #0
    sta @(++ -p)
    jmp play_audio

inversions:
    15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0 0

downsamples:
@(maptimes [integer (/ _ 16)] 256)
