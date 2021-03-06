; nipkow – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

;current_low = 0
;average = 1
;tleft = 3
;tmp = 4

restart_delay = @(+ (half (+ 4 3 3)) 8)
timer = @(- (* 8 (nipkow-longest-pulse)) restart_delay)

tape_audio_player:
    ; Disable interrupts.
    sei
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
    lda #$fd            ; (character set at $1400)
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
l:  lda luminances,x
    sta $1400,x
    iny
    dex
    bpl -l
    ldx #63
    ldy #64
l:  lda luminances,x
    eor #$ff
    sta tmp
    lda #0
    lsr tmp
    rol
    lsr tmp
    rol
    lsr tmp
    rol
    lsr tmp
    rol
    lsr tmp
    rol
    lsr tmp
    rol
    lsr tmp
    rol
    lsr tmp
    rol
    sta $1400,y
    iny
    dex
    bpl -l

    ldx #15
l:  txa
    sta screen,x
    lda #15
    sec
    sbc screen,x
    sta @(+ 16 screen),x
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
    lda $9121       ; Reset the VIA2 CA1 status bit.
l:  lda $912d       ; Read the VIA2 CA1 status bit.
    lsr             ; Shift to test bit 2.
    lsr
    bcc -l          ; Nothing happened yet. Try again…

    ; Get sample.
    ldx $9124       ; Read the timer's low byte which is your sample.
    lda $9125       ; Read the timer's high byte.
    sty $9125       ; Restart timer and acknowledge interrupt.
    bmi framesync_a
no_framesync_a:

    ; Downsample and play.
    lda downsamples,x
    sta $900e

    ; Make sum of samples.
    txa             ; Add X to word average.
    clc
    adc average
    sta average
    bcc +n
    inc @(++ average)
n:

    ; Check if sum is complete.
    dec tleft
    bne play_video      ; No, continue with video…

    ; Adjust timer if average pulse length isn't centered.
    lda @(++ average)   ; average / 256
    cmp #$3f
    beq +j              ; It's already what we want…
    bcc +n
    dec current_low
    bne +d
n:  inc current_low
d:  lda current_low
    sta $9124

j:  lda #0
    sta average
    sta @(++ average)

play_video:
    ; Wait for end of pulse.
    lda $9121       ; Reset the VIA2 CA1 status bit.
l:  lda $912d       ; Read the VIA2 CA1 status bit.
    lsr             ; Shift to test bit 2.
    lsr
    bcc -l          ; Nothing happened yet. Try again…

    ; Get sample.
    ldx $9124       ; Read the timer's low byte which is your sample.
    lda $9125       ; Read the timer's high byte.
    sty $9125       ; Restart timer and acknowledge interrupt.
    bmi framesync_b
no_framesync_b:
    lda downsamples,x
    tax

    ; Invert every second luminance value.
    lda @(++ +p)
    lsr
    bcs +p
    lda inversions,x
    tax

    ; Draw luminance pixel.
p:  stx $1e00       ; Save as luminance char.
    inc @(++ -p)    ; Step to next pixel.

    jmp play_audio

framesync_a:
    cpx #$f0
    bcc no_framesync_a
    bcs framesync

framesync_b:
    cpx #$f0
    bcc no_framesync_b

    ; Start new frame.
framesync:
    lda #0
    sta @(++ -p)
    jmp play_audio

downsamples:    @(maptimes [integer (/ _ 8)] 128)
                @(maptimes [identity 15] 128)
inversions:     15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
