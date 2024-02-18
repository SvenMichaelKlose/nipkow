# nipkow – datasette multimedia player

This a real–time multimedia tape player for the unexpanded Commodore
VIC-20.  It plays pulse–width modulated audio from tape which you may
stop, resume, rewind or fast-forward like a regular music cassette.
Its output is roughly four bits at 4-6kHz sampling rate.

To make tapes audio recording equipment is required.


# Download and build on Linux

You'll need at least git, sbcl, mplayer and sox installed.  Then download
'download-and-build.sh' and execute it.


# How it works

## Pulse width modulation

The datasette isn't suited for analog signals.  It can only detect if a
signal from datassette goes low.  Data is encoded by writing pulses of
different lengths that are measured on playback.  To record audio the
samples are pulse–width modulated. A sample with the value 0 is converted
to the shortest pulse, a sample of the maximum value of 15 is turned into
the longest pulse.  (The sample values are inverted on playback actually
but that really doesn't matter.)

The recorded pulses look like this:

           X cycles        | 128 cycles max.
    +----------------------+FEDCBA9876543210 <- sample values for each 8
    |                      |    variable    |    extra cycles
    | Minimum pulse width  |   additional   |
    | for sample value 0.  |     width      |
    +----------------------+----------------+
                                    +       
                                    | +     
                                    |  +    
                                    |   +   
                                    |   +   
                                    |  +    
                                    | +     
                                    |+      
                                    +  <- average sample value
                                   +|
                                  + |
                                 +  |
                                +   |
                                +   |
                                 +  |
                                  + |
                                   +|       

The minimum pulse width is required to a. make a pulse occur at all and b.
to make the signal get through the datassette's high pass filter.  That
filter is the reason why the sampling rate is limited to 6kHz.  Once could
increase it to 7kHz or more but the resulting distortions are very audible
and everything closing in to 8kHz is very likely to get filtered out.

## Staying in tune

One cannot assume that all tape drives have the same speed and we need some
way to adjust the minimum pulse length somehow. That's done by constantly
calculating the average pulse length to correct the reference timer in
small steps.  This is working out very well.  On arthritic, wobbly
datassettes the audio wobbles along in disturbing perfection.

## Playing video (UNFINISHED!)

Video resolution is 16x16 pixels with 16 grades of luminance.  Audio and
video samples are simply interleaved.  An extra long pulse tells the player
to reset the screen pointer and to start over with the following audio pulse.
