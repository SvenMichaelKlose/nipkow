# Overview

This a real–time multimedia tape player for the unexpanded Commodore
VIC-20.  It plays pulse–width modulated audio from tape which you may
stop, resume, rewind or fast-forward like a regular music cassette.

The output is roughly four bits as ~4kHz sampling rate.

To record the TAPs on real cassettes you should use a real machine that
writes the tape or the signal will be degraded.  Software to do this
isn't avaible from me, yet.


# Download and build on Linux

You'll need at least git and sbcl installed.  Then download
'download-and-build.sh' and execute it.


# How it works

This is taken from a posting in the VIC-20 Denial forum at
http://sleepingelephant.com/denial/

The two VIAs have two timers each that count down at the speed of the CPU
clock.  Timer 2 in each chip has the advantage that it has "latches".  If
you write the low byte of the timer, it goes into the latch without
affecting the timer.  As soon as you write the high byte the timer is set
to the written values, counting down.  If you set the timer to 256 on a NTSC
VIC it'll count down to 0 in 1027270 / 256 = 1/4013s.
Fortunately, the timers can also be read.

The VIA can tell you if the signal from tape went low – the end of a
pulse; that's that status bit.  It has to be reset manually.

Now, that's all we need.  The recorded pulses look like this:


          192 cycles       | 128 cycles max.
    ------------------------0123456789ABCDEF <- sample values for each 8
    |                      |    variable   |    extra cycles
    | minimum pulse width  |   additional  |
    |                      |     width     |
    Longest pulse is 310 cycles. ---------->
    Average is 256 cycles ---------->
                                    +       
                                    | +     
                                    |  +    
                                    |   +   
                                    |   +   
                                    |  +    
                                    | +     
                                    |+      
                                    +  <- our audio wave
                                   +|       
                                  + |       
                                 +  |       
                                +   |       
                                +   |       
                                 +  |       
                                  + |       
                                   +|       

We need the minimum pulse width to actually output the samples and to
do the bookkeeping.  More about that later on.

Now for the actual playing. We set the timer to the longest pulse length of
210.  When the minimum pulse width has passed the timer counted down to
128.  When the pulse stops a little bit later we get some value from 120
to 127.  That shifted three bits to the right gives us the sample value 15.
When the status bit signals that the pulse ended we just read the low byte
of the timer, immediately reset the timer by writing its high byte, then we
shift the value we've just read and output it.

That's where the trouble starts.  One cannot assume that all tape drives
have the same speed and we need some way to adjust the minimum pulse length
somehow. If we calculate the average value of all samples, we get the sample
value that lies on the center of our audio wave. The timer should've counted
down to 64 if the timing was perfect.  (Sure as hell that's only the case in
VICE.)  If the average timer value is above our desired value, we decrement
the longest pulse width or we increment it or we leave it alone when it's
right. BUT you have to take the time into account that is not measured
from the point when the status bit isn't yet detected (average loop cycles)
and the sure time not measured until the timer is restarted. Maybe that's
why the desired average is 41, not 64.

How the average is calculated isn't very obvious since the player isn't
initialized properly. Since it's self-adjusting it simply doesn't have to
be.  It sums up the last 256 samples in chunks of 128 to let the former
averages flow in, so the change would be gradual.  I've no bloody clue
about mathematics.  It just hit me on the throne and it works.  Might also
a flaw in this algorithm that skewed the desired average sample.

I'm sure that everything will be explained some day.