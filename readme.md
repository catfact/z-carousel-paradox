# z-carousel-paradox

this repo contains a relatively complete and playable software instrument based on sketches developed during iteration 1 of the `z-elements` workshop. it consists of a number of classes contained in `ZCarouselParadox.sc`, and usage/configuration stuff in `startup.scd`.

## basic concept

in essence, the effect is very simple: a stereo delay line with feedback. the main distinguishing feature is how delay time modulation is handled, which is by smoothing (as in a physical tape or bucket brigade.) 

the delay time is smoothed twice: once with a nonlinear integrator, and once with slew rate limiting. the slew rate effectively allows deliberately pitched artifacts to emerge. these pitch ratios can be set separately for lengthening and shortening the delay time.

when delay times are short, this feels and sounds a bit like a normal BBD style delay with additional constraints on pitch. however, this delay line is also very long - 32s as written, but this is basically arbitrary and limited only by RAM. when moving between a very short and a very long delay, with pitch restraints, that gesture can take a long time to complete! for example: with pitch artifacts constrained to an octave, contracting from 10 seconds to 1 seconds of delay takes 9 seconds of real time! in the interim, various stored audio material is passed over by the read head, pitched 1 octave up. (timing is additionally distorted by the relatively slight amount of nonlinear integrator-style smoothing.)

i am calling this thing _Carousel Paradox_, for no particularly great reason.


## control scheme

in the workshop, we quickly mapped held MIDI note intervals to the pitch constraints of the delay modulation, and also played with tap tempo. this seemed pretty interesting and worth exploring more. i've added some behavior mode flags and pedal controls, so the full present idea is something like this:

- the first note pressed in a chord sets the start of a tempo tap. this is the _base note_.
- each time an additional note is pressed, it sets the delay time, computed as the **time delta between the base note and the current note**.
- pressing a non-base note also sets the _delay slew ratio_ for contracting (negative slope) and/or expanding (positive slope). there are several modes for this:
  - `\symmetricPitchUp`: always pitch up by the interval amount, whether extending or contracting (reversed when extending)
  - `\symmetricPitchUpDown` (default): pitch up for contracting; pitch down or up for extending depending on the sign of the interval
  - `\signedPitchUp`: always pitch up, but apply to extending/contracting depending on interval sign

additionally, MIDI sustain and mute pedals apply other modes:

- with **sustain** held, playing a single note sets the interval compared to the last base note. if **not muting**, this also taps the tempo

- with **mute** held, only interval is set, tap tempo is not used

## other changes

workshop participants may note some changes to the bus architecture and sclang class architecture. 
there are no big reasons for any of this except that it's a little simplified, and things have been repackaged from a "prototype" environment to a "product" environment. in some kinds of practice it would make sense to keep reusing the classes from the workshop. here, i wanted to make everything pretty much self-contained.


## extras

- implemented a "freeze" mode that disables the write head. this transforms the processor into a looper, but that still undergoes modulation and pitch changes on the stored audio. it's kind of a wonky looper because the loop time is **not settable** (ha!) after initialization time. i guess imagine this as a physical tape system where you are moving the play head, can disable the erase head, and have a processed signal path between read and write heads.

- added some stereo processing to the input+feedback path: L/R inversion, L/R bias, mid/side balance

- added highpass and lowpass filtering to the input and feedback path

- added some rather quick and arbitrary distortion / softclipping to the output and feedback path

- added an extremely basic and minimal UI that shows mapped parameter values

- added a delay time control that is accumulative in the log(2) domain. effectively this means the tapped tempo acts as a "base time" and the "time knob" changes it in fractions of a golden ratio.

- added separate compress / expand thresholds and gain smoothing

## caveats / TODO

- there are quite possibly bugs in the control mechanism and pedal handling, i've mostly played without the pedals (and maybe the design can be made more useful there)

- the looping mechanism is sort of a half-baked experiment. it would be more effective to simply put a dedicated looping module in series after the effect... or possibly to set feedback gain to unity and bypass all other live input and feedback processing. (the problem there is that delay modulation artifacts will still be written to the buffer... hm.)

- build out the other UI a tiny bit more. (e.g. i'd like to see final parameter values in real units, in addition to MIDI input values.)

- maybe some state / preset management besides hardcoding things in the startup script
