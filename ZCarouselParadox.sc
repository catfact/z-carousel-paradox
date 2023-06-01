    // an instrument based on slew-rate limited delay modulation,
    // with companding in the feedback loop
    ZCarouselParadox { 
        var <server;
        var <processor;

    }

    // a single "processor module", incorporating echo and companding
    ZCarouselProcessor  {
        setNoteInterval { arg interval;
        // TODO
        }

        setEchoTime { arg time;
        // TODO
        }
    }

    // MIDI control structure designed to connect to a single processor module
    ZCarouselMidiControls { 
        var <processor;
        var <pedalState;
        var <tapStartTime;
        var <lastBaseNote;
        var <numHeldNotes;

        *new { arg processor;
            ^super.newCopyArgs(processor).init
        }

        init {
            pedalState = Dictionary.with(
                \sustain, false,
                \mute, false
            );
            lastHeldNote = nil;
            numHeldNotes = 0;
        }

        // the central control gesture of the instrument uses a MIDI keyboard (or equivalent.)
        //
        // playing an interval on the keyboard simultaneously sets:
        // - the slew rate of the delay modulation, tuning it to the interval played
        // - the echo time, according to the time between the first and second noteons.
        // 
        // pedal modifications:
        // - with the mute pedal engaged, tap tempo will be ignored
        // - with the sustain pedal engaged, playing any single note will set the interval according to the last stored base note.
        noteOn { arg num;

            // i think it would be nice, while sustaining and not muting, to allow tapping tempo with a single note
            if (pedalState[\sustain], {
                setNoteInterval(lastBaseNote, num);
                if (pedalState[\mute].not, {
                    var now = SystemClock.seconds;
                    if (tapStartTime.notNil, {
                        setTimeDelta(now - tapStartTime);
                    });
                    tapStartTime = now;q
                });
            }, {
                // not sustaining, so what we do depends on held notes count
                if (numHeldNotes < 1, { 
                    lastBaseNote = num;
                     if (pedalState[\mute].not, {
                        tapStartTime = SystemClock.seconds;
                     });
                }, {
                    if (pedalState[\mute].not, {
                        // not muted, so set the tap tempo
                        if (tapStartTime.notNil, {
                            setTimeDelta(SystemClock.seconds - tapStartTime);
                        });
                    });
                    // set the interval
                    setNoteInterval(lastBaseNote, num);
                });
            });
            numHeldNotes = numHeldNotes + 1;
        }

        noteOff { arg num; 
            numHeldNotes = numHeldNotes - 1;
        }

        setNoteInterval { arg baseNote, newNote;
            processor.setNoteInterval(newNote - baseNote);
        }

        setTimeDelta { arg delta;
            processor.setEchoTime(delta);
        }
    }

    ZCarouselEcho { 

    }

    ZCarouselCompander { 

    }