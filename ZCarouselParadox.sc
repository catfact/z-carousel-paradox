//--- ZCarouselParadox
//-- a performance-oriented audio effect
//-- based on slew-rate limited delay modulation
//-- with companding in the input / feedback loop

// top level class.
// handles connections to system audio and MIDI,
// basic group structure / node ordering,
// and owns the processor and control modules
ZCarouselParadox {
	var <server;
	var <processor;

	*new { arg server;
		^super.newCopyArgs(server).init
	}

	init {

	}
}

// a single "processor module", incorporating echo and companding
// defines the single large synthdef used for everythoing
ZCarouselProcessor  {

	// make the buffer long enough to act as a decent looper
	classvar bufferLength = 32.0;

	var <server;
	var <buffer;
	var <synth;
	var <bus;

	*sendSynthDefs { arg server;

		var def = SynthDef.new(\ZCarouselProcessor, {

			var buffer = \buffer.kr;
			var bufFrames = BufFrames.kr(buffer);
			var delayTime = Lag.ar(Slew.ar(K2A.ar(\delayTime.kr(1)),
				\delayTimeSlewUp.kr(1), \delayTimeSlewDown.kr(1)), \delayTimeLag.kr(1));

			// refinement: adding a `freeze` control
			// we can implement "freeze" by interpolating between input and previous buffer contents,
			// when writing to the buffer.
			// to do this we need to change 2 things:
			// - use discrete read/write ugens instead of `BufDelay`
			// - perform feedback within the synthdef
			// - (if it's from a bus, the extra latency will phase with previous buffer contents)
			//
			// additionally: when we disable the write head, we'll hear buffer contents on a loop,
			// and this loop length is separate from the delay time!
			// (naively, the loop length is equal to the entire buffer length)
			//
			// so we'll add an additional `loopTime` parameter,
			// and wrap both Rd and Wr phases to the corresponding frame count.
			// this comes with caveats:
			// - modulating loop time will cause clicks!
			//   (sorry, a clickless, modulateable looper needs special attention)
			// - i couldn't figure out a way to initialize loop time control to the buffer duration,
			//   so it needs to be set explicitly at synhth creation if that is desired.
			// - when write head is *not disabled*, the loop still applies;
			//   making it shorter than the delay time will cause Weird things to happen
			var loopFrames = \loopTime.kr * SampleRate.ir;
			var phaseWr = (Phasor.ar(rate:1, end: bufFrames)).wrap(0, loopFrames-1);
			var phaseOffset = (delayTime * SampleRate.ir).min(bufFrames-1);
			var phaseRd = (phaseWr + bufFrames - phaseOffset).wrap(0, loopFrames-1);
			var previous = BufRd.ar(2, buffer, phaseWr);
			var output = BufRd.ar(2, buffer, phaseRd);

			// "freeze" interpolation signal
			var freeze = Lag.ar(K2A.ar(\freeze.kr(0)), \freezeLag.kr(0.2));
			//var freeze = 0;

			 var feedback = output *  \feedbackLevel.kr(0);

			// sum input with feedback
			var input = In.ar(\in.kr, 2) + feedback;

			// further processing applies to both the first-pass input and the feedback
			// exercise: play with the routing of these processing steps

			// highpass and lowpass filtering
			input = LPF.ar(HPF.ar(input, \feedbackHpf.kr(20)), \feedbackLpf.kr(18000));

			// stereo image processing
			input = ZCarouselStereoImage.midSideFlip(input,
				\stereoMidGain.kr(1), \stereoSideGain.kr(1), \stereoBias.kr(0), \stereoFlip.kr(0));

			// companding
			input = ZCarouselCompander.compandStereo(input,
				// control rate output busses for envelope and gain:
				\outEnv.kr, \outGain.kr,
				// compander parameters:
				\thresholdCompress.kr(-12), \thresholdExpand.kr(-36),
				\slopeAbove.kr(1), \slopeBelow.kr(1),
				// input envelope smoothing:
				\compEnvAttack.kr(0.01), \compEnvRelease.kr(0.1),
				// output gain smoothing:
				\compGainAttack.kr(0.01), \compGainRelease.kr(0.1));

			// mix with previous buffer contents
			input = [
				SelectX.ar(freeze, [input[0], previous[0]]),
				SelectX.ar(freeze, [input[1], previous[1]]),
			];

			// write the buffer
			BufWr.ar(input, buffer, phaseWr);

			// and finally, output the delayed signal
			Out.ar(\out.kr(0), output * \level.kr(1));

		});
		// post all our control names for reference
		def.allControlNames.do({ arg control; control.postln; });
		def.send(server);
	}

	*new { arg server;
		^super.newCopyArgs.init;
	}

	init {
		buffer = Buffer.alloc(server, server.sampleRate * bufferLength, 2);
		synth = Synth.new(\ZCarouselProcessor, [
			\
		]);
	}

	setNoteInterval { arg interval;
		// TODO
	}

	setEchoTime { arg time;
		synth.set(\delayTime, time);
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
		lastBaseNote = nil;
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
				tapStartTime = now;
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

///---- additional DSP classes
//-- _because reasons_, it turns out to be better to roll all the DSP into a single synthdef
//-- but it still seems helpful to organize some peripheral functions away from the main processor

// functions for stereo imaging
ZCarouselStereoImage {
	classvar scale = 0.7071067811865475; // == 1 / sqrt(2)

	*midSideFlip {
		arg in, // assumed stereo input
		midGain=1, sideGain=1, // separate mid/side levels
		bias=0,     // L/R bias in [-1, 1] - applied first
		flip=0;     // inversion mix (in [0, 1]) - applied last
		var mid, side, l, r, input, output;
		// fixme: nicer pan law would be nicer
		input = [in[0] * (1-bias), in[1] * (1+bias)];
		mid = (input[0] + input[1]) * scale;
		side = (input[0] - input[1]) * scale;
		mid = mid * midGain;
		side = side * sideGain;
		l = (mid+side) * scale;
		r = (mid-side) * scale;
		output = [
			SelectX.ar(flip, [l, r]),
			SelectX.ar(flip, [r, l]),
		];
		^output;
	}
}

// functions for companding
ZCarouselCompander {

	// compute compander input envelope using windowed average power
	*compEnvRms {
		arg input, attack, release, windowSize=0.02;
		var windowFrames = windowSize * SampleRate.ir;
		// NB: expensive!
		var env = (RunningSum.ar(input.squared, windowFrames)/windowFrames).sqrt;
		^LagUD.ar(env, attack, release);
	}

	// compute compander input envelope using decaying peak amplitude
	// `PeakFollower` is actually not a windowed peak follower like i thought!
	// it is in fact just like `LagUD(0, c)`, but computed differently (maybe faster)
	// i have not found a good way of computing a windowed maximum
	// (`RunningMax` doesn't work the same way as `RunningSum`)
	// so that would be a good candidate for custom ugen
	// NB: decay time is fixed at synthdef creation
	*compEnvPeakDecay {
		arg input, attack, release, decayTime=0.02;
		// expensive, but only happens once (.ir)
		var decay = (-90.dbamp) ** (1/(decayTime*SampleRate.ir));
		var envelope = PeakFollower.ar(input, decay);
		//decay.poll;
		envelope = LagUD.ar(envelope, attack, release);
		^envelope;
	}


	// compute compander gain with hard knee
	*compGainHardKnee {
		arg env, threshDb, slopeAbove, slopeBelow, attack, release;
		var envDb = env.ampdb;
		var deltaDb = envDb - threshDb;
		var newTargetDb = threshDb + (deltaDb * if(envDb > threshDb, slopeAbove, slopeBelow));
		var gainDb = newTargetDb - envDb;
		//envDb.poll;
		gainDb = LagUD.ar(gainDb.dbamp, release, attack);
		^gainDb
	}

	// TODO: soft knee option

	// put companding functions together
	*compandStereo {
		arg input, // assume input is 2-channel array
		outEnv,
		outGain,
		/// refinement:
		// provide separate thresholds for comp and expand
		// this is best done here rather than making separate compander blocks,
		// because this way we can re-use the (expensive) input envelope calculation
		thresholdCompress = -12,
		thresholdExpand = -36,
		slopeAbove, slopeBelow,
		envAttack=0.01, envRelease=0.1,
		gainAttack=0.01, gainRelease=0.02;

		var inputEnvelope = max(
			ZCarouselCompander.compEnvPeakDecay(input[0], envAttack, envRelease),
			ZCarouselCompander.compEnvPeakDecay(input[1], envAttack, envRelease),
		);
		var gainCompress = ZCarouselCompander.compGainHardKnee(inputEnvelope,
			thresholdCompress, slopeAbove, 1,
			gainAttack, gainRelease);
		var gainExpand = ZCarouselCompander.compGainHardKnee(inputEnvelope,
			thresholdCompress, 1, slopeBelow,
			gainAttack, gainRelease);
		var totalGain = gainCompress * gainExpand;

		Out.kr(outEnv, A2K.kr(inputEnvelope));
		Out.kr(outGain, A2K.kr(totalGain));

		^[input[0] * totalGain, input[1] * totalGain]
	}

}