
//------------------------------------------------
//-- ZCarouselParadox
//-- a performance-oriented audio effect
//-- based on slew-rate limited delay modulation
//-- with companding in the input / feedback loop

// top level class.
// handles connections to system audio and MIDI,
// basic group structure / node ordering,
// and owns the processor and control modules
//
// (same as to `ZAudioContext` prototyping environment)
ZCarouselParadox {
	classvar <patchLevelLagTime = 0.05;

	var <server;
	var <hwInNumChannels;
	var <hwOutNumChannels;
	var <hwInChannelOffset;
	var <hwOutChannelOffset;

	var <bus;
	var <patch;
	var <group;

	var <processor;
	var <midiController;
	var <midiInput;


	*sendSynthDefs { arg server;

		// main processor synthdefs
		ZCarouselParadox_Processor.sendSynthDefs(server);

		// utility synthdefs
		SynthDef.new(\patch_mono,{
			Out.ar(\out.kr(0), In.ar(\in.kr, 1) * \level.kr(1, \levelLag.kr(patchLevelLagTime)));
		}).send(server);

		SynthDef.new(\patch_stereo,{
			var levelLag = \levelLag.kr(patchLevelLagTime);
			Out.ar(\out.kr,
				In.ar(\in.kr, 2) * \level.kr(1, levelLag));
		}).send(server);

		SynthDef.new(\patch_pan,{
			var snd = In.ar(\in.kr, 1) * \level.kr(1, \levelLag.kr(patchLevelLagTime));
			Out.ar(\out.kr, Pan2.ar(snd, \pan.kr(0, \panLag.kr(patchLevelLagTime))));
		}).send(server);

		SynthDef.new(\adc_mono, {
			Out.ar(\out.kr, SoundIn.ar(\in.kr(0)) * \level.kr(1, \levelLag.kr(patchLevelLagTime)));
		}).send(server);

		SynthDef.new(\adc_stereo, {
			var levelLag = \levelLag.kr(patchLevelLagTime);
			var in = \in.kr(0);
			Out.ar(\out.kr, SoundIn.ar([in, in+1]) * \level.kr(1, levelLag));
		}).send(server);

		SynthDef.new(\adc_pan, {
			var snd = SoundIn.ar(\in.kr(0), 1) * \level.kr(1, \levelLag.kr(patchLevelLagTime));
			Out.ar(\out.kr, Pan2.ar(snd, \pan.kr(0, \panLag.kr(patchLevelLagTime))));
		}).send(server);

	}

	*new { arg server,
		hwInNumChannels=1,
		hwOutNumChannels=2,
		hwInChannelOffset=0,
		hwOutChannelOffset=0;

		^super.newCopyArgs(server, hwInNumChannels, hwOutNumChannels, hwInChannelOffset, hwOutChannelOffset).init;
	}

	init {
		//-----------------------------------
		//--- generic stuff
		/// groups
		group = Dictionary.new;
		// group to hold (internal) input patches
		group[\in] = Group.new(server);
		// group to hold processing synths
		group[\process] = Group.after(group[\in]);
		// group to hold (internal) output patches
		group[\out] = Group.after(group[\out]);

		/// busses
		bus = Dictionary.new;
		bus[\hw_in] = Bus.audio(server, 2);
		bus[\hw_out] = Bus.audio(server, 2);

		postln("hwInNumChannels: " ++ hwInNumChannels);

		/// I/O patch synths
		patch = Dictionary.new;
		patch[\hw_in] = if (hwInNumChannels > 1, {
			postln("patching stereo HW input to main input bus");
			Synth.new(\adc_stereo, [
				\in, hwInChannelOffset, \out, bus[\hw_in].index,
			], group[\in], \addBefore);
		}, {
			postln("panning mono HW input to main input bus");
			Synth.new(\adc_pan, [
				\in, hwInChannelOffset, \out, bus[\hw_in].index
			], group[\in], \addBefore);
		});

		patch[\hw_out] = Synth.new(\patch_stereo, [
			\in, bus[\hw_out].index, \out, hwOutChannelOffset
		], group[\out], \addAfter);


		//-----------------------------------
		//--- specific stuff
		processor = ZCarouselParadox_Processor.new(this);
		midiController = ZCarouselParadox_MidiController.new(processor);

		// NB: the generic MIDI input wrapper could just as well be left outside this class
		// (probably better/ more flexible in fact)
		// but here i'm including it to keep things more self-contained
		midiInput = ZCarouselParadox_MidiInput.new(nil, nil, true);

		midiInput.noteon ({
			arg num, vel;
			midiController.noteOn(num);
		});

		midiInput.noteoff ({
			arg num, vel;
			midiController.noteOff(num);
		});

		// sustain pedal
		midiInput.cc(64, {
			arg val;
		});

		// mute pedal
		midiInput.cc(67, {
			arg val;
		});
	}

	setSynthParam { arg key, value;
		this.processor.synth.set(key, value);
	}
}

//------------------------------------------------------------
// ZCarouselProcessor: top-level effect module
// defines the single large synthdef used for echo + companding
ZCarouselParadox_Processor  {
	classvar <>bufferLength = 32.0;

	var <context; // a `ZCarouselParadox`
	var <server;
	var <buffer;
	var <synth;
	var <bus;
	var <patch;
	var <echoTime;

	// `intervalMode` allowed values:
	// \symmetricPitchUp
	//  - always pitch up by the interval amount, whether extending or contracting
	//
	// \symmetricPitchUpDown
	//  - negative interal will be applied in reverse
	//
	// \signedPitchUp
	//  - always pitch up, for contracting/extending depending on interval sign

	var <intervalMode;

	// user-settable function taking a note interval and returning a frequency ratio
	var <>tuningFunction;

	*sendSynthDefs { arg server;

		var def = SynthDef.new(\ZCarouselProcessor, {

			var buffer = \buffer.kr;
			var bufFrames = BufFrames.kr(buffer);
			var delayTime = Lag.ar(Slew.ar(K2A.ar(\delayTime.kr(1)),
				\delayTimeSlewUp.kr(1), \delayTimeSlewDown.kr(1)), \delayTimeLag.kr(0.02));

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
			// in fact it is equal to the entire buffer length
			// (seems possible to change this, but first attempt had artifacts)
			var phaseOffset = (delayTime * SampleRate.ir).min(bufFrames-1);

			//  write-head phasor
			var phaseWr = (Phasor.ar(rate:1, end: bufFrames));
			// read head follows behind; offset to keep from going negative
			var phaseRd = (phaseWr - phaseOffset + bufFrames);
			// to fake a disabled write head, use a read head with the same phasor
			var previous = BufRd.ar(2, buffer, phaseWr);
			// get the output early in the graph for feedback processing
			var output = BufRd.ar(2, buffer, phaseRd);
			// "freeze" interpolation signal
			var freeze = Lag.ar(K2A.ar(\freeze.kr(0)), \freezeLag.kr(0.2));

			// var feedCompMix = \feedbackCompandMix.kr(1);
			// var inCompMix = \inputCompandMix.kr(1);

			var input, feedback, outputDist, compDryBus, compWetBus;

			/// NB / FIXME: this is highly inefficient!!!
			/// all the distortion types are computed in parallel, and they are quite expensive
			outputDist = Array.fill(2, { arg i;
				SelectX.ar(\distortShape.kr(0), [
					output[i].softclip,
					output[i].tanh,
					output[i].distort.distort,
					output[i].distort.distort.distort.distort,
					sin(output[i]*4)
				])
			});

			output = Array.fill(2, { arg i;
				SelectX.ar(\distortAmount.kr(0), [
					output[i], outputDist[i]
				])
			});

			//--- highpass and lowpass filtering
			output = LPF.ar(HPF.ar(output, \hpf.kr(20)), \lpf.kr(18000));

			feedback = output *  \feedbackLevel.kr(0);

			input = In.ar(\in.kr, 2) ;

			// further processing applies to both the first-pass input and the feedback
			// try: play with the routing of these processing steps

			//--- stereo image processing
			// NB / FIXME?: maybe separate settings for input / feedback?
			// if they have same settings, the duplication is weird,
			// but can't see a way around it, given bus structure  needed by FB compand mix
			input = ZCarouselParadox_StereoImage.midSideFlip(input,
				\inputStereoMidGain.kr(1), \inputStereoSideGain.kr(1),
				\inputStereoBias.kr(0), \inputStereoFlip.kr(0));

			feedback = ZCarouselParadox_StereoImage.midSideFlip(feedback,
				\feedbackStereoMidGain.kr(1), \feedbackStereoSideGain.kr(1),
				\feedbackStereoBias.kr(0), \feedbackStereoFlip.kr(0));

			//--- companding

			input = ZCarouselParadox_Compander.compandStereo(input,
				\outInputEnv.kr, \outInputGain.kr,
				\inputThresholdCompress.kr(-12), \inputThresholdExpand.kr(-36),
				\inputSlopeAbove.kr(1), \inputSlopeBelow.kr(1),
				\inputCompEnvAttack.kr(0.01), \inputCompEnvRelease.kr(0.1),
				\inputGainUpLagCompress.kr(0.01), \inputGainDownLagCompress.kr(0.1),
				\inputGainLagExpand.kr(0.01), \inputGainDownLagExpand.kr(0.1)
			);

			feedback = ZCarouselParadox_Compander.compandStereo(feedback,
				\outFeedbackEnv.kr, \outFeedbackGain.kr,
				\feedbackThresholdCompress.kr(0), \feedbackThresholdExpand.kr(-60),
				\feedbackSlopeAbove.kr(1), \feedbackSlopeBelow.kr(1),
				\feedbackCompEnvAttack.kr(0.01), \feedbackCompEnvRelease.kr(0.1),
				\feedbackGainUpLagCompress.kr(0.01), \feedbackGainDownLagCompress.kr(0.1),
				\feedbackGainUpLagExpand.kr(0.01), \feedbackGainDownLagExpand.kr(0.1)
			);

			feedback = Sanitize.ar(feedback);

			// mix input with processed feedback
			input = input + feedback;

			// mix input with previous contents
			input = [
				SelectX.ar(freeze, [input[0], previous[0]]),
				SelectX.ar(freeze, [input[1], previous[1]]),
			];

			// write to the buffer
			BufWr.ar(input, buffer, phaseWr);

			// output the delayed signal
			Out.ar(\out.kr(0), output * \level.kr(1));

			// output the smoothed delay time at control rate
			Out.kr(\outDelay.kr, A2K.kr(delayTime));

		});
		// post all our control names for reference
		def.allControlNames.do({ arg control; control.postln; });
		def.send(server);
	}

	*new { arg context;
		^super.newCopyArgs(context).init;
	}

	init {

		server = context.server;
		buffer = Buffer.alloc(server, server.sampleRate * bufferLength, 2);
		bus = Dictionary.newFrom([
			\inputCompEnv, Bus.control(server, 1),
			\inputCompGain, Bus.control(server, 1),
			\feedbackCompEnv, Bus.control(server, 1),
			\feedbackCompGain, Bus.control(server, 1),
			\delayTime, Bus.control(server, 1);
		]);

		// NB: assumes that this is constructed in a Thread/Routine!
		server.sync;

		synth = Synth.new(\ZCarouselProcessor, [
			\buffer, buffer,
			// NB: here's one place to change for different bus structure
			\in, context.bus[\hw_in],
			\out, context.bus[\hw_out],
			\outInputEnv, bus[\inputCompEnv],
			\outInputGain, bus[\inputCompGain],
			\outFeedbackEnv, bus[\feedbackCompEnv],
			\outFeedbackGain, bus[\feedbackCompGain],
			\outDelay, bus[\delayTime]
		], context.group[\process]);

		echoTime = 1.0;

		tuningFunction = { arg interval;
			interval.midiratio
		};

		intervalMode = \symmetricPitchUpDown;

	}

	setNoteInterval { arg interval;
		// postln("set interval: " ++ interval);
		if (interval == 0, {
			// do nothing
		}, {
			switch(intervalMode,
				{\symmetricPitchUp}, {
					var ratio = tuningFunction.value(interval.abs);
					// postln("symmetric upward ratio = " ++ ratio);
					synth.set(\delayTimeSlewUp, ratio + 1);
					synth.set(\delayTimeSlewDown, ratio - 1);
				},
				{\symmetricPitchUpDown}, {
					var ratio = tuningFunction.value(interval);

					postln("symmetric bipolar ratio = " ++ ratio);
					if (ratio > 1, {
						// postln("raising");
						synth.set(\delayTimeSlewUp, ratio + 1);
						synth.set(\delayTimeSlewDown, ratio - 1);
					}, {
						// postln("lowering");
						synth.set(\delayTimeSlewUp, ratio + 1);
						// unfortunately
						synth.set(\delayTimeSlewDown, ratio);
					});
				},
				{\signedPitchUp}, {
					if (interval > 0, {
						synth.set(\delayTimeSlewDown, tuningFunction.value(interval) - 1);
					}, {
						synth.set(\delayTimeSlewUp, tuningFunction.value(interval.neg) + 1);
					});
			});
		});

		//----====----====----====----====----====----====
		// exercise: other behaviors are possible here.
		// for example, slew intervals could yield downward pitch adjustments...
	}

	setEchoTime { arg time;
		echoTime = time.min(bufferLength - 0.001);
		synth.set(\delayTime, echoTime);
		^echoTime
	}
}

//------------------------------------------------------------
// ZCarouselMidiInput: MIDI I/O glue class
// (same as `ZSimpleMidiControl` from prototyping environment)
ZCarouselParadox_MidiInput {
	var <port, <dev;
	var <ccFunc; // array of functions
	var <noteOnFunc;
	var <noteOffFunc;

	var <>verbose = false;
	var <>channel = nil;

	*new { arg deviceName=nil, portName=nil, connectAll=false;
		^super.new.init(deviceName, portName, connectAll);
	}

	init {
		arg deviceName, portName, connectAll;
		MIDIClient.init;

		if (connectAll, {
			postln("connect all MIDI");
			MIDIIn.connectAll;
		}, {
			var endpoint = MIDIIn.findPort(deviceName, portName);
			if (endpoint.isNil, {
				postln ("ERROR: couldn't open device and port: " ++ deviceName ++ ", " ++ portName);
			});
		});

		ccFunc = Array.newClear(128);

		MIDIIn.addFuncTo(\control, { arg uid, chan, num, val;
			if (channel.isNil || (chan == channel), {
				if (verbose, { [uid, chan, num, val].postln; });
				if (ccFunc[num].notNil, {
					ccFunc[num].value(val);
				});
			});
		});

		MIDIIn.addFuncTo(\noteOn, { arg uid, chan, num, vel;
			if (channel.isNil || (chan == channel), {
				if (verbose, { [uid, chan, num, vel].postln; });
				if (noteOnFunc.notNil, { noteOnFunc.value(num, vel); });
			});
		});


		MIDIIn.addFuncTo(\noteOff, { arg uid, chan, num, vel;
			if (channel.isNil || (chan == channel), {
				if (verbose, { [uid, chan, num, vel].postln; });
				if (noteOffFunc.notNil, { noteOffFunc.value(num, vel); });
			});
		});
	}

	// set a handler for a given CC numnber
	cc { arg num, func;
		ccFunc[num] = func;
	}

	noteon { arg func;
		noteOnFunc = func;
	}

	noteoff { arg func;
		noteOffFunc = func;
	}
}

//---------------------------------------------------------------------------------------
// ZCarouselMidiController: class implementing MIDI control logic for a single processsor

ZCarouselParadox_MidiController {
	// specific logic
	var <processor;
	var <pedalState;
	var <tapStartTime;
	var <lastBaseNote;
	var <numHeldNotes;
	var <timeDelta;
	var <>tapCallback;


	*new { arg processor;
		^super.newCopyArgs(processor).init
	}

	init {
		pedalState = Dictionary.newFrom([
			\sustain, false,
			\mute, false
		]);
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

		numHeldNotes.postln;

		// i think it would be nice, while sustaining and not muting,
		// to allow tapping tempo with a single note
		if (pedalState[\sustain], {
			this.setNoteInterval(lastBaseNote, num);
			if (pedalState[\mute].not, {
				var now = SystemClock.seconds;
				if (tapStartTime.notNil, {
					this.setTimeDelta(now - tapStartTime);
				});
				tapStartTime = now;
			});
		}, {
			// not sustaining, so what we do depends on held notes count
			if (numHeldNotes < 1, {
				postln("not sustaining, first note");
				lastBaseNote = num;
				if (pedalState[\mute].not, {
					postln("not muted, setting tap start");
					tapStartTime = SystemClock.seconds;
				});
			}, {
				// set the interval
				this.setNoteInterval(lastBaseNote, num);
				if (pedalState[\mute].not, {
					// not muted, so set the tap tempo
					if (tapStartTime.notNil, {
						this.setTimeDelta(SystemClock.seconds - tapStartTime);
					});
				});
			});
		});
		numHeldNotes = numHeldNotes + 1;
	}

	noteOff { arg num;
		numHeldNotes = numHeldNotes - 1;
		// if the count goes negative its probably (e.g.) a missed noteon from startup
		numHeldNotes = numHeldNotes.max(0);
	}

	setNoteInterval { arg baseNote, newNote;
		processor.setNoteInterval(newNote - baseNote);
	}

	setTimeDelta { arg delta;
		var actualTime;
		postln("set time delta: " ++ delta);
		timeDelta = delta;
		actualTime = processor.setEchoTime(delta);
		if (tapCallback.notNil, {
			tapCallback.value(actualTime);
		});

	}

	clearHeldNotes {
		numHeldNotes = 0;
	}
}

//---------------------------------------------------------------------
///---- additional DSP classes
//-- _because reasons_, it turns out to be better to roll all the DSP into a single synthdef
//-- but it still seems helpful to organize some peripheral functions away from the main processor

//---------------------------------------------------------------------
// ZCarouselStereoImage: collection of DSP functions for stereo imaging
ZCarouselParadox_StereoImage {
	classvar scale = 0.7071067811865475; // == 1 / sqrt(2)

	*midSideFlip {
		arg in, // assumed stereo input
		midGain=1, sideGain=1, // separate mid/side levels
		bias=0,     // L/R bias in [-1, 1] - applied after M/S, before flip
		flip=0;     // inversion mix (in [0, 1]) - applied last
		var mid, side, l, r, input, output;

		mid = (in[0] + in[1]) * scale;
		side = (in[0] - in[1]) * scale;
		mid = mid * midGain;
		side = side * sideGain;
		l = (mid+side) * scale;
		r = (mid-side) * scale;
		// fixme: nicer pan law would be... nicer
		l = l * (1-bias);
		r = r * (1+bias);
		output = [
			SelectX.ar(flip, [l, r]),
			SelectX.ar(flip, [r, l]),
		];
		^output;
	}
}

//----====----====----====----====----====----====
// ZCarouselCompander: collection of DSP functions for companding
ZCarouselParadox_Compander {

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
		envelope = LagUD.ar(envelope, attack, release);
		^envelope;
	}


	// compute compander gain with hard knee
	// NB / OPTIMIZE: .ampdb and .dbamp are very costly at audio rate
	// fast and usable approximations should be possible
	*compGainHardKnee {
		arg env, threshDb, slopeAbove, slopeBelow, upLag, downLag;
		var envDb = env.ampdb;
		var deltaDb = envDb - threshDb;
		var newTargetDb = threshDb + (deltaDb * if(envDb > threshDb, slopeAbove, slopeBelow));
		var gainDb = newTargetDb - envDb;
		// TRY: apply gain smoothing in different domains besides linear amplitude
		gainDb = LagUD.ar(gainDb.dbamp, upLag, downLag);
		^gainDb
	}

	//----====----====----====----
	//---- TODO: soft knee option!
	//----====----====----====----

	// put companding functions together
	*compandStereo {
		arg input, // assume input is 2-channel array
		// output busses
		outEnv,
		outGain,
		// separate thresholds for comp and expand
		// this is best done here rather than making separate compander blocks,
		// because this way we can re-use the (maybe expensive) input envelope calculation
		thresholdCompress = -12,
		thresholdExpand = -36,
		slopeAbove=1,
		slopeBelow=1,
		// i totally confused myself with "attack" and "release" while making this.
		// "attack" would traditionally mean when gain is *decreasing*,
		// but input amplitude is *increasing*,
		// and vice versa for "release."
		// let's use a consistent and more low-level terminology here:
		// the rising or falling integration time constants.
		envUpLag=0.002,
		envDownLag=0.01,
		gainUpLagCompress=0.008,
		gainDownLagCompress=0.05,
		gainUpLagExpand=0.5,
		gainDownLagExpand=0.1,
		dryMix=0,
		wetMix=1;

		var inputEnvelope = max(
			ZCarouselParadox_Compander.compEnvPeakDecay(input[0], envUpLag, envDownLag),
			ZCarouselParadox_Compander.compEnvPeakDecay(input[1], envUpLag, envDownLag),
		);

		var gainCompress = ZCarouselParadox_Compander.compGainHardKnee(inputEnvelope,
			thresholdCompress, slopeAbove, 1,
			gainUpLagCompress, gainDownLagCompress);

		var gainExpand = ZCarouselParadox_Compander.compGainHardKnee(inputEnvelope,
			thresholdExpand, 1, slopeBelow,
			gainUpLagExpand, gainDownLagExpand);

		var totalGain = gainCompress * gainExpand;

		Out.kr(outEnv, A2K.kr(inputEnvelope));
		Out.kr(outGain, A2K.kr(totalGain));

		dryMix = dryMix * totalGain;
		wetMix = wetMix * totalGain;

		^[
			(dryMix * input[0]) + (wetMix * input[0]),
			(dryMix * input[1]) + (wetMix * input[1])
		]
	}

}

//----====----===----====
// gui utility

ZCarouselParadox_UiState {

	classvar <keys;

	var <carousel;
	var <data;

	*initClass {
		keys = [
			\feedbackLevel,
			\delayTime,
			\feedbackStereoFlip,
			\inputThresholdExpand,
			\distortAmount,
			\distortShape,
			\hpf,
			\lpf,
			\freeze
		]
	}

	*new { arg carousel; ^super.newCopyArgs(carousel).init }

	init {
		// each entry is: [input (midi/toggle), output (whatever)]
		data = Dictionary.with(*(keys.collect({ arg k;
			k -> [0, 0.0]
		})));
		data.postln;
	}

	setValue { arg key, input, output, update=true;
		data[key] = [input, output];
		if(update, {
			postln("carousel.setSynthParam("++key++", "++output++")");
			carousel.setSynthParam(key, output);
		});
	}

	setInput { arg key, value;
		data[key][0] = value;
	}

	setOutput { arg key, value;
		data[key][1] = value;
	}

	getInput { arg key;
		postln("getInput " ++ key ++ " : " ++ data[key][0]);
		^data[key][0]
	}

	getOutput { arg key;
		postln("getOutput " ++ key ++ " : " ++ data[key][1]);
		^data[key][1]
	}

	draw {
		keys.do ({
			arg k, i;
			var input = data[k][0].asString;
			var output = data[k][1];
			var p = 10@(i*15);
			output = if(output.isNumber, {output.round(0.0001)}, {output}).asString;
			Pen.stringAtPoint(k.asString, p, color:Color.white);
			Pen.stringAtPoint(input, p.translate(160@0), color:Color.white);
			Pen.stringAtPoint(output, p.translate(280@0), color:Color.white);
		});
	}
}

ZCarouselParadox_HistoryPlot {
	classvar <paramCount = 3;
	classvar <>historyCount = 128;
	classvar <>maxTime = 32;

	var <processor;
	var <viewParent;
	var <bounds;
	var <view;
	var <data;
	var <frameInterval;
	var <tickRoutine;

	*new { arg processor, viewParent, bounds;
		^super.newCopyArgs(processor, viewParent, bounds).init;
	}

	init {
		"param count: ".postln;
		paramCount.postln;
		data = Array.fill(paramCount, { LinkedList.new(historyCount) });

		view = Array.fill(paramCount, {
			arg i;
			var b = bounds.copy;
			var h = bounds.height / paramCount;
			//			h.postln;
			b.height = h;
			b.top = bounds.top + (h * i);
			"input bounds: ".postln;
			bounds.postln;

			"view bounds: ".postln;
			b.postln;
			MultiSliderView.new(viewParent, b)
			.elasticMode_(1)
			.gap_(0)
			.thumbSize_(0)
			.drawRects_(true)
			.drawLines_(true)
			.isFilled_(true)
		});

		frameInterval = 1 / 15;

		tickRoutine = Routine {
			var displayValue = Array.newClear(paramCount);
			var cond = Condition.new;
			inf.do {
				// fetch all bus values;
				cond.test = false;
				processor.bus[\inputCompGain].get({ arg val;
					displayValue[0] = val.ampdb.max(-60).linlin(-60, 0, 0, 1);
					cond.test = true;
					cond.signal;
				});
				cond.wait;
				cond.test = false;

				processor.bus[\feedbackCompGain].get({ arg val;
					displayValue[1] = val.ampdb.max(-60).linlin(-60, 0, 0, 1);
					cond.test = true;
					cond.signal;
				});
				cond.wait;
				cond.test = false;
				processor.bus[\delayTime].get({ arg val;
					displayValue[2] = val.linlin(0, maxTime, 0, 1);
					cond.test = true;
					cond.signal;
				});
				cond.wait;
				// update the history and the views
				paramCount.do({ arg i;
					while ({data[i].size >= historyCount}, {
						data[i].popFirst;
					});
					data[i].add(displayValue[i]);
					{ view[i].value = data[i].asArray; }.defer;
				});
				{ viewParent.refresh; }.defer;
				frameInterval.wait;
			}
		}.play;
	}
}
