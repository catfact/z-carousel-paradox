
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

			var phaseOffset = (delayTime * SampleRate.ir).min(bufFrames-1);

			var phaseWr = (Phasor.ar(rate:1, end: bufFrames));
			var phaseRd = (phaseWr - phaseOffset + bufFrames);

			var previous = BufRd.ar(2, buffer, phaseWr);
			var output = BufRd.ar(2, buffer, phaseRd);

			// "freeze" interpolation signal
			var freeze = Lag.ar(K2A.ar(\freeze.kr(0)), \freezeLag.kr(0.2));

			var input, feedback, outputDist;

			outputDist = Array.fill(2, { arg i;
				SelectX.ar(\distortShape.kr(0), [
					/// NB: this is a highly inefficient approach!
					/// all the distortion types are computed in parallel..
					output[i].softclip,
					output[i].distort.distort,
					output[i].tanh,
					sin(output[i]*2)*0.77
				])
			});

			output = Array.fill(2, { arg i;
				SelectX.ar(\distortAmount.kr(0), [
					output[i], outputDist[i]
				])
			});

			// highpass and lowpass filtering
			output = LPF.ar(HPF.ar(output, \hpf.kr(20)), \lpf.kr(18000));

			feedback = output *  \feedbackLevel.kr(0);
			// sum input with feedback
			input = In.ar(\in.kr, 2) + feedback;


			// further processing applies to both the first-pass input and the feedback
			// try: play with the routing of these processing steps

			// stereo image processing
			input = ZCarouselParadox_StereoImage.midSideFlip(input,
				\stereoMidGain.kr(1), \stereoSideGain.kr(1), \stereoBias.kr(0), \stereoFlip.kr(0));

			// companding
			input = ZCarouselParadox_Compander.compandStereo(input,
				// control rate output busses for envelope and gain:
				\outEnv.kr, \outGain.kr,
				// compander parameters:
				\thresholdCompress.kr(-12), \thresholdExpand.kr(-36),
				\slopeAbove.kr(1), \slopeBelow.kr(1),
				// input envelope smoothing:
				\compEnvAttack.kr(0.01), \compEnvRelease.kr(0.1),
				// output gain smoothing:
				\gainUpLagCompress.kr(0.01), \gainDownLagCompress.kr(0.1),
				\gainUpLagExpand.kr(0.01), \gainDownLagExpand.kr(0.1)
			);

			// mix with previous buffer contents
			input = [
				SelectX.ar(freeze, [input[0], previous[0]]),
				SelectX.ar(freeze, [input[1], previous[1]]),
			];

			// write to the buffer
			BufWr.ar(input, buffer, phaseWr);

			// and finally, output the delayed signal
			Out.ar(\out.kr(0), output * \level.kr(1));

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
			\comp_env, Bus.control(server, 1),
			\comp_gain, Bus.control(server, 1),
		]);

		// NB: assumes that this is constructed in a Thread/Routine!
		server.sync;

		synth = Synth.new(\ZCarouselProcessor, [
			\buffer, buffer,
			// NB: here's one place to change for different bus structure
			\in, context.bus[\hw_in],
			\out, context.bus[\hw_out],
			\outEnv, bus[\comp_env],
			\outGain, bus[\comp_gain],
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
		synth.set(\delayTime, time);
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
		postln("set time delta: " ++ delta);
		timeDelta = delta;
		processor.setEchoTime(delta);
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
	*compGainHardKnee {
		arg env, threshDb, slopeAbove, slopeBelow, attack, release;
		var envDb = env.ampdb;
		var deltaDb = envDb - threshDb;
		var newTargetDb = threshDb + (deltaDb * if(envDb > threshDb, slopeAbove, slopeBelow));
		var gainDb = newTargetDb - envDb;
		// try: apply gain smoothing in different domains besides linear amplitude
		gainDb = LagUD.ar(gainDb.dbamp, release, attack);
		^gainDb
	}

	//----====----====----====----
	//---- TODO: soft knee option!
	//----====----====----====----

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
		// i totally confused myself with "attack" and "release" while making this.
		// "attack" would traditionally mean when gain is *decreasing*,
		// but input amplitude is *increasing*,
		// and vice versa for "release."
		// let's use a consistent and more low-level terminology here:
		// the rising or falling integration time constants.
		envUpLag=0.002, envDownLag=0.01,
		// effectively we are cascading two amplitude smoothers for envelope and
		gainUpLagCompress=0.008, gainDownLagCompress=0.05,
		gainUpLagExpand=0.5, gainDownLagExpand=0.1;

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

		^[input[0] * totalGain, input[1] * totalGain]
	}

}

//----====----===----====
// gui utility

ZCarouselParadox_UiState{

	classvar <keys;

	var <carousel;
	var <data;

	*initClass {
		keys = [
			\feedbackLevel,
			\delayTime,
			\stereoFlip,
			\thresholdExpand,
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
			carousel.setSynthParam(key, output);
		});
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
			Pen.stringAtPoint(input, p.translate(120@0), color:Color.white);
			Pen.stringAtPoint(output, p.translate(220@0), color:Color.white);
		});
	}
}