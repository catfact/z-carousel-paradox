s = Server.default;
// s.options.device = "Steinberg UR-RT2";
// ~mono_input = false;

~mono_input = true;

s.waitForBoot { Routine {

	ZCarouselParadox.sendSynthDefs(s);

	s.sync; // <- important

	// removed the `loopTime` synth parameter since it always caused a click as implemented
	// instead, made this classvar writable; it will be used as the buffer length for new processors
	ZCarouselParadox_Processor.bufferLength = 32.0;

	z = ZCarouselParadox.new(s, if(~mono_input, {1}, {2}));

	{
		z.bus[\hw_out].scope;
	}.defer;


	// by default, the tuning system is 12tet; can override it thusly:
	~ratios = [ 1, 16/15, 9/8, 6/5, 5/4, 4/3, 729/512, 3/2, 8/5, 10/6, 16/9, 15/8];
	z.processor.tuningFunction = { arg interval;
		~ratios[interval % 12] * (2 ** (interval / 12).floor)
	};

	// feed back
	z.setSynthParam(\feedbackLevel, -4.dbamp);
	// ping pong
	z.setSynthParam(\stereoFlip, 1);
	if (~mono_input, { z.setSynthParam(\inputStereoBias, -0.7); });

	// set input compander as a gate
	z.setSynthParam(\inputThresholdExpand, -16);
	z.setSynthParam(\inputGainUpLagExpand, 0.6);
	z.setSynthParam(\inputGainUpLagExpand, 0.01);
	z.setSynthParam(\inputSlopeBelow, 16);
	z.setSynthParam(\inputGainDownLagExpand, 01);
	z.setSynthParam(\inputGainUpLagExpand, 0.6);

	// set feedback compander as a compressor
	z.setSynthParam(\feedbackThresholdCompress, -18);
	z.setSynthParam(\feedbackSlopeAbove, 0.3);
	z.setSynthParam(\feedbackGainDownLagCompress, 0.04);
	z.setSynthParam(\feedbackGainDownLagCompress, 0.09);

	// let's have a little level fader
	~level_taper_break = -36.dbamp;
	// 127 - (36*2) -> 55

	~level_taper_log = {arg midi; ((midi-127)/4).dbamp };
	~level_taper_break_midi = 24;
	~level_taper_break_value = ~level_taper_log.value(~level_taper_break_midi);
	~level_taper = { arg midi;
		if (midi < ~level_taper_break_midi, {
			midi.linlin(0, ~level_taper_break_midi, 0, ~level_taper_break_value)
		}, {
			~level_taper_log.value(midi);
		})
	};
	// looks like this:
	/*
	Array.series(128).collect({ arg x; ~level_taper.value(x) }).plot;
	Array.series(128).collect({ arg x; ~level_taper.value(x).ampdb.max(-100) }).plot;
	*/

	//  UI state mgmt got bloated enough to want a helper class
	d = ZCarouselParadox_UiState.new(z);
	z.midiController.tapCallback = { arg delta; d.setOutput(\delayTime, delta) };

	// GUI display elements (views)
	{
		w = Window("z-carousel-paradox", Rect(100, 200, 400, 520), false);
		u = UserView(w, Rect(0, 0, w.bounds.width, w.bounds.height));
		u.background = Color.black;
		u.drawFunc = {
			d.draw;
		};

		// also a little class to show history plots
		ZCarouselParadox_HistoryPlot.maxTime = ZCarouselParadox_Processor.bufferLength;
		v = ZCarouselParadox_HistoryPlot.new(z.processor, w, Rect(0, 280, 400, 240));

		w.front;
	}.defer;

	// all MIDI updates go through this to keep UI display in sync
	~ui_handle_midi = { arg key, value;
		switch(key,
			{\feedbackLevel}, {
				d.setValue(\feedbackLevel, value, ~level_taper.value(value));
			},
			{\feedbackStereoFlip}, {
				d.setValue(\feedbackStereoFlip, value, value.linlin(0, 127, 0, 1));
			},
			{\inputThresholdExpand}, {
				d.setValue(\inputThresholdExpand, value, value.linlin(0, 127, -48, 0));
			},

			{\freeze}, {
				if (value > 0, {
					var newVal = if(d.getInput(\freeze) > 0, { 0 }, { 1 });
					d.setValue(\freeze, newVal, newVal);
				});
			},
			{\delayTime}, {
				// let's change echo time by discrete steps in log(2) domain
				// this means the gesture should be reversible
				// assuming the controller is sending [0, 127] and not a delta
				d.setValue(\delayTime, value,
					d.getOutput(\delayTime) * if(value < d.getInput(\delayTime), {
						0.84089641525371
					}, {
						1.1892071150027
					})
				);
			},
			{\distortAmount}, {
				d.setValue(\distortAmount, value, value.linlin(0, 127, 0, 1));
			},
			{\distortShape}, {
				d.setValue(\distortShape, value, value.linlin(0, 127, 0, 5));
			},
			{\hpf}, {
				var newVal = value.min(d.getInput(\lpf) - 1);
				d.setValue(\hpf, newVal, newVal.midicps);
			},
			{\lpf}, {
				var newVal = value.max(d.getInput(\hpf) + 1);
				d.setValue(\lpf, newVal, newVal.midicps);
			}
		);
		{ u.refresh; }.defer;
	};


	//--- initial state
	~ui_handle_midi.value(\feedbackLevel, 120);
	~ui_handle_midi.value(\feedbackStereoFlip, 127);
	~ui_handle_midi.value(\inputThresholdExpand, 40);
	//~ui_handle_midi.value(\delayTime, 64);
	// needs special handling:
	d.setValue(\delayTime, 64, 1, false);
	~ui_handle_midi.value(\distortAmount, 0);
	~ui_handle_midi.value(\distortShape, 0);
	~ui_handle_midi.value(\hpf, 0);
	~ui_handle_midi.value(\lpf, 127);

	// some more MIDI controls; customize to taste
	// these 8 CCs happen to be how i have my controller knobs set up...

	// uncoment to inspect all incoming MIDI:
	z.midiInput.verbose = true;

	z.midiInput.cc(2, { arg value;
		~ui_handle_midi.value(\feedbackLevel, value);
	});

	z.midiInput.cc(7, { arg value;
		~ui_handle_midi.value(\feedbackStereoFlip, value);
	});

	z.midiInput.cc(10, { arg value;
		~ui_handle_midi.value(\inputThresholdExpand, value);
	});

	z.midiInput.cc(11, { arg value;
		~ui_handle_midi.value(\delayTime, value);
	});

	z.midiInput.cc(12, { arg value;
		~ui_handle_midi.value(\distortAmount, value);
	});

	z.midiInput.cc(13, { arg value;
		~ui_handle_midi.value(\distortShape, value);
	});

	z.midiInput.cc(71, { arg value;
		~ui_handle_midi.value(\hpf, value);
	});

	z.midiInput.cc(74, { arg value;
		~ui_handle_midi.value(\lpf, value);
	});

	// ... and these 8 are my pads
	z.midiInput.cc(16, { arg value;
		~ui_handle_midi.value(\freeze, value);
	});

	z.midiInput.cc(17, { arg value;
	});

	z.midiInput.cc(18, { arg value;
	});

	z.midiInput.cc(19, { arg value;
	});

	z.midiInput.cc(20, { arg value;
	});

	z.midiInput.cc(21, { arg value;
	});

	z.midiInput.cc(22, { arg value;
	});

	z.midiInput.cc(23, { arg value;
	});


}.play }