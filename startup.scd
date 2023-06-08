s = Server.default;
s.options.device = "Steinberg UR-RT2";

s.waitForBoot { Routine {

	ZCarouselParadox.sendSynthDefs(s);

	s.sync; // <- important

	z = ZCarouselParadox.new(s, 2);

	{
		z.bus[\hw_out].scope;
	}.defer;


	// by default, the tuning system is 12tet; can override it thusly:
	~ratios = [ 1, 16/15, 9/8, 6/5, 5/4, 4/3, 729/512, 3/2, 8/5, 10/6, 16/9, 15/8];
	z.processor.tuningFunction = { arg interval;
		~ratios[interval % 12] * (2 ** (interval / 12).floor)
	};

	z.processor.synth.set(\feedbackLevel, -4.dbamp);
	// ping pong
	z.processor.synth.set(\stereoFlip, 1);
	// engage the expander
	z.processor.synth.set(\thresholdExpand, -42);
	z.processor.synth.set(\slopeBelow, 16);
	z.processor.synth.set(\compGainAttack, 4);
	z.processor.synth.set(\compGainRelease, 1);

	// let's have a little level fader
	~level_taper_break = -36.dbamp;
	~level_taper = { arg midi;
		if (midi < 55, {
			(midi / 54.0) * ~level_taper_break;
		}, {
			((midi-127)*0.5).dbamp
		});
	};
	// looks like this:
	// Array.series(128).collect({ arg x; ~level_taper.value(x) }).plot;


	// let's have a little UI
	~ui_state = Dictionary.new;
	~ui_state[\freeze] = 0;
	~ui_state[\time] = z.processor.echoTime;

	{
		w = Window("z-carousel-paradox", Rect(100, 200, 400, 300), false);
		u = UserView(w, Rect(0, 0, w.bounds.width, w.bounds.height));
		u.background = Color.black;
		u.drawFunc = {
			// FIXME: not efficient of course
			~ui_state.keys.asArray.sort.do({ arg k, i;
				var val = ~ui_state[k];
				var p = 10@(i*15);
				Pen.stringAtPoint(k.asString, p, color:Color.white);
				Pen.stringAtPoint(val.asString, p.translate(100@0), color:Color.white);
				if (k == \time, {
					Pen.stringAtPoint(z.processor.echoTime.asString,
						p.translate(200@0), color:Color.white);
				});
			});
		};
		w.front;
	}.defer;

	~ui_set_midi = { arg key, value;
		switch(key,
			{\feedback}, {
				~ui_state[key] = value;
				z.setSynthParam(\feedbackLevel, ~level_taper.value(value));
			},
			{\flip}, {
				~ui_state[key] = value;
				z.setSynthParam(\stereoFlip, value.linlin(0, 127, 0, 1));
			},
			{\expansion}, {
				~ui_state[key] = value;
				z.setSynthParam(\thresholdExpand, value.linlin(0, 127, -48, 0));
			},

			{\freeze}, {
				if (value > 0, {

					if (~ui_state[\freeze] > 0, {
						~ui_state[\freeze] = 0;
					}, {
						~ui_state[\freeze] = 1;
					});
					z.setSynthParam(\freeze, ~ui_state[\freeze]);
				});
			},
			{\time}, {

				// let's change echo time by discrete steps in log(2) domain
				// this means the gesture should be reversible
				// assuming the controller is sending [0, 127] and not a delta
				z.processor.setEchoTime(
					z.processor.echoTime
					* if (~ui_state[\time] < value, {
						1.1892071150027
					}, {
						0.84089641525371
					})
				);
				~ui_state[\time] = value;
		});
		{ u.refresh; }.defer;
	};


	// some more MIDI controls; customize to taste
	// these 8 CCs happen to be how i have my controller knobs set up...

	// uncoment to inspect all incoming MIDI:
	// z.midiInput.verbose = true;

	z.midiInput.cc(2, { arg value;
		~ui_set_midi.value(\feedback, value);
	});

	z.midiInput.cc(7, { arg value;
		~ui_set_midi.value(\flip, value);
	});

	z.midiInput.cc(10, { arg value;
		~ui_set_midi.value(\expansion, value);
	});

	z.midiInput.cc(11, { arg value;
		~ui_set_midi.value(\time, value);
	});

	z.midiInput.cc(12, { arg value;
		//...
	});

	z.midiInput.cc(13, { arg value;
		//...
	});

	z.midiInput.cc(71, { arg value;
		//...
	});

	z.midiInput.cc(74, { arg value;
		//...
	});

	// ... and these 8 are my pads
	z.midiInput.cc(16, { arg value;
		~ui_set_midi.value(\freeze, value);
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