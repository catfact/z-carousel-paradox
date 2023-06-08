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

	/* // linear crossfade between old and new loop content
	z.processor.synth.set(\freeze, 0.0);
	z.processor.synth.set(\freeze, 1.0);
	*/

	// some more MIDI controls; customize to taste
	z.midiInput.cc(2, { arg value; });
	z.midiInput.cc(7, { arg value; });
	z.midiInput.cc(10, { arg value; });
	z.midiInput.cc(11, { arg value; });


}.play }