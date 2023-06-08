s = Server.default;
s.options.device = "Steinberg UR-RT2";

s.waitForBoot { Routine {

	ZCarouselParadox.sendSynthDefs(s);

	s.sync; // <- important

	z = ZCarouselParadox.new(s, 2);

	// z = ZAudioContext.new(s, 2, 2, 0, 0);
	//
	// ~echo_in_b = Bus.audio(s, 2);
	// ~echo_in_s = Synth.new(\patch_stereo, [\in, z.bus[\hw_in], \out, ~echo_in_b], z.group[\in], \addAfter);
	//
	// ~echo_out_b = Bus.audio(s, 2);
	// ~echo_out_s = Synth.new(\patch_stereo, [\out, z.bus[\hw_out], \in, ~echo_out_b], z.group[\out], \addBefore);
	//
	// ~buflen = 32.0;
	// ~buf = Buffer.alloc(s, ~buflen * s.sampleRate, 2);
	//
	// ~comp_env_b = Bus.control( s);
	// ~comp_gain_b = Bus.control(s);
	//
	// s.sync;
	//
	// x = Synth.new(\ZCarouselProcessor, [
	// 	\buffer, ~buf.bufnum,
	// 	\loopTime, ~buf.duration,
	// 	\delayTime, 4.0,
	// 	\in, ~echo_in_b.index,
	// 	\out, ~echo_out_b.index,
	// 	\outEnv, ~comp_env_b.index,
	// 	\outGain, ~comp_gain_b.index,
	// ], z.group[\process]);
	//
	// {
	// 	z.bus[\hw_in].scope;
	// 	//		~echo_in_b.scope;
	// 	//		~echo_out_b.scope;
	//
	// }.defer;

}.play }