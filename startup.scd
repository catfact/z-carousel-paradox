s = Server.default;
s.options.device = "Steinberg UR-RT2";

s.waitForBoot { Routine {

	ZCarouselParadox.sendSynthDefs(s);

	s.sync; // <- important


	z = ZCarouselParadox.new(s, 2);

	{
		z.bus[\hw_out].scope;
	}.defer;

	z.processor.synth.set(\feedbackLevel, 0.35);


}.play }