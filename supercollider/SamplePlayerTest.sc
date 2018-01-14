SynthDef(\Sampler, {|bufNum, rate=1, lfo=0.1|
	Out.ar(0, PlayBuf.ar(2, bufNum, SinOsc.ar(lfo, 0, 0.2) * BufRateScale.kr(bufNum) * rate));
}).add;

b = Buffer.read(s, File.getcwd +/+ "aSample.aif");


a = Synth(\Sampler, [\bufNum, b, \rate, 0.2]);
a.set(\rate, 1)
a.set(\lfo, 0)
a.free