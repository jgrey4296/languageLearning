//A Simple Sine Synth
SynthDef.new(\SSine, { |bus=0, freq=440, dur=0.2|
	var osc, noise, amp;
	osc = SinOsc.ar(freq, 0, 0.5);
	amp = EnvGen.kr(Env.perc(0.01, dur), doneAction:2);
	Out.ar(bus, (osc) * amp);
}).add;


//A Simple Mix of two oscillators
SynthDef.new(\MixOsc, { |bus=0, freq=440, dur=0.2, mix=0.5, offset=20|
	var osc1, osc2, amp, cFreq;
	cFreq = Clip.kr(freq, 300, 500);
	osc1 = SinOsc.ar(cFreq, 0, 0.5);
	osc2 = VarSaw.ar(cFreq, 0, 0.5, 0.5);
	amp = EnvGen.kr(Env.perc(0.01, dur), doneAction:2);
	Out.ar(bus, ((osc1 * Clip.kr(mix, 0, 1)) + (osc2 * Clip.kr((1 - mix), 0, 1))) * amp);
}).add;

//Use of a plucked String
//No Done action, should check this
SynthDef.new(\SPluck, {|bus=0, freq=440, gate=1, pos=0.1|
	var pluck, pluckSig, amp;
	pluckSig = LFClipNoise.ar(2000) * EnvGen.ar(Env.perc(0.01, 0.1));
	pluck = DWGPlucked.ar(freq, 0.5, gate, pos, 1, 20, pluckSig);
	Out.ar(bus, pluck);
}).add;

//LineNoise
SynthDef.new(\LNoise, {|bus=0, att=1, rel=1|
	var noise, amp;
	amp = EnvGen.ar(Env.linen(att, 0.0, rel, 0.6), doneAction:2);
	noise = PinkNoise.ar(amp);
	Out.ar(bus, noise);
}).add;

//A Simple Delayed Synth
SynthDef.new(\TestDelay, {|inbus=8, outbus=0, delT=3, mix=0.5|
	var signal, delayed, env, combined;
	var input = In.ar(inbus);
	delayed = DelayL.ar(input, delT, delT);
	Out.ar(outbus, (input * Clip.kr(mix, 0, 1)) +(delayed * Clip.kr(1-mix, 0, 1)));
}).add;

//Controlling the synth with .set
//retrigger by setting gate to 0, then to 1.
SynthDef(\SimpleRetriggered, { |freq=440, gate=0|
	var env = EnvGen.kr(Env.asr(0.9, 0.5, 0.5), gate: gate);
	Out.ar(0, SinOsc.ar(freq, 0, 0.5) * env)
}).add;

//Like SimpleRetriggered,
//but needs .run to retrigger, along with a gate signal
SynthDef(\RetriggerPaused, { |freq=440, gate=0|
	var env = EnvGen.kr(Env.asr(0.9, 0.5, 0.5), gate: gate);
	PauseSelf.kr(env);
	Out.ar(0, SinOsc.ar(freq, 0, 0.5) * env)
}).add;
