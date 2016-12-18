//Basic server commands
s.boot;
s.reboot;
s.quit;
s.freeAll;
s.status;
//Simple control of computer voice
"hello".speak;
// a simple synth
{ SinOsc.ar(1000,0,0.1); }.play;
//A Pair of signals
{Mix.ar([SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)]) }.scope
//Changing the amplitude:
{ var ampOsc;
	ampOsc = SinOsc.kr(0.5, 1.5pi, 0.5, 0.5);
	SinOsc.ar(440, 0, ampOsc);
}.play

//Synthdef:
a = SynthDef.new("Blah", { arg pitch = 440, pitch2 = 442; var outArray;
	outArray = [SinOsc.ar(pitch,0,0.2), SinOsc.ar(pitch2,0,0.2)];
	Out.ar(0,outArray);
}).add
b = Synth.new("Blah",["pitch",440, "pitch2", 450]);
c = Synth.new("Blah",["pitch",450]);
b.set("pitch2",428)
b.free
c.free
s.reboot