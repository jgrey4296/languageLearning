//SynthDefs
SynthDef.new(\SSine, { |freq=440, dur=0.2|
	var osc, noise, amp;
	osc = SinOsc.ar(freq, 0, 0.2);
	amp = EnvGen.kr(Env.perc(0.01, dur), doneAction:2);
	Out.ar(0, (osc) * amp);
}).add


//Effects

//Synth/Effect Adding


//Event Def
~note = {|note, dur| (note:note, dur:dur, \instrument:\SSine).play; }
~note.value(35, 0.9)
//------------------------------
//Clock
SystemClock.sched(0, { ~note.value(25 + 8.rand, 0.5); 1 } )

t = TempoClock.default.beats
TempoClock.default.tempo = 2;
TempoClock.default.schedAbs(t+2, { ~note.value(25 + 8.rand, 0.7); 0.5});
TempoClock.default.clear

//Routines
r = Routine({
	loop {
		~note.value(25 + 10.rand, 0.5);
		0.5.yield;
	}
})
r.play;
r.reset;
r.stop;

//Events and Delay
b = Bus.audio(s, 1);
d = Synth.new(\TestDelay, [\inbus, b, \delT, 7]);
TempoClock.sched(0, {
	(\instrument:\SSine,
		bus:b,
		note:0 + 15.rand,
		dur: 2,//[0.2, 0.5, 0.9, 1, 1.2, 1.5].choose,
		addAction: \addAtHead).play;
	("Num Synths: " + s.numSynths).postln;
	1; });

