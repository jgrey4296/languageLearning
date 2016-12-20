//Basic server commands
s.boot;
s.reboot;
s.quit;
s.freeAll;
s.status;
//Simple control of computer voice
"hello".speak;
// a simple synth
{ SinOsc.ar(440,0,0.5,0.2); }.play;
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
b.set("pitch2",800)
c.set("pitch",550,"pitch2",700)
b.free
c.free
s.reboot

/** Bus example:
	Route outFreq through a bus, into inFreq
	**/
b = Bus.control(s,1); //the bus

//The control signal
SynthDef("tutorial-outFreq", { arg freq = 400, offset=40, bus;
	Out.kr(bus,SinOsc.kr(1,0,freq/offset,freq));
}).add;

//The audio signal
SynthDef("tutorial-inFreq", { arg bus, freqOffset = 0;
	Out.ar(0, SinOsc.ar(In.kr(bus) + freqOffset, 0, 0.5));
}).add;

//Instantiation
x = Synth.new("tutorial-outFreq", [\bus, b]);
y = Synth.after(x,"tutorial-inFreq",[\bus,b]);
z = Synth.after(x,"tutorial-inFreq",[\bus,b,\freqOffset,200])

x.set("offset",10)
x.set("freq",1000)
y.set("freqOffset",200)
z.set("freqOffset",250)

r = Routine({
	loop {
		({SinOsc.ar(440,0,0.2) * EnvGen.kr(Env.perc(0.01, 0.2), doneAction: 2);}).play;
		1.yield;
	}
});

r.play
r.stop

//Repeating task
t = { Task({
	var i = 0, n = [440,560,880,1000];
	loop {
		//note the wrapping/folding 'at' operator:
		({ SinOsc.ar(n@@i,0,0.2) * EnvGen.kr(Env.perc(0.01,0.2), doneAction: 2);}).play;
		i = i + 1;
		1.wait;
	}
});
}

u = t.value
v = t.value
c = TempoClock.default
c.tempo = 2
q = Task({var i = 0;
	loop{
		"Beat: ".post;
		(i % 4).postln;
		i = i + 1;
		1.yield;
	}
})

(
q.play(c,true,[4,0]);
u.play(c,true,[4,0]);
v.play(c,true,[4,1.222]);
)
v.stop
u.stop
q.stop
c.dump
c.class.dumpInterface