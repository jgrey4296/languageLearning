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





