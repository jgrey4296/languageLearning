(
//{SinOsc.ar(1000,0,0.1)}.play;

SynthDef.new("test", { arg freq = 440; Out.ar(0,SinOsc.ar(freq,0,0.2)) }).add;


)