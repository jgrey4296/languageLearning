{
	n = NetAddr.new("127.0.0.1", 4444);    // create the NetAddr
	// create the OSCresponderNode
	SynthDef.new("example1",
		{ arg freq;
			var env = EnvGen.kr(Env.perc(attackTime: 0.01, releaseTime:0.2, level:0.5), doneAction:2);
			Out.ar(0, SinOsc.ar(freq, mul:env)); }).load(s);

	r = OSCFunc({ arg msg, time, addr, recvPort; Synth.new("example1", [\freq, 400 + (msg[2] * 20)]); msg.postln; }, "/phys", n);
}
