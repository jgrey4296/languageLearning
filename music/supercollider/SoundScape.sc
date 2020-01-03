// Soundscape Generator

//Sample Playback Synth
//Able to controlled by name: 'engine' 'birds' etc
//Att/Sus in seconds, calculated with TempoClock.beats2secs
SynthDef.new("Sampler", { |buf=0, sampleName, att, sus, rel, loop=0, gate=1|
	//todo, with a simple asr envelope
	var env, player;
	env = EnvGen.ar(Env.asr(att, sus, rel), gate:gate, doneAction:2);
	player = PlayBuf.ar(2, buf, loop:loop);
	Out.ar(buf, player * env);
}).add;


//Create a Dictionary of name -> samplebuffer
~loadSamples ={ |fileNames|
	var dict = Dictionary.new;
	fileNames.do({|x|
		dict.put(x, Buffer.read(s, File.getcwd +/+ x + ".aif"));
	});
	dict;
}.value()

~PatternDict = Dictionary.new;
~ActiveSamples = Dictionary.new;

//OSC messages to define playback times and volumes
//That create the patterns to layer
//in a depth 2 set of patterns.
//ie: subdivision of a day into hours and quarter hours.
~Socket = NetAddr("127.0.0.1", 7771);
o = OSCFunc({ |msg, time, add, recvPort|
	//use .pairsDo
	//Receive a Msg of [\timeString, \sample, \prob, \tags]
	//~PatternDict.put();
	
}, '/command', ~Socket);

//TempoClock to Move through the 'Day' Pattern
TempoClock.default.schedAbs(TempoClock.nextBar, { TempoClock.default.beatsPerBar = 3});
TempoClock.default.sched(TempoClock.nextBar, {
	//Each Beat (numHours * numSubDiv?):
	//select what *can* fire,
	//see if it probabilistically *does* fire

	//check playing sounds
	//probabilistically stop them, or hard stop
	
});


//pattern :: i -> [DHA] -> (PSeq(Day), [DHA])
//DHA :: (Days :: [Day], Hours :: (Hour, [Activity]), Activities :: [Activity]) 
//Day :: [(Hour, [Activity])]
//Activity :: [(TimeDiv, Sound, Probability, fadeType)]


//[0-4,6-8] -> engineNoise #probability:0.8 #repeat #distant
//[5-8] -> birdSong #probability:0.2 #continuous #fadeIn:0.25 #fadeOut:0.5
//[-] -> nuclearExplosion #probability:0.00001 #once #fadeIn:0 #fadeOut:0


