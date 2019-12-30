//A Simple Supercollider state machine
{
	var state_machine = Dictionary.new;
	var notes = Dictionary.new;
	var location = 'c';
	var i = 10;

	SynthDef.new(\SMSy, {|freq| Out.ar(0,SinOsc.ar(freq,0.0,0.2) *
		EnvGen.kr(Env.perc(0.04,0.4), doneAction: 2));}).add;

	notes = notes.putPairs(['a',44,'b',46,
		'c',47,'d',49,'e',51,'f',52,'g',54].collect({|x| if(x.isInteger, x - 12, x)}));
	
	//First a state machine of transitions:
	state_machine.putPairs([
		'a',['c','d','e'],
		'b',['d','e','f'],
		'c',['e','f','g'],
		'd',['f','g','a'],
		'e',['g','a','b'],
		'f',['a','b','c'],
		'g',['b','c','d']
	].collect({|x|
		if((x.class == Symbol).not,
			{x.select({|y,i| i%2==0})},
			x)
	}));

	r = Routine({
	while( {i > 0}, {
		var theEvent = (note:notes[location], \instrument:\SMSy);
		i = i - 1;
		theEvent.play;
		location.post; "->".post; notes[location].postln;
		location = state_machine[location].scramble.pop;
		1.wait;
	})
	});

	r.play;
	r.play;
	
}.value