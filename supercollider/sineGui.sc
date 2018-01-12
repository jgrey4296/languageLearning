
//Create a simple Sine Synth
SynthDef.new(\SSine, { |bus=0, freq=440, amp=0.2|
	Out.ar(bus, SinOsc.ar(freq, 0, 0.5) * amp);
}).add;

//setup the basic synths 
~num = 15;

~synthBank = (0..~num).collect({|x|
	Synth.new(\SSine, [\freq, 300, \amp, 0])	
});

//Then Create controllers for frequency and amplitude
~freqKnobs = ~synthBank.collect({ |s|
	var newKnob = Knob.new(nil, Rect(0, 0, 100, 100));
	newKnob.action_({|x|
		var amnt = 300 + (500 * x.value);
		s.set(\freq, amnt);
	});
	newKnob;
});

~ampKnobs = ~synthBank.collect({ |s|
	var newKnob = Knob.new(nil, Rect(0,0, 100, 100));
	newKnob.action_({|x|
		s.set(\amp, 0.8 * x.value);
	});
	newKnob;
});

~knobPairs = ~freqKnobs +++ ~ampKnobs;
//Put the knobs into a gui

~createWindow = {|pairs, synths|
	var bounds, wind, hlayouts;

	hlayouts = pairs.collect({|p|
		HLayout(*p)
	});
	
	bounds = Window.screenBounds;
	wind = Window.new("SineBank", Rect(bounds.rightBottom.x / 2,
		bounds.rightBottom.y / 2, ~num * 125, ~num * 125)).front;

	wind.layout_(VLayout(*hlayouts));
	wind.onClose_({
		synths.do({|x| x.free; });
	});
	wind;
};

w = ~createWindow.value(~knobPairs, ~synthBank);

