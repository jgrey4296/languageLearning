
//Create a simple Sine Synth
SynthDef.new(\SSine, { |bus=0, freq=440, amp=0.2|
	Out.ar(bus, SinOsc.ar(freq, 0, 0.5) * amp);
}).add;

//setup the basic synths 
~num = 10;
~knobSize = 50;

~synthBank = (0..~num).collect({|x|
	Synth.new(\SSine, [\freq, 300, \amp, 0])	
});

//Then Create controllers for frequency and amplitude
~freqKnobs = ~synthBank.collect({ |s|
	var newKnob = Knob.new(nil, Rect(0, 0, ~knobSize, ~knobSize));
	newKnob.action_({|x|
		var amnt = 300 + (500 * x.value);
		s.set(\freq, amnt);
	});
	newKnob;
});

~ampKnobs = ~synthBank.collect({ |s|
	var newKnob = Knob.new(nil, Rect(0,0, ~knobSize, ~knobSize));
	newKnob.action_({|x|
		s.set(\amp, 0.8 * x.value);
	});
	newKnob;
});

~knobPairs = ~freqKnobs +++ ~ampKnobs;
//Put the knobs into a gui

~createWindow = {|pairs, synths|
	var bounds, wind, hlayouts, interHeight, interWidth, scopeView;

	interHeight = 25 + 200 + (~num * ~knobSize);
	interWidth = 250;
	hlayouts = pairs.collect({|p|
		var newView = View.new(nil, Rect(0,0,~knobSize * 2, ~knobSize));
		newView.layout_(HLayout(*p));
		newView.background = Color.rand;
		newView;
	});

	scopeView = View.new(nil, Rect(0, 0, 100, 200));
	Stethoscope.new(nil, 1, view: scopeView, bufnum: 0);
	
	bounds = Window.screenBounds;
	wind = Window.new("SineBank", Rect(bounds.rightBottom.x / 2,
		bounds.rightBottom.y / 2, interWidth, interHeight)).front;

	wind.layout_(HLayout(scopeView, VLayout(*hlayouts)));
	wind.onClose_({
		synths.do({|x| x.free; });
	});
	wind;
};

w = ~createWindow.value(~knobPairs, ~synthBank);

