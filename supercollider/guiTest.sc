
var createWindow = {
	var bounds, wind, button, text, text2, theKnob;

	bounds = Window.screenBounds;
	wind = Window.new("Test", Rect(bounds.rightBottom.x / 2,
		bounds.rightBottom.y / 2, 200, 200)).front;
	//wind.view.decorator = FlowLayout(wind.view.bounds);
	button = Button.new(nil, Rect(0, 0, 100, 100)).states_([["One"], ["Two"], ["Three"]]);
	text = StaticText.new(nil, Rect(0, 0, 100, 100)).string_("Initial");
	text2 = StaticText.new(nil, Rect(0,0, 200, 200)).string_("A Test Text");
	theKnob = Knob.new(nil, Rect(0,0,100,100));
	wind.layout_(VLayout(HLayout(button, text), text2, theKnob));
	button.action_({|x|
		text2.mouseLeaveAction_({text2.string_("bloo");});
		text2.mouseEnterAction = {text2.string_("Blah");};
	});

	theKnob.action_({|x| text.string_(x.value.asString.select({|x, i| i < 4})); });
	
	wind;
};

w = createWindow.value();


