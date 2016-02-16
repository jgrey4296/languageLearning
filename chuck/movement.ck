
fun void movement(dur totalLength){
    SinOsc s => Gain g;
    SinOsc s2 => g;
    g.gain(0.0);
    g => dac;
    
    Math.random2(400,800) => int start;
    start + Math.random2(200,1000) => int end;
    
    s.freq(start);
    s2.freq(start+100);

    totalLength / 50 => dur loopTime;

    while(g.gain() < 0.5){
        g.gain() + 0.001 => g.gain;
        1::ms => now;
    }
    me.yield();
    loopTime * 5 => now;
    while(s.freq() < end){
        1 + s.freq() => s.freq;
        1 + s2.freq() => s2.freq;
        loopTime => now;
    }
    loopTime * 5 => now;
    while(s.freq() > start){
        s.freq() - 1 => s.freq;
        s2.freq() - 1 => s2.freq;
        loopTime => now;
    }

    while(g.gain() > 0){
        g.gain() - 0.0001 => g.gain;
        1::ms => now;
    }
    
}

2::second => dur curLength;
for(0 => int i; i < 10; i++){
    spork ~ movement(curLength);
    me.yield();
    curLength + 25::ms => curLength;
}

while(true){
    2::second => now;
}