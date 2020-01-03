//There must be 4=>n parameters
if(me.args() != 0){
    <<< "Insufficient args" >>>;
    me.exit();
}

//Data:
[0,4,8,12] @=> int scale[];
<<< "------" >>>;
Std.mtof(69) => float basePitch;

//Ugen Graph:
SinOsc s => ADSR e => JCRev r => dac;
basePitch => s.freq;
0.0 => r.mix;
e.set(2::ms,200::ms,0.0,10::ms);

//timing:
120 => int bpm;
60::second / bpm => dur bps;
bps / 2 => dur step;

0 => int count;
1 => int inc;

//me.yield();

//Loop:
while(true){
    //scale[ Math.random2(0,scale.size()-1)] => int nextPitch;
    count + inc => count;
    if(count >= scale.size()-1 || count <= 0 ){
        inc * -1 => inc;
    }

    scale[count] => int nextPitch;
    basePitch + Std.mtof(69 + nextPitch) => float newPitch;
    //<<< "Next Pitch:",nextPitch," New Pitch: ",newPitch  >>>;
    newPitch => s.freq;
    e.keyOn();
    step => now;
    e.keyOff();
    step => now;
}