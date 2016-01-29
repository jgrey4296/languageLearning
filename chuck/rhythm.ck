//Sound Gen
SinOsc s => ADSR sE => dac;
Noise s2 => HPF hp => LPF lp=> ADSR e => dac;
600 => s.freq;
1000 => hp.freq;
1200 => lp.freq;
//Increment Division:
10.0 => float incDev;
//Pattern Duration:
Math.random2f(2.0,5.0) => float patternInt;
1::second * patternInt => dur patternLength;
<<< "Pattern Length:", patternInt >>>;
//Pattern:
[0,0,0,0,1,1,0,0] @=> int pattern[];
[1.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5] @=> float pattern2[];
//Step duration:
patternLength / pattern.size() => dur stepLength;

//ADSR timing:
stepLength / 4 => dur decayLength;
sE.set(decayLength*4, decayLength, .8, 300::ms);
e.set( 0::ms, decayLength, .0, 5::ms);


//current step:
0 => int step;
//Sound Loop:
while(true){
    if(step >= pattern.size()){
        0 => step;
    }
    pattern[step] => s.gain;
    pattern2[step] => s2.gain;
    e.keyOn();
    //sE.keyOn();
    stepLength / 2 => now;
    e.keyOff();
    //sE.keyOff();
    step++;
    stepLength => now;
}