<<< "start" >>>;
Std.rand2f(0,10) * 1::second => dur wait;
<<< "Waiting:", wait >>>;

now + wait => time later;
later => now;
<<< "after wait" >>>;

SinOsc s => dac;
440 => s.freq;

while(true){
    if(s.freq() > 1000){
        440 => s.freq;
    }
    s.freq() + 0.5 => s.freq;
    1::ms => now;
}