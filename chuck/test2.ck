SinOsc s => dac;

100 => int direction;

600 => s.freq;

while(true){
    if(s.freq() > 1000 || s.freq() < 440){
        direction * -1 => direction;
    }
    
    s.freq() + direction => s.freq;
    100::ms => now;
}