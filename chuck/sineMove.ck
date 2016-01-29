//Sound Generator:
SinOsc s => dac;

//Increment Division:
10.0 => float incrementDivision;

//Target Frequencies:
[440.0,1000.0] @=> float pitches[];
pitches[1] - pitches[0] => float range;
Std.rand2f(pitches[0],pitches[1]) => s.freq;
//The Increment
range / incrementDivision => float inc;
//Duration of movement
2::second => dur moveLength;
//Step to match the duration for frequency increment
moveLength / incrementDivision => dur incLength;

//Sound loop
while(true){
    s.freq() + inc => s.freq;
    if(s.freq() >= pitches[1] || s.freq() <= pitches[0]){
        <<< "switching" >>>;
        -1 *=> inc;
    }
    incLength => now;
}