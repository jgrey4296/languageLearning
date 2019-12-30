
//Patch:
ModalBar bar => dac;

//settings:
19 => bar.preset;
0.1 => bar.stickHardness;
0.5 => bar.strikePosition;
0.4 => bar.vibratoGain;
0.8 => bar.masterGain;


[0,1,2,3,4,5,6,7,8,9,10,11,12] @=> int scale[];
1 => int current;

while(true){
       
    <<< "Preset : " + bar.preset() >>>;
    
    scale[current] + 57 => Std.mtof => bar.freq;
    
    .8 => bar.noteOn;

    .5::second => now;

    1 +=> current;
    if(current >= scale.size()){
         0 => current;
}
    
}