//Midi setup
MidiIn min;
if(! min.open( 1 ) ) me.exit();
MidiOut mout;
if(! mout.open(0) ) me.exit();

//utility functions:
fun MidiMsg keyDown(int note,int vel){
    MidiMsg msg;
    0x90 => msg.data1;
    note => msg.data2;
    vel => msg.data3;
    mout.send(msg);
    return msg;
}
fun void keyUp(MidiMsg m){
    0x80 => m.data1;
    mout.send(m);
}
//Potential Notes:
40 => int baseNote;
[0,1,2,3,4,5,6,7,8,9,10,11,12] @=> int notes[];


//BPM setup:
120 => int bpm;
60::second / bpm => dur crotchet;
crotchet / 4 => dur quaver;

//Wait for an initial event
min => now;
//wait for a bar
crotchet * 4 => now;


while(true)
{
    MidiMsg msg;
    notes[Math.random2(0,notes.size()-1)] + baseNote => int pitch;
    
    keyDown(pitch,127) @=> msg;
    if(Math.randomf() > 0.5){
        crotchet => now;
    }else{
        quaver => now;
    }
    keyUp(msg);
    Math.random2(0,3) => int wait;
    if(wait <= 0){
        crotchet => now;
    }else if(wait <= 1){
        quaver => now;
    }else{
        crotchet + quaver => now;
    }
}
