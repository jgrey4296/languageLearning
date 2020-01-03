<<< "Shred id:" + me.id() >>>;
//<<< me.args() >>>;
//<<< me.arg(0) => Std.atoi >>>;
me.arg(0) => Std.atoi => int type;
me.arg(1) => Std.atoi => int usePattern;

//Midi setup
MidiIn min;
if(! min.open( 1 ) ) me.exit();
MidiOut mout;
if(! mout.open(0) ) me.exit();

0::second => dur n;

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

fun void playNote(int note, int vel, dur play, dur wait){
    MidiMsg msg;
    if(play == n){
        keyDown(note,0) @=> msg;
    }else{
        keyDown(note,vel) @=> msg;
    }
    play => now;
    keyUp(msg);
    2*wait => now;
}

//Potential Notes:
36 => int baseNote;
12 => int octave;
//Kicks:
[ [0,1],
//snares:
[2,4],
//claps:
[3],
//perc:
[5,6],
//closedHH
[8,10],
//other
[7,9,11]
] @=> int notes[][];

//BPM setup:
120 => int bpm;
60::second / bpm => dur crotchet;
crotchet / 4 => dur quaver;

//Wait for an initial event
//min => now;
//wait for a bar
//crotchet * 4 => now;

//utility:
crotchet => dur c;
quaver => dur q;


//Patterns, as arrays of pairs for play and rest:
[
[[c,n],[n,n]],
[[q,n],[q,n]],
[[c,n],[q,n],[c,n]],
[[c,n],[c,n]]
//[[c,n],[c,c],[q,n],[q,n],[c,q]]
] @=> dur patterns[][][];

0 => int patternPosition;
patterns[usePattern] @=> dur pattern[][];

0 => int patternCount;

while(true)
{
    pattern.size() => int patternSize;
    
    //The midi message to use
    MidiMsg msg;
    //get a pitch
    notes[type] @=> int usableNotes[];
    usableNotes[Math.random2(0,usableNotes.size()-1)] + baseNote => int pitch;

    //select and play pattern:
    pattern[patternPosition][0] => dur playLength;
    pattern[patternPosition][1] => dur waitLength;
    //play the actual note
    playNote(pitch,90,playLength,waitLength);

    //increment position
    (patternPosition + 1) % (patternSize)  => patternPosition;
    //change patterns:
    if(patternPosition == 0){
        patternCount + 1 => patternCount;
    }
    if(patternCount > Math.random2(2,5) && patternPosition == 0){
        Math.random2(0,patterns.size()-1) => usePattern;
        patterns[Math.random2(0,patterns.size()-1)] @=> pattern;
        0 => patternCount;
    }
}
