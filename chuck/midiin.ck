MidiIn min;

if ( !min.open(1) ) me.exit();

while(true){
    min => now;

    <<< "Msg received", min>>>;

}