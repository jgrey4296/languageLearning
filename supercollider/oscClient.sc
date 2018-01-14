thisProcess.openUDPPort(7772);
//Send to a listening server
b = NetAddr("127.0.0.1", 7771);
b.sendMsg("/hello", "there");
b.free;

//Listen to a sending client:
n = NetAddr("127.0.0.1", 7772);
o = OSCFunc({ |msg, time, add, recvPort| msg.postln; }, '/test');
o.free;
n.free;