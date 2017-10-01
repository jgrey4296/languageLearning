using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

//Always sent and recieved first, indicate the specific type of netdata to follow,
//and the number of blocks the data will require to be read;
[Serializable]
public class NetHeader {
	string type;
	int numOfBlocks;

	public NetHeader(string t, int s){
		type = t;
		numOfBlocks = s;
	}
}

//The Superclass of net messages
//Always sent second
[Serializable]
public class NetData {
}

//An example NetData subclass. holds an arbitrary string
[Serializable]
public class NetString : NetData {
	string data;

	public NetString( string d ){
		data = d;
	}
}