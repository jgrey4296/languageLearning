/**
 * from https://stackoverflow.com/questions/42531691/
 */
using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Net.Sockets;
using System.Threading;
using UnityEngine.Serialization;
using System.Text;

//Data Class used to serialze to json
[Serializable]
public class MyData {

	public int age;
	public string name;

}


public class NetworkScript : MonoBehaviour
{
	public String host = "localhost";
	public Int32 port = 50000;

	Boolean connected = false;
	internal Boolean socket_ready = false;
	internal String input_buffer = "";

	TcpClient tcp_socket;
	NetworkStream net_stream;

	StreamWriter socket_writer;
	StreamReader socket_reader;

	//Data Object for sending and receiving
	MyData theData = new MyData();
	MyData returnData = new MyData();
    string dataAsJson = "";
    string receivedData = "";
    
	private void Start() {
		Debug.Log ("Starting server");
        try	{
			tcp_socket = new TcpClient(host, port);
			net_stream = tcp_socket.GetStream();
			socket_writer = new StreamWriter(net_stream);
			socket_reader = new StreamReader(net_stream);
			socket_ready = true;
			socket_writer.AutoFlush = true;
			connected = true;
            //Send an initial bit of data:
			theData.age = 10;
			theData.name = "Handshake";
			writeSocket (theData);
		}
		catch (Exception e)	{
			// Something went wrong
			Debug.Log("Socket error: " + e);
		} 

	}

	//... writing to a socket...
	public void writeSocket(MyData d) {
		if (!socket_ready) {
			Debug.Log ("Early Exit in writeSocket");
			return;
		}
		if (!net_stream.CanWrite) {
			Debug.Log ("Can't Write");
			return;
		}
        dataAsJson = JsonUtility.ToJson (d);		
		socket_writer.Write(dataAsJson);
	}
		
	void Update() {
		if (!connected) {
			return;
		}

		receivedData = readSocket();
		if (receivedData == "") {
			return;
		}

		Debug.Log ("Received data: " + receivedData);
		JsonUtility.FromJsonOverwrite (receivedData, returnData);
        //Handle the data:        
		switch (returnData.name) {
		case "blah":
			theData.name = "ping";
			writeSocket(theData);
			break;

		case "bloo":
			theData.name = "end";
			writeSocket (theData);
			break;

		default:
			closeSocket ();
			break;
		}
	}

	//... reading from a socket...
	public String readSocket() {
		if (!socket_ready) {
			return "";
		}

		if (!net_stream.CanRead) {
			return "";
		}
			
		if (!net_stream.DataAvailable) {
            return "";
        }
        string read = socket_reader.ReadLine ();
        Debug.Log ("Data Read: " + read);
        return read;
	}

	void OnApplicationQuit() {
		closeSocket();
	}

	//... closing a socket...
	public void closeSocket() {
		Debug.Log ("Closing Socket");
		if (!socket_ready)
			return;
        //Send a final message before closing the socket
		theData.name = "end";
		writeSocket (theData);
		socket_writer.Close();
		socket_reader.Close();
		tcp_socket.Close();
		socket_ready = false;
		connected = false;
	}
}
