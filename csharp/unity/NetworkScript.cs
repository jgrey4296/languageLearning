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
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;

//Network Header Format
//TODO: Determine final format
[Serializable]
class VaultData {
	public enum NetworkMessageT { HANDSHAKE, INFO, ACTION, AI_GO, AI_COMPLETE, QUIT, PAYLOAD};

	public int size;
	public NetworkMessageT iden;
	public string data;
	public string hash;

	public VaultData(NetworkMessageT t, int s){
		this.iden = t;
		this.size = s;
	}

	public VaultData(NetworkMessageT t, int s, string h){
		this.size = s;
		this.iden = t;
		this.hash = h;
	}

	public VaultData(NetworkMessageT t, string d){
		this.iden = t;
		this.data = d;
	}
}

public class NetworkScript : MonoBehaviour
{
	//Target:
	public String host = "localhost";
	public Int32 port = 50000;
	public int blockSize = 1024;
	public int headerSize = 64;

	//Internal Management
	Boolean connected = false;
	internal Boolean socket_ready = false;
	internal String input_buffer = "";

	//Actual Network connections
	TcpClient tcp_socket;
	NetworkStream net_stream;
	StreamWriter socket_writer;
	StreamReader socket_reader;

	//Data Objects for sending and receiving
	Queue<VaultData> toServerQueue = new Queue<VaultData>();
	Queue<VaultData> fromServerQueue = new Queue<VaultData> ();
	//Raw text data to be sent
    string dataAsJson = "";
    string receivedJson = "";
    
	//MD5 Hasher
	MD5 md5 = MD5.Create();

	/*
	 * Upon Creation of the network script,
	 * setup the network.
	 */
	private void Start() {
		Debug.Log ("Starting server");
		Debug.Log ("Enum Values: " + (int) VaultData.NetworkMessageT.HANDSHAKE);
        try	{
			tcp_socket = new TcpClient(host, port);
			net_stream = tcp_socket.GetStream();
			socket_writer = new StreamWriter(net_stream);
			socket_reader = new StreamReader(net_stream);
			socket_ready = true;
			socket_writer.AutoFlush = true;
			connected = true;
            //SETUP HANDSHAKE:
			toServerQueue.Enqueue(new VaultData(VaultData.NetworkMessageT.HANDSHAKE, 0));
			flushQueue ();
		}
		catch (Exception e)	{
			// Something went wrong
			Debug.Log("Socket error: " + e);
		} 

	}

	public void createMessage(string data){
		string hash = CalculateHash (data);
		VaultData payload = new VaultData (VaultData.NetworkMessageT.PAYLOAD, data);
		VaultData header = new VaultData (VaultData.NetworkMessageT.INFO, data.Length, hash);
		toServerQueue.Enqueue (header);
		toServerQueue.Enqueue (payload);
	}

	/*
	 * Send all messages in the queue
	 */
	public void flushQueue() {
		if (!socket_ready) {
			Debug.Log ("Early Exit in writeSocket");
			return;
		}
		if (!net_stream.CanWrite) {
			Debug.Log ("Can't Write");
			return;
		}

		//For each message in the queue, convert to string and send it
		foreach ( var datum in this.toServerQueue){
			string message;
			if (datum.iden == VaultData.NetworkMessageT.PAYLOAD) {
				message = datum.data;
			} else {
				message = JsonUtility.ToJson (datum);
				//PAD IF TOO SMALL
				if (message.Length < this.headerSize) {
					Debug.Log ("PADDING MESSAGE");
					var amnt = (this.headerSize - message.Length);
					var pad = new String ('!', (amnt > 0 ? amnt : 0));
					message += pad;
				} else if (message.Length > this.headerSize) {
					throw new Exception ("Header is too big");
				}
			}
			Debug.Log ("Sending: " + message);
			this.send_data_segments (message);
		}

		socket_writer.Flush ();
		this.toServerQueue.Clear ();
	}
		
	/*
	 * Given a string thats larger than the blocksize,
	 * split it, send a size header, then send the data
	 */
	void send_data_segments(string data){
		//todo: store payloads in a dict by their hash,
		//to resend if there is corruption
		socket_writer.Write (data);
	}

	/*
	 * The update tick where data is listened for
	 */
	void Update() {
		if (!connected) {
			return;
		}

		receivedJson = readSocket();
		if (receivedJson == "") {
			return;
		}

		Debug.Log ("Received data: " + receivedJson);
		//Convert json into object
		VaultData fromNetworkData = JsonUtility.FromJson<VaultData>(receivedJson);
        //Handle the data:        
		switch (fromNetworkData.iden) {
		case VaultData.NetworkMessageT.HANDSHAKE:
			Debug.Log ("Network Handshake Complete");
			this.createMessage ("some information");
			//TODO: Send setup information here
			this.flushQueue ();
			break;
		case VaultData.NetworkMessageT.ACTION:
			Debug.Log ("Received Action");
			Debug.Log (fromNetworkData);
			this.fromServerQueue.Enqueue (fromNetworkData);
			break;
		case VaultData.NetworkMessageT.AI_COMPLETE:
			Debug.Log ("AI Finished, time to trigger actions");
			break;
		case VaultData.NetworkMessageT.INFO:
		case VaultData.NetworkMessageT.AI_GO:
			throw new Exception ("Data or command passed in the wrong direction");
			break;
		case VaultData.NetworkMessageT.QUIT:
		default:
			closeSocket ();
			break;
		}
	}

	/*
	 * Listen to the socket, and consume information from it
	 */
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

	/*
	 * Cleanup the socket on exit
	 */
	void OnApplicationQuit() {
		closeSocket();
	}

	/*
	 * Routine to close the socket, 
	 * sending a closing handshake.
	 */
	public void closeSocket() {
		Debug.Log ("Closing Socket");
		if (!socket_ready)
			return;
        //Send a final message before closing the socket
		toServerQueue.Clear();
		toServerQueue.Enqueue (new VaultData (VaultData.NetworkMessageT.QUIT, 0));
		flushQueue ();
		socket_writer.Close();
		socket_reader.Close();
		tcp_socket.Close();
		socket_ready = false;
		connected = false;
	}

	private string CalculateHash(string input){
		//From https://blogs.msdn.microsoft.com/csharpfaq/2006/10/09/how-do-i-calculate-a-md5-hash-from-a-string/
		byte[] inputBytes = System.Text.Encoding.ASCII.GetBytes (input);
		byte[] hash = md5.ComputeHash (inputBytes);
		StringBuilder sb = new StringBuilder ();
		for (int i = 0; i < hash.Length; i++) {
			sb.Append (hash [i].ToString ("X2"));
		}
		return sb.ToString ();
	}

}
