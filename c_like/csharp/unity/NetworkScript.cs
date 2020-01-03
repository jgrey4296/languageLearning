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
using UnityEngine.UI;

//Network Header Format
[Serializable]
class VaultData {
	//Client -> Server : HandShake, Info, AI_GO, QUIT, PAYLOAD, RESEND
	//Server -> Client : HandShake, Action, AI_COMPLETE, PAYLOAD, RESEND
	public enum NetworkMessageT { HANDSHAKE, INFO, ACTION, AI_GO, AI_COMPLETE, QUIT, PAYLOAD, RESEND};

	public int size;
	public NetworkMessageT iden;
	public string data;
	public string hash;

	public VaultData(NetworkMessageT t, int s){
		this.iden = t;
		this.size = s;
	}

	public VaultData(NetworkMessageT t, int s, string h){
		this.iden = t;
		this.size = s;
		this.hash = h;
	}

	public VaultData(NetworkMessageT t, string d){
		this.iden = t;
		this.size = d.Length;
		this.data = d;
		this.hash = NetworkScript.CalculateHash (d);
	}
}

//TODO: Add in an action definition class

public class NetworkScript : MonoBehaviour
{
	protected static NetworkScript instance;
	public static NetworkScript GetInstance(){
		return instance;
	}

	//UI Display of connection status
	public Text text;

	//Target:
	public String host = "localhost";
	public Int32 port = 50000;
	public const int blockSize = 1024;
	public const int headerSize = 128;

	//Internal Management
	public static List<String> emptyStrArr = new List<String>();
	Boolean connected = false;
	internal Boolean socket_ready = false;
	internal String input_buffer = "";
	bool payload_flag = false;

	//Dictionary of All Objects that have registered their GUID
	//For lookup. Used to send initial world information, and
	//recieve actions
	public Dictionary<String, NetworkObject> registered;
	//Stores of HASH -> Message for resending upon corruption
	public Dictionary<String, String> recentMessages;


	//Actual Network connections
	TcpClient tcp_socket;
	NetworkStream net_stream;
	StreamWriter socket_writer;
	StreamReader socket_reader;

	//Data Objects for sending and receiving
	Queue<VaultData> toServerQueue = new Queue<VaultData>();
	Queue<String> fromServerQueue = new Queue<String> ();
	//Raw text data to be sent
    string dataAsJson = "";
	List<String> receivedDataArray;
    
	//MD5 Hasher
	private static MD5 md5 = MD5.Create();

	//Setup the singleton
	public void Awake(){
		if (NetworkScript.instance == null) {
			NetworkScript.instance = this;
		} else if (NetworkScript.instance != this){
			DestroyObject (this);
		}
			
		this.registered = new Dictionary<String, NetworkObject> ();
		this.recentMessages = new Dictionary<String, String> ();

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

	//Ensure Singleton status
	private void Start() {
		if (NetworkScript.instance == null) {
			NetworkScript.instance = this;
		} else if (NetworkScript.instance != this){
			DestroyObject (this);
		}

	}

	//Create the header and payload for a string
	public void createMessage(string data){
		string hash = NetworkScript.CalculateHash (data);
		VaultData payload = new VaultData (VaultData.NetworkMessageT.PAYLOAD, data);
		VaultData header = new VaultData (VaultData.NetworkMessageT.INFO, data.Length, hash);
		toServerQueue.Enqueue (header);
		toServerQueue.Enqueue (payload);
		this.recentMessages.Add (hash, data);
	}

	/*
	 * Send all messages in the queue
	 */
	public void flushQueue() {
		if (!socket_ready) {
			Debug.Log ("Early Exit in writeSocket");
			return;
		}
			
		//For each message in the queue, convert to string and send it
		foreach ( var datum in this.toServerQueue){
			if (!net_stream.CanWrite) {
				Debug.Log ("Can't Write");
				return;
			}

			string message;
			if (datum.iden == VaultData.NetworkMessageT.PAYLOAD) {
				message = datum.data;
			} else {
				message = JsonUtility.ToJson (datum);
				//PAD IF TOO SMALL
				if (message.Length < NetworkScript.headerSize) {
					Debug.Log ("PADDING MESSAGE");
					var amnt = (NetworkScript.headerSize - message.Length);
					var pad = new String ('!', (amnt > 0 ? amnt : 0));
					message += pad;
				} else if (message.Length > NetworkScript.headerSize) {
					throw new Exception ("Header is too big");
				}
			}
			Debug.Log ("Sending: " + message);
			this.send_data_segments (message);
			Debug.Log ("Sent message");
		}
		socket_writer.Flush ();
		this.toServerQueue.Clear ();
	}
		
	/*
	 * Given a string thats larger than the blocksize,
	 * split it, send a size header, then send the data
	 */
	void send_data_segments(string data){
		socket_writer.Write (data);
	}

	/*
	 * The update tick where data is listened for
	 */
	void Update() {
		if (!connected) {
			return;
		}

		while (net_stream.DataAvailable) {
			Debug.Log ("Data Available");
			String receivedData = readSocket ();
			if (receivedData == "") {
				return;
			}
			var dataList = receivedData.Split ('|');
			foreach (var s in dataList) {
				this.ProcessReceivedData (s);
			}
		}
		//Flush the output message queue upon completion of listening
		this.flushQueue ();
	}

	/**
	 * Accept a received string of information from the server,
	 * and do something with it
	 */ 
	void ProcessReceivedData(string receivedJson){
		if (receivedJson == "") {
			return;
		}

		Debug.Log ("Received data: " + receivedJson);
		//Convert json into object
		VaultData fromNetworkData = JsonUtility.FromJson<VaultData>(receivedJson);
        
		/*
		 * Handle the data:     
		 */
		switch (fromNetworkData.iden) {
		//INIITIAL SETUP HANDSHAKE:
		case VaultData.NetworkMessageT.HANDSHAKE:
			Debug.Log ("Network Handshake Complete");
			//Update the connection UI:
			text.text = "Connected";
			//Send some information
			foreach (var obj in this.registered.Values) {
				this.createMessage (obj.GetComponent<ActorAIStub> ().toPythonString ());
			}
			break;
		//INSTRUCTION TO CHANGE THE GAME WORLD:
		case VaultData.NetworkMessageT.ACTION:
			Debug.Log ("Got an Action Header");
			Debug.Log (String.Format ("Payload: {0}", fromNetworkData.data));
			Debug.Assert (fromNetworkData.hash == NetworkScript.CalculateHash (fromNetworkData.data));
			this.fromServerQueue.Enqueue (fromNetworkData.data);
			break;
		//NOTIFICATION OF AI CYCLE / TURN COMPLETION:
		case VaultData.NetworkMessageT.AI_COMPLETE:
			Debug.Log ("AI Finished, time to trigger actions");
			break;
		//INSTRUCTION TO RESEND CORRUPTED DATA
		case VaultData.NetworkMessageT.RESEND:
			Debug.Log ("Hash Mismatch, Resend Data: " + fromNetworkData.hash);
			break;
		//MESSAGES THAT SHOULD ONLY BE SENT, NOT RECEIVED:
		case VaultData.NetworkMessageT.INFO:
		case VaultData.NetworkMessageT.AI_GO:
			throw new Exception ("Data or command passed in the wrong direction");
			break;
		//UPON QUIT OF GAME, SIGNAL SHUTDOWN OF SERVER
		case VaultData.NetworkMessageT.QUIT:
			text.text = "Disconnected";
			closeSocket ();
			break;
		default:
			closeSocket ();
			break;
		}
	}

	/*
	 * Listen to the socket, and consume information from it
	 */
	public String readSocket(int amnt = NetworkScript.headerSize, string expectedHash = null) {
		if (!socket_ready) { Debug.Log ("Socket not ready"); return ""; }
		if (!net_stream.CanRead) { Debug.Log ("Stream unable to read");	return ""; }
		if (!net_stream.DataAvailable) { return ""; }
		return socket_reader.ReadLine ();
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
		if (!socket_ready) { return; }
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

	/**
	 * Utility to get the md5 (currently) hash of a string for error checking
	 */
	public static string CalculateHash(string input){
		//From https://blogs.msdn.microsoft.com/csharpfaq/2006/10/09/how-do-i-calculate-a-md5-hash-from-a-string/
		byte[] inputBytes = System.Text.Encoding.ASCII.GetBytes (input);
		byte[] hash = NetworkScript.md5.ComputeHash (inputBytes);
		StringBuilder sb = new StringBuilder ();
		for (int i = 0; i < hash.Length; i++) {
			sb.Append (hash [i].ToString ("X2"));
		}
		return sb.ToString ();
	}

	/**
	 * Register an object with its GUID,
	 * so that actions can lookup their targets
	 */
	public void register(NetworkObject obj){
		Debug.Assert (obj != null);
		this.registered.Add (obj.GetInstanceID ().ToString(), obj);
	}


	/**
	 * Actions are stored in a queue during listening,
	 * this method then fires those actions
	 */
	public void TriggerActions(){
		foreach (var action in this.fromServerQueue) {
			Debug.Log (action);
		}
	}

}
