/**
 * from https://stackoverflow.com/questions/42531691/
 */
using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Net.Sockets;
using System.Threading;



public class NetworkScript : MonoBehaviour
{
	public String host = "localhost";
	public Int32 port = 50000;

	internal Boolean socket_ready = false;
	internal String input_buffer = "";

	TcpClient tcp_socket;
	NetworkStream net_stream;

	StreamWriter socket_writer;
	StreamReader socket_reader;

	private void Start()
	{
		Debug.Log ("Starting server");
		setupSocket();		
		writeSocket ("Handshake");
	}

		
	void Update()
	{
		//string received_data = readSocket();
		string received_data = readSocket();
		Debug.Log ("Received data: " + received_data);
		switch (received_data)
		{
		case "pong":
			Debug.Log("Python controller sent: " + (string)received_data);
			writeSocket("ping");
			break;
		default:
			Debug.Log ("Nothing received");
			writeSocket ("Handshake");
			break;
		}
	}

	void OnApplicationQuit()
	{
		closeSocket();
	}

	// Helper methods for:
	//...setting up the communication
	public void setupSocket()
	{
		try
		{
			tcp_socket = new TcpClient(host, port);
			net_stream = tcp_socket.GetStream();
			socket_writer = new StreamWriter(net_stream);
			socket_reader = new StreamReader(net_stream);
			socket_ready = true;
			socket_writer.AutoFlush = true;
			
		}
		catch (Exception e)
		{
			// Something went wrong
			Debug.Log("Socket error: " + e);
		}
	}

	//... writing to a socket...
	public void writeSocket(string line)
	{
		if (!socket_ready) {
			Debug.Log ("Early Exit in writeSocket: " + line);
			return;
		}

		//line = line + "\r\n";
		Debug.Log("Writing to socket: " + line);
		socket_writer.Write(line);
	}

	//... reading from a socket...
	public String readSocket()
	{
		if (!socket_ready) {
			Debug.Log ("Early Read exit");
			return "";
		}

		Debug.Log (" Data Available: " + net_stream.DataAvailable.ToString ());
		if (net_stream.DataAvailable) {
			string read = socket_reader.ReadLine();
			Debug.Log("Data Read: " + read);
			return read;
		}

		Debug.Log ("Skipped Read");
		return "";
	}

	//... closing a socket...
	public void closeSocket()
	{
		Debug.Log ("Closing Socket");
		if (!socket_ready)
			return;

		socket_writer.Close();
		socket_reader.Close();
		tcp_socket.Close();
		socket_ready = false;
	}
}
