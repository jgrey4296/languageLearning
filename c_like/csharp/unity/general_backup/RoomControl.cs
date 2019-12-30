using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public enum Direction { UP, DOWN, LEFT, RIGHT };

public class RoomControl : MonoBehaviour {

	//leftDoor.activated == closed
	public GameObject leftDoor = null;
	public GameObject rightDoor = null;
	public GameObject upDoor = null;
	public GameObject downDoor = null;

	//The Rooms next door:
	public GameObject leftRoom = null;
	public GameObject rightRoom = null;
	public GameObject upRoom = null;
	public GameObject downRoom = null;

	void BuildWalls(){
		//check which directions have rooms
			//create full walls on directions with no room
			//create doorWalls on directions with a room
			
	}


	public void AddRoom (Direction d, GameObject room) {
		//Check here that its a room
		switch (d) {
		case Direction.UP:
			//Take the gameobject, place it the right scale units over
			upRoom = room;
			break;
		case Direction.DOWN:
			downRoom = room;
			break;
		case Direction.LEFT:
			leftRoom = room;
			this.leftDoor.SetActive (false);
			break;
		case Direction.RIGHT:
			rightRoom = room;
			this.rightDoor.SetActive (false);
			break;
		default:
			Debug.Log ("Unknown direction");
			break;
		}

	}


	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
