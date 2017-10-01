using System.Collections;
using System.Collections.Generic;
using UnityEngine;


/*
 * FloorController: Builds a row of rooms, including elevators,
 * and ensures the leftmost and rightmost rooms have complete walls.
 */
public class FloorController : MonoBehaviour {
	//TODO: pathfind between rooms
	public float roomWidth;
	public int floorIndex { get; private set; }
	public GameObject[] RoomTemplates;
	public GameObject ElevatorTemplate;
	//The instantiated rooms
	List<GameObject> rooms = new List<GameObject>();
	//the indices of elevators on the floor.
	List<int> elevatorIndices = new List<int>();

	public List<GameObject> getRooms(){
		return rooms;
	}
		

	// Use this for initialization
	public void BuildFloor (int fI, int numberOfRooms) {
		floorIndex = fI;
		//Create a row of rooms
		for (int i = 0; i < numberOfRooms; ++i) {
			var selectedRoomTemplate = RoomTemplates [Random.Range (0, RoomTemplates.Length)];
			var pos = new Vector3 (i * roomWidth, 0f, 0f);
			GameObject newInstantiation = Instantiate (selectedRoomTemplate) as GameObject;
			newInstantiation.transform.parent = this.transform;
			newInstantiation.transform.localPosition = pos;
			newInstantiation.GetComponent<RoomController> ().index = i;
			//if the first or last room, enable the doors
			if (i == 0) {
				newInstantiation.gameObject.GetComponent<RoomController> ().toggleDoorL ();
			} else if (i == numberOfRooms - 1) {
				newInstantiation.gameObject.GetComponent<RoomController> ().toggleDoorR ();
			}
			//Store the room
			rooms.Add (newInstantiation);
		}

	}

	public void ReplaceRoomIndexWithElevator(int i){
		if (i < 0 || i >= rooms.Count) {
			throw new KeyNotFoundException ();
		}
		var oldRoom = rooms [i];
		var newElevator = Instantiate (ElevatorTemplate) as GameObject;
		newElevator.transform.parent = this.transform;
		newElevator.transform.localPosition = oldRoom.transform.localPosition;
		rooms [i] = newElevator;
		elevatorIndices.Add (i);
		newElevator.GetComponent<RoomController> ().index = i;
		Destroy (oldRoom);

		if (i == 0) {
			newElevator.GetComponent<RoomController> ().toggleDoorL ();
		} else if (i == rooms.Count - 1) {
			newElevator.GetComponent<RoomController> ().toggleDoorR ();
		}

	}

	public GameObject GetRoom(int roomIndex){
		if (roomIndex < 0 || roomIndex >= this.rooms.Count) {
			throw new UnityException ("Attempting to get a room outside of bounds");
		}
		return this.rooms [roomIndex];
	}

	void addRoom(bool rightEdge, GameObject roomType){
		//TODO: instantiate the roomtype
		//then offset the instance
		//rooms.Insert (index, roomInstance);
	}

	void replaceRoom(int index, GameObject roomType){
		//TODO: instantiate the roomtype. SEE ELEVATOR CREATION
		//copy the transform from the room to be replaced
		//rooms[i] = roomInstance;
		//Destroy(oldRoom);
	}


	// Update is called once per frame
	void Update () {
		
	}
}
