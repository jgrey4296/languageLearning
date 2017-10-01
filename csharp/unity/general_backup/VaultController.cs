using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(FoodSystemController))]
[RequireComponent(typeof(PowerSystemController))]
[RequireComponent(typeof(WaterSystemController))]
/*
 * VaultController : The top level singleton class for the
 * simulation. Creates each individual floor.
 * Holds all individual rooms as well.
 * Serves as the central point for control of subsystems.
 */
public class VaultController : MonoBehaviour {
	//TODO: add pathfinding between floors
	//Singleton variable:
	public static VaultController instance = null;

	public int numberOfFloors;
	public int numberOfRoomsPerFloor;
	public float floorHeight;
	public GameObject FloorController;
	public int numberOfElevators;

	List<GameObject> floors = new List<GameObject> ();
	List<GameObject> allRooms = new List<GameObject>();

	void Awake() {
		if (VaultController.instance == null) {
			instance = this;
		} else if (VaultController.instance != this) {
			Destroy (this.gameObject);
		}
	}


	//Setup the Vault by building each floor, which builds rooms.
	void Start () {
		//Create each floor, one above the other
		for (int i = 0; i < numberOfFloors; i++) {
			var floorPosition = new Vector3 (0f, i * floorHeight, 0f);
			var newFloor = Instantiate (FloorController) as GameObject;
			newFloor.transform.localPosition = floorPosition;
			floors.Add (newFloor);
			//Trigger the floor building
			newFloor.GetComponent<FloorController>().BuildFloor(i, numberOfRoomsPerFloor);
		}
		//Trigger Elevator construction
		int elevatorIndex = Random.Range(0, numberOfRoomsPerFloor);
		foreach (var floor in floors) {
			floor.GetComponent<FloorController> ().ReplaceRoomIndexWithElevator (elevatorIndex);
		}


		//Collect all the rooms together
		foreach (var floor in floors) {
			allRooms.AddRange (floor.GetComponent<FloorController> ().getRooms ());
		}
		verifyRoomList ();
	}

	void tickResourceSystem () {
		//Foreach room in all rooms, tick the sources and sinks of resources.
	}

	// Update is called once per frame
	void Update () {
		
	}

	void verifyRoomList(){
		foreach (var room in allRooms) {
			if (room.tag != "Room"){
				throw new UnityException("A Room is not tagged as a room");
			}
		}
	}

	public GameObject getRoomOnFloor(int roomIndex, int floorIndex){
		var floor = floors [floorIndex];
		var room = floor.GetComponent<FloorController> ().GetRoom (roomIndex);
		return room;
	}

	public void addFloor(bool bottomEdge, int elevatorIndex){
		//TODO: check the elevator lines up
		//then add a new floor, setting transform correctly,
		//and instiate the elevator.
	}

}
