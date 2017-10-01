using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VaultGen : MonoBehaviour {
	//Defines the number of rooms
	public int xSize = 4;
	public int ySize = 4;
	//The rooms that can be instantiated.
	public GameObject[] roomTypePool;
	//The elevator prefab
	public GameObject elevator;
	//The actual instanced rooms, a 1d array of 2d locations.
	private List<GameObject> instancedRooms = new List<GameObject>();
	
	private Dictionary<Direction, int[]> n_coords = new Dictionary<Direction, int[]> {
		{ Direction.LEFT, new int[] { -1, 0 } },
		{ Direction.RIGHT, new int[] { 1, 0 } },
		{ Direction.UP , new int[] { 0, 1 } },
		{ Direction.DOWN, new int[] { 0, -1 } }
	};

	// Use this for initialization
	void Awake () {
		Debug.Log ("Awaking to create the rooms");

		int elevator_x = Random.Range (1, ySize);
		//Create the rooms floor by floor
		for (int i = 0; i < ySize; i++) {
			Debug.Log ("Setting up Floor: " + i);
			//Setup y offset
			float yTransform = i * 10;
			//Row setup
			for (int j = 0; j < xSize; j++) {
				Debug.Log ("Setting up room: " + i + ", " + j);
				GameObject newRoom;
				if (j == elevator_x) {
					newRoom = this.elevator;
				} else {
					newRoom = roomTypePool [Random.Range (0, roomTypePool.Length)];
				}

				float xTransform = j * 20;

				var newRoomInstance = Instantiate (newRoom, new Vector3 (xTransform, yTransform, 0.0f), Quaternion.identity) as GameObject;
				instancedRooms.Add (newRoomInstance);
				newRoomInstance.transform.SetParent (this.transform);
								
			}
		}

		//Link the elevators together
		for (int i = 0; i < ySize; i++) {
			for (int j = 0; j < xSize; j++) {
				var current = instancedRooms [i * ySize + j];
				var neighbours = this.getNeighbours (j, i);
				foreach (var neighbour in neighbours) {
					current.GetComponent<RoomControl> ().AddRoom (neighbour.Key, neighbour.Value);
				}
			}
		}
	}
	
	// Update is called once per frame
	void Update () {
		
	}
		
	Dictionary<Direction, GameObject> getNeighbours(int x, int y){
		Dictionary<Direction, GameObject> neighbours = new Dictionary<Direction, GameObject>();
		foreach (var pair in n_coords) {
			int xp = x + pair.Value [0];
			int yp = y + pair.Value [1];
			int[] coords = { xp, yp };
			int flatCoord = coords [1] * xSize + coords [0];
			if (0 <= flatCoord && flatCoord < instancedRooms.Count){
				neighbours.Add(pair.Key, instancedRooms[flatCoord]);
			} 			
		}
		return neighbours;
	}

}


