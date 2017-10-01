using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Keeps track of the room an agent is in, by caching collisions
 */
public class RoomDetector : MonoBehaviour {

	public GameObject currentRoom { get; private set; }

	void OnTriggerEnter2D(Collider2D collider){
		if (collider.gameObject.tag == "Room") {
			currentRoom = collider.gameObject;
			var index = currentRoom.GetComponent<RoomController> ().index;
			Debug.Log ("Actor in: " + currentRoom.name + " " + index);
		}
	}
	
}
