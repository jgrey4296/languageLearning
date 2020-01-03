using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(VaultController))]
/*
 * ElevatorController : Exists on the VaultController singleton
 * Is called by an elevator room to move an actor up or down a number of floors.
 */
public class ElevatorController : MonoBehaviour {

	public enum ElevatorDirection { Up, Down };

	public void MoveActor (GameObject actor, ElevatorDirection dir){
		//Where the actor is
		var currentRoom = actor.GetComponent<RoomDetector> ().currentRoom;
		int currentRoomIndex = currentRoom.GetComponent<RoomController> ().index;
		int currentFloorIndex = currentRoom.transform.parent.GetComponent<FloorController> ().floorIndex;
		//Where its going
		int nextIndex = currentFloorIndex;
		if (dir == ElevatorDirection.Up) {
			nextIndex += 1;
		} else if (dir == ElevatorDirection.Down) {
			nextIndex -= 1;
		}
		//Making sure its in bounds
		if (nextIndex < 0 || nextIndex >= VaultController.instance.numberOfFloors) {
			return;
		}
		Debug.Log (string.Format("Moving: {0} {1} from {2} to {3} and position {4}", 
			actor.name, dir.ToString(), currentFloorIndex, nextIndex, currentRoomIndex));
		//Then move the actor
		GameObject targetRoom = VaultController.instance.getRoomOnFloor(currentRoomIndex, nextIndex);
		Transform targetDestination = targetRoom.GetComponent<RoomController> ().getElevatorTransformTarget ();
		actor.transform.position = targetDestination.position;
	}
}
