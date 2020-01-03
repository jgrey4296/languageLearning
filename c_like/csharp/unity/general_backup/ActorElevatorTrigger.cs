using System.Collections;
using System.Collections.Generic;
using UnityEngine;


/*
 * ActorElevatorTrigger : Detect when an actor is in an
 * 	"ElevatorDoor" tagged collider, and call the vault controller's 
 * 	move level method.
 */
public class ActorElevatorTrigger : MonoBehaviour {

	public bool IsInElevator = false;
	private bool moveUp = false;
	private bool moveDown = false;
	ElevatorController elevatorController;

	void Start(){
		elevatorController = VaultController.instance.GetComponent<ElevatorController> ();
	}

	void OnTriggerEnter2D(Collider2D collider){
		if (collider.tag == "ElevatorDoor") {
			Debug.Log ("In Elevator");
			IsInElevator = true;
		} 
	}

	void OnTriggerExit2D(Collider2D collider){
		if (collider.tag == "ElevatorDoor") {
			Debug.Log ("Out of elevator");
			IsInElevator = false;
		}
	}
		
	void Update() {
		// Read the inputs.
		moveUp = Input.GetKeyDown(KeyCode.R);
		moveDown = Input.GetKeyDown (KeyCode.F);

		if (moveUp && IsInElevator) {
			elevatorController.MoveActor (this.transform.parent.gameObject, ElevatorController.ElevatorDirection.Up);
		} else if (moveDown && IsInElevator) {
			elevatorController.MoveActor (this.transform.parent.gameObject, ElevatorController.ElevatorDirection.Down);
		} 
	}

}
