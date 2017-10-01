using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

/*
 * RoomController : Controls details of a specific room.
 * Mainly able to toggle the existence of left and rightmost walls/doors.
 */
public class RoomController : MonoBehaviour {
	//TODO: add method to broadcast room state over network
	public GameObject RoomSimComponent;
	public int index { get; set; }
	//The child nodes of the room that can be populated as items.
	public List<GameObject> itemSlots;
	

	// Use this for initialization
	void Start () {
		itemSlots = new List<GameObject> ();
		//populate itemslots with the itemstub tagged children
		int max = this.transform.childCount;
		for (int i = 0; i < max; i++) {
			var child = this.transform.GetChild (i);
			//Only itemstubs can be slots
			if (!child.tag.Equals ("ItemStub")) {
				continue;
			}
			itemSlots.Add (child.gameObject);
			Debug.Log ("Item slot count: " + itemSlots.Count);
		}


	}
	
	// Update is called once per frame
	void Update () {
		
	}

	public void addItem(GameObject item){
		// filter itemslots for matching size
		var matchSize = item.GetComponent<ItemInfo>().itemSize;
		var openSlots = itemSlots.FindAll (s => s.GetComponent<ItemInfo> ().itemSize == matchSize 
			&& s.transform.childCount == 1);
		if (openSlots.Count == 0) {
			Debug.Log ("No open slots");
			return;
		}
		var selected = openSlots [Random.Range (0, openSlots.Count)];
		//TODO: instantiate the item into one of the open slots
		//no need to mess with transforms
		selected.GetComponentInChildren<SpriteRenderer>().enabled = false;
		var newObject = Instantiate (item, selected.transform);

	}

	public void RemoveItem(GameObject item){
		for (int i = 0; i < item.transform.childCount; i++) {
			if (item.transform.GetChild (i).GetComponent<ItemInfo> () != null) {
				Destroy (item.transform.GetChild (i).gameObject);
			}
		}
		item.GetComponentInChildren<SpriteRenderer> ().enabled = true;

	}


	public void toggleDoorL(){
		var doorL = this.transform.Find("DoorL");
		bool currentDoorState = doorL.gameObject.activeInHierarchy;
		doorL.gameObject.SetActive(!currentDoorState);
	}

	public void toggleDoorR(){
		var doorR = this.transform.Find("DoorR");
		bool currentDoorState = doorR.gameObject.activeInHierarchy;
		doorR.gameObject.SetActive(!currentDoorState);
	}

	public Transform getElevatorTransformTarget(){
		var door = this.transform.Find ("ElevatorDoor");
		if (door == null) {
			Debug.Log (this.gameObject.name);
			throw new UnityException ("Could not find Elevator door");
		}
		return door;
	}

}
