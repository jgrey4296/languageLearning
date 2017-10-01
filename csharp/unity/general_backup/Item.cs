using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Item : MonoBehaviour {
	//An object on the map

	//Todo: make costs enums
	public float movementCost = Mathf.Infinity;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

	public float GetCost(){
		return movementCost;
	}

}
