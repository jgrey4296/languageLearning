using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Baseclass of Resource controllers. Food, Water, power all use this
 */
public class ResourceControllerBase : MonoBehaviour {

	public int StartAmnt;

	int CurrentAmnt;

	public void increment (int amnt){
		CurrentAmnt += amnt;
	}

	public void decrement (int amnt){
		CurrentAmnt -= amnt;
		if (CurrentAmnt < 0) {
			CurrentAmnt = 0;
		}
	}

	public bool hasEnough (int minAmnt ){
		if (minAmnt < CurrentAmnt) {
			return true;
		} else {
			return false;
		}
	}


	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
