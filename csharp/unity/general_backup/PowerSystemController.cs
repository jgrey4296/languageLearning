using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Central location to tick power resource levels
 */
public class PowerSystemController : ResourceControllerBase {

	public static PowerSystemController instance = null;

	void Awake(){
		if (PowerSystemController.instance == null) {
			PowerSystemController.instance = this;
		} else if (PowerSystemController.instance != this){
			Destroy(this);
		}
	}



	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
