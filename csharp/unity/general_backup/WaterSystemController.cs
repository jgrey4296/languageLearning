using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * The main controller for water resource
 */
public class WaterSystemController : ResourceControllerBase {

	public static WaterSystemController instance = null;

	void Awake(){
		if (WaterSystemController.instance == null) {
			WaterSystemController.instance = this;
		} else if (WaterSystemController.instance != this){
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
