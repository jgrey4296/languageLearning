using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Central place to tick food resource amounts
 */
public class FoodSystemController : ResourceControllerBase {

	public static FoodSystemController instance = null;

	void Awake(){
		if (FoodSystemController.instance == null) {
			FoodSystemController.instance = this;
		} else if (FoodSystemController.instance != this){
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
