using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/*
 * GlobalItemList: A Singleton that everything accesses to build and use items 
 */
public class GlobalItemList : MonoBehaviour {
	//TODO: send information from here over the network
	public static GlobalItemList instance;

	void Awake(){
		if (GlobalItemList.instance == null) {
			GlobalItemList.instance = this;
		} else if (GlobalItemList.instance != this) {
			Destroy (this);
		}
	}

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
