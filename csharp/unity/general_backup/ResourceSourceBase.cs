using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * A Generic Source of a resource
 */
public class ResourceSourceBase : MonoBehaviour {

	public ResourceControllerBase controller;
	public int sourceAmnt;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

	void step(){
		controller.increment (sourceAmnt);
	}
}
