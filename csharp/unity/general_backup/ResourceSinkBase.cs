using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * A Generic sink of a resource
 */
public class ResourceSinkBase : MonoBehaviour {

	public ResourceControllerBase controller;
	public int sinkAmnt;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

	void step(){
		controller.decrement (sinkAmnt);
	}

}
