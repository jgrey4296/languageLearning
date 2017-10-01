using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class autorotate : MonoBehaviour {
	public Vector3 rot;

	// Use this for initialization
	void Start () {
		
	}

	// Update is called once per frame
	void Update () {
		this.transform.Rotate (rot);

	}


}
