using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CreateScene : MonoBehaviour {
	public int numOfObjects = 10;
	public float xBound = 10;
	public float zBound = 10;
	public GameObject[] objects;

	void Awake () {
		for (var i = 0; i < numOfObjects; i++) {
			GameObject toInstantiate = objects [Random.Range (0, objects.Length)];
			Vector3 pos = new Vector3 (Random.Range (-xBound, xBound), Random.Range(0, 10), Random.Range (-zBound, zBound));

			GameObject instanced = Instantiate (toInstantiate, pos, Quaternion.identity) as GameObject;
		}
	}


	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
