using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class raycastTest : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		LayerMask l = 1 << 11;
		l += 1 << 12;
		var results = Physics2D.RaycastAll (transform.position, new Vector2 (1, 0), 
			Mathf.Infinity, l);
		foreach (var result in results) {
			if (result.collider != null) {
				var rndVec = Random.ColorHSV ();
				result.collider.transform.GetComponent<colourChanger> ().change (rndVec);
			}
		}
	}
}
