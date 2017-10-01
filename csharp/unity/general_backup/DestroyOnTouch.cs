using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DestroyOnTouch : MonoBehaviour {
	/*
	 * Place on a collider below the street, catches and destroys 
	 * 	(later moves to a pool) the objects/ npcs that have fallen off the street
	 */
	void OnCollisionEnter2D (Collision2D col){
		//Alt: add to global object pool for reuse
		Destroy (col.gameObject);
	}
}
