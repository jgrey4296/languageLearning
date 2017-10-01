using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

/**
 * Central Location to deal with mouse control.
 */
public class MouseControl : MonoBehaviour {

	public Text text;
	public GameObject actor;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		if (Input.GetButtonDown("Fire1")){
			Debug.Log ("button1 clicked");
			Vector3 mousePos = Input.mousePosition;
			Vector3 inputPosition = Input.mousePosition; 
			Vector3 transformed = new Vector3 (inputPosition.x, inputPosition.y, Camera.main.transform.position.z - 2f);
			Vector3 ray = Camera.main.ScreenToWorldPoint (transformed);
			text.text = string.Format ("Mouse Position: {0}", ray);
		}

		if (Input.GetButtonDown ("Fire2")) {
			text.text = "Fired";
			int targetLayer = LayerMask.GetMask ("CombattingActor");
			for (var i = 0; i < actor.transform.childCount; i++){
				actor.transform.GetChild(i).gameObject.layer = targetLayer;
			}
			actor.layer = targetLayer;
		
		}

	}
}
