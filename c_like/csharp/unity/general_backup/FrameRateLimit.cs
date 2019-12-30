using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Simple component to set framerate
 */
public class FrameRateLimit : MonoBehaviour {

	// Use this for initialization
	void Start () {
		QualitySettings.vSyncCount = 0;  // VSync must be disabled
		Application.targetFrameRate = 45;
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
