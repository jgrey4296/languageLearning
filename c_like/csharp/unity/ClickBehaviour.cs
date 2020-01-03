using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ClickBehaviour : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}

	// Update is called once per frame
	void Update () {
		if (Input.GetMouseButton (0)) {
			Vector3 v = Input.mousePosition;
			Vector3 wv = Camera.main.ScreenToWorldPoint (v);
			Vector3 localPos = transform.InverseTransformPoint (wv);
			Debug.Log (string.Format ("Location: {0}", localPos));
			Tile t = MapManager.instance.GetTile (localPos);
			if (t.items.Count == 0) {
				Debug.Log ("Creating a new item");
				var newItem = Instantiate (MapManager.instance.defaultClick, t.transform, false);
				Debug.Log (string.Format ("New Item's transform: {0}", Vector3.zero));
				t.items.Add (newItem.GetComponent<Item>());
			}
		}
		if (Input.GetMouseButtonDown (1)) {
			foreach (var actor in MapManager.instance.actors) {
				actor.notActive = false;
			}
		}
	}
}
