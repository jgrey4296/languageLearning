using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Tile : MonoBehaviour {
	//A Ground tile of the map
	public float movementCost = 1;

	public List<Item> items = new List<Item>();


	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

	public float GetCost(){
		float currentCost = movementCost;
		foreach (var item in items){
			currentCost += item.GetCost();
		}
		if (currentCost >= Mathf.Infinity) {
			transform.Find ("Sprite").GetComponent<SpriteRenderer> ().color = Color.red;
		}
		return currentCost;
	}

	public string ToString(){
		return string.Format ("Tile({0},{1})", transform.localPosition.x, transform.localPosition.y);
	}
		
}
