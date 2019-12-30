using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/*
 * ItemInfo: Information held to match item stubs with actual instantations.
 */
public class ItemInfo : MonoBehaviour {
	public enum ItemSize { GroundSmall, GroundMed, GroundLarge,
		WallSmall, WallMed, WallLarge,
		CeilSmall, CeilMed, CeilLarge }	

	public ItemSize itemSize;

	//TODO: add EL String representations of items


	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
